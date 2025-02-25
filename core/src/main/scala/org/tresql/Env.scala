package org.tresql

import sys._
import CoreTypes.RowConverter
import org.tresql.metadata.TypeMapper
import org.tresql.resources.{MacrosLoader, TresqlMacro, TresqlMacros}
import parsing.QueryParsers
import ast.Exp

import java.sql.SQLException
import scala.util.Try

/** Environment for expression building and execution */
private [tresql] class Env(_provider: EnvProvider, resources: Resources, val db: Option[String],
                           val reusableExpr: Boolean)
  extends Resources with AbstractMetadata {

  def this(provider: EnvProvider, db: Option[String], reusableExpr: Boolean) =
    this(provider, null, db, reusableExpr)
  def this(resources: Resources, reusableExpr: Boolean) =
    this(null: EnvProvider, resources, None, reusableExpr)
  def this(params: Map[String, Any], resources: Resources, reusableExpr: Boolean) = {
    this(null: EnvProvider, resources, None, reusableExpr)
    update(params)
  }

  //provided envs are used for statement closing. this list is filled only if provider is not set.
  //NOTE: list contains also this environment
  private var providedEnvs: List[Env] = Nil
  //is package private since is accessed from QueryBuilder
  private[tresql] val provider: Option[EnvProvider] = Option(_provider)

  private def rootEnv(e: Env): Env = e.provider.map(p => rootEnv(p.env)).getOrElse(e)
  private val root = rootEnv(this)
  root.providedEnvs = this :: root.providedEnvs

  private var vars: Option[scala.collection.mutable.Map[String, Any]] = None
  private var _exprs: Option[Map[Expr, Int]] = None
  private val ids = scala.collection.mutable.Map[String, Any]()
  private var _result: Result[_ <: RowLike] = _
  private var _statement: java.sql.PreparedStatement = _
  //stores row count returned by SelectResult and all subresults.
  //if resources.maxResultSize is greater than zero
  //Row count is accumulated only for top level Env i.e. provider is None
  private var _rowCount = 0
  //used in macro to convert result at certain query depth and child position to macro generated object
  //converter map is set from macro and are stored in level Env object i.e. provider is None
  private var _rowConverters: Option[Map[List[Int], RowConverter[_ <: RowLike]]] = None

  def apply(name: String): Any = get(name).map {
    case e: Expr => e()
    case x => x
  } getOrElse (throw new MissingBindVariableException(name))
  def apply(name: String, path: List[String]) = get(name, path)
    .getOrElse(throw new MissingBindVariableException((name :: path).mkString(".")))

  def get(name: String): Option[Any] =
    vars.flatMap(_.get(name)) orElse provider.flatMap(_.env.get(name))
  def get(name: String, path: List[String]): Option[Any] = {
    def tr(p: List[String], v: Option[Any]): Option[Any] = p match {
      case Nil => v
      case n :: rest => v flatMap {
        case m: Map[String@unchecked, _] => tr(rest, m.get(n))
        case s: Seq[_] => tr(rest, Try(n.toInt).flatMap(i => Try(s(i))).toOption)
        case p: Product => tr(rest,
          Try(n.toInt).flatMap(v => Try(p.productElement(v - 1))).toOption)
        case null => Some(null)
        case x => None
      }
    }
    tr(path, get(name))
  }

  /* if not found into this variable map look into provider's if such exists */
  def contains(name: String): Boolean =
    vars.map(_.contains(name))
     .filter(_ == true)
     .getOrElse(provider.exists(_.env.contains(name)))
  def contains(name: String, path: List[String]): Boolean = {
    def tr(p: List[String], v: Any): Boolean = p match {
      case Nil => true
      case n :: rest => v match {
        case m: Map[String@unchecked, _] => m.get(n).exists(tr(rest, _))
        case s: Seq[_] => Try(n.toInt)
          .flatMap(i => Try(tr(rest, s(i))))
          .getOrElse(false)
        case p: Product => Try(n.toInt)
          .flatMap(v => Try(p.productElement(v - 1)))
          .map(tr(rest, _))
          .getOrElse(false)
        case _ => false
      }
    }
    get(name).exists(tr(path, _))
  }

  /* finds closest env with vars map set (Some(vars)) and looks there if variable exists */
  def containsNearest(name: String): Boolean =
    vars.map(_.contains(name)).getOrElse(provider.exists(_.env.containsNearest(name)))

  override def params: Map[String, Any] =
    (vars.map(_.toMap) orElse provider.map(_.env.params)).getOrElse(Map())

  private[tresql] def update(name: String, value: Any): Unit = {
    vars.map(_(name) = value) orElse provider.map(_.env(name) = value)
  }

  private[tresql] def update(vars: Map[String, Any]): Unit = {
    this.vars = if (vars == null) None else Some(scala.collection.mutable.Map.empty ++ vars)
  }

  private [tresql] def updateExprs(exprs: Map[Expr, Int]) = _exprs = Option(exprs)

  def apply(rIdx: Int): Result[_ <: RowLike] = {
    var i = 0
    var e: Env = this
    while (i < rIdx && e != null) {
      e = e.provider.map(_.env).orNull
      i += 1
    }
    if (i == rIdx && e != null) e.result else error("Result not available at index: " + rIdx)
  }

  def apply(expr: Expr): Int = _exprs.map(_.getOrElse(expr,
      error(s"Expression not found in env: $this. Expr: $expr"))).getOrElse(
          error(s"Expression '$expr' not found. Hidden column expressions not set in environment: $this"))

  private[tresql] def statement = _statement
  private[tresql] def statement_=(st: java.sql.PreparedStatement) = _statement = st

  private[tresql] def result = _result
  private[tresql] def result_=(r: Result[_ <: RowLike]) = _result = r

  private[tresql] def closeStatement = {
    root.providedEnvs foreach (e=> if (e.statement != null) e.statement.close)
  }

  private[tresql] def nextId(seqName: String): Any = {
    import CoreTypes._
    //TODO perhaps built expressions can be used to improve performance?
    implicit val implres = this
    val id = Query.unique[Any](implres.idExpr(seqName))
    ids(seqName) = id
    id
  }
  private[tresql] def currId(seqName: String): Any = (ids.get(seqName) orElse provider.map(_.env.currId(seqName))).get
  private[tresql] def currIdOption(seqName: String): Option[Any] =
    ids.get(seqName) orElse provider.flatMap(_.env.currIdOption(seqName))
  //update current id. This is called from QueryBuilder.IdExpr
  private[tresql] def currId(seqName: String, id: Any): Unit = ids(seqName) = id

  /* like currId, with the difference that this.ids is not searched */
  private[tresql] def idRef(seqName: String) = provider.map(_.env.currId(seqName))
  private[tresql] def idRefOption(seqName: String) = provider.flatMap(_.env.currIdOption(seqName))

  private[tresql] def rowCount: Int = provider.map(_.env.rowCount).getOrElse(_rowCount)
  private[tresql] def rowCount_=(rc: Int): Unit = {
    provider.map(_.env.rowCount = rc).getOrElse (this._rowCount = rc)
  }

  private[tresql] def rowConverter(queryPos: List[Int]): Option[RowConverter[_ <: RowLike]] =
    rowConverters.flatMap(_.get(queryPos))
  private[tresql] def rowConverters: Option[Map[List[Int], RowConverter[_ <: RowLike]]] =
    provider.flatMap(_.env.rowConverters) orElse _rowConverters
  private[tresql] def rowConverters_=(rc: Map[List[Int], RowConverter[_ <: RowLike]]): Unit = {
    provider.map(_.env.rowConverters = rc).getOrElse (this._rowConverters = Option(rc))
  }

  //resources methods
  private def get_res: Resources = provider.map(_.env.get_res).getOrElse(resources)
  override def conn: java.sql.Connection = db.map(get_res.extraResources(_).conn).getOrElse(get_res.conn)
  override def metadata = db.map(get_res.extraResources(_).metadata).getOrElse(get_res.metadata)
  /** for performance reasons dialect is val, so it does not need to be lifted on every call */
  override val dialect: CoreTypes.Dialect = liftDialect(db.map(get_res.extraResources(_).dialect).getOrElse(get_res.dialect))
  /** for performance reasons toBindableValue is val, so it does not need to be lifted on every call */
  override val toBindableValue: PartialFunction[Any, Any] =
    liftToBindableValue(db.map(get_res.extraResources(_).toBindableValue).getOrElse(get_res.toBindableValue))
  override def idExpr = db.map(get_res.extraResources(_).idExpr).getOrElse(get_res.idExpr)
  override def queryTimeout: Int = db.map(get_res.extraResources(_).queryTimeout).getOrElse(get_res.queryTimeout)
  override def fetchSize: Int = db.map(get_res.extraResources(_).fetchSize).getOrElse(get_res.fetchSize)
  override def maxResultSize: Int = db.map(get_res.extraResources(_).maxResultSize).getOrElse(get_res.maxResultSize)
  override def recursiveStackDepth: Int = db.map(get_res.extraResources(_).recursiveStackDepth).getOrElse(get_res.recursiveStackDepth)
  override def cache: Cache = db.map(get_res.extraResources(_).cache).getOrElse(get_res.cache)
  override def logger: TresqlLogger = db.map(get_res.extraResources(_).logger).getOrElse(get_res.logger)
  override def bindVarLogFilter: BindVarLogFilter = db.map(get_res.extraResources(_).bindVarLogFilter).getOrElse(get_res.bindVarLogFilter)
  override def isMacroDefined(name: String): Boolean =
    db.map(get_res.extraResources(_).isMacroDefined(name)).getOrElse(get_res.isMacroDefined(name))
  override def isBuilderMacroDefined(name: String): Boolean =
    db.map(get_res.extraResources(_).isBuilderMacroDefined(name)).getOrElse(get_res.isBuilderMacroDefined(name))
  override def isBuilderDeferredMacroDefined(name: String): Boolean =
    db.map(get_res.extraResources(_).isBuilderDeferredMacroDefined(name)).getOrElse(get_res.isBuilderDeferredMacroDefined(name))
  override def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp =
    db.map(get_res.extraResources(_).invokeMacro(name, parser, args))
      .getOrElse(get_res.invokeMacro(name, parser, args))
  override def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr =
    db.map(get_res.extraResources(_).invokeBuilderMacro(name, builder, args))
      .getOrElse(get_res.invokeBuilderMacro(name, builder, args))
  override def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr =
    db.map(get_res.extraResources(_).invokeBuilderDeferredMacro(name, builder, args))
      .getOrElse(get_res.invokeBuilderDeferredMacro(name, builder, args))

  protected def liftDialect(dialect: CoreTypes.Dialect) =
    if (dialect == null) null else dialect orElse defaultDialect
  protected def liftToBindableValue(tbv: PartialFunction[Any, Any]) =
    if (tbv == null) defaultToBindableValue else tbv orElse defaultToBindableValue

  //meta data methods
  override def table(name: String) = metadata.table(name)
  override def tableOption(name:String) = metadata.tableOption(name)
  override def procedure(name: String) = metadata.procedure(name)
  override def procedureOption(name:String) = metadata.procedureOption(name)

  //debugging methods
  def variables = "\nBind variables:" +
    vars.map(_.mkString("\n ", "\n ", "\n")).getOrElse("<none>")
  def allVariables = "\nBind variables:\n" +
    valsAsString("  ", this, e => e.vars.getOrElse(Map.empty))
  def allIds = "\nIds:\n" + valsAsString("  ", this, _.ids)
  private def valsAsString(offset: String, env: Env, vals: Env => scala.collection.Map[String, Any]): String =
    vals(env).mkString(s"\n$offset<vals>\n$offset", "\n" + offset, s"\n${offset}<vals end>\n") +
        env.provider.map(p => valsAsString(offset * 2, p.env, vals)).getOrElse("")

  override def toString: String = super.toString +
    provider.map(p=> s":$p#${p.env.toString}").getOrElse("<no provider>")

}

final case class ResourcesTemplate(override val conn: java.sql.Connection,
                                   override val metadata: Metadata,
                                   override val dialect: CoreTypes.Dialect,
                                   override val toBindableValue: PartialFunction[Any, Any],
                                   override val idExpr: String => String,
                                   override val queryTimeout: Int,
                                   override val fetchSize: Int,
                                   override val maxResultSize: Int,
                                   override val recursiveStackDepth: Int,
                                   override val params: Map[String, Any],
                                   override val extraResources: Map[String, Resources],
                                   override val logger: Logging#TresqlLogger,
                                   override val cache: Cache,
                                   override val bindVarLogFilter: Logging#BindVarLogFilter,
                                   macros: Any = null) extends Resources {

  private [tresql] def this(res: Resources) = this(
    res.conn, res.metadata, res.dialect, res.toBindableValue, res.idExpr, res.queryTimeout,
    res.fetchSize, res.maxResultSize, res.recursiveStackDepth, res.params, res.extraResources,
    res.logger, res.cache, res.bindVarLogFilter)

  private val macroResources = new MacroResourcesImpl(macros, metadata)

  override protected[tresql] def copyResources: Resources#Resources_ =
    super.copyResources.copy(macros = macroResources)

  override def isMacroDefined(name: String): Boolean = macroResources.isMacroDefined(name)
  override def isBuilderMacroDefined(name: String): Boolean = macroResources.isBuilderMacroDefined(name)
  override def isBuilderDeferredMacroDefined(name: String): Boolean = macroResources.isBuilderDeferredMacroDefined(name)
  override def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp =
    macroResources.invokeMacro(name, parser, args)
  override def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr =
    macroResources.invokeBuilderMacro(name, builder, args)
  override def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr =
    macroResources.invokeBuilderDeferredMacro(name, builder, args)
}


/** Implementation of [[Resources]] with thread local instance based on template */
trait ThreadLocalResources extends Resources {

  lazy final val resourcesTemplate: ResourcesTemplate = initResourcesTemplate
  /** Creates resources template with default values from {{{Resources}}}.
   * Subclasses can override this method.
   * */
  def initResourcesTemplate: ResourcesTemplate = new ResourcesTemplate(new Resources {
  })

  private val _threadResources = new ThreadLocal[Resources] {
    override def initialValue(): Resources = resourcesTemplate
  }

  private def threadResources: Resources = _threadResources.get
  private def threadResources_=(res: Resources): Unit = _threadResources.set(res)

  def apply(params: Map[String, Any], reusableExpr: Boolean) = new Env(params, this, reusableExpr)

  override def conn = threadResources.conn
  override def metadata = threadResources.metadata
  override def dialect = threadResources.dialect
  override def toBindableValue: PartialFunction[Any, Any] = threadResources.toBindableValue
  override def idExpr = threadResources.idExpr
  override def queryTimeout = threadResources.queryTimeout
  override def fetchSize = threadResources.fetchSize
  override def maxResultSize = threadResources.maxResultSize
  override def recursiveStackDepth: Int = threadResources.recursiveStackDepth
  override def extraResources: Map[String, Resources] = threadResources.extraResources
  override def isMacroDefined(macroName: String) = threadResources.isMacroDefined(macroName)
  override def isBuilderMacroDefined(macroName: String) = threadResources.isBuilderMacroDefined(macroName)
  override def isBuilderDeferredMacroDefined(macroName: String) = threadResources.isBuilderDeferredMacroDefined(macroName)
  override def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp =
    threadResources.invokeMacro(name, parser, args)
  override def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr =
    threadResources.invokeBuilderMacro(name, builder, args)
  override def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr =
    threadResources.invokeBuilderDeferredMacro(name, builder, args)

  override def cache: Cache = threadResources.cache
  def cache_=(idExpr: String => String) = sys.error("Do not set per thread cache for ThreadLocalResources. Cache is global")
  override def logger: TresqlLogger = threadResources.logger
  def logger_=(idExpr: String => String) = sys.error("Do not set per thread logger for ThreadLocalResources. Logger is global")
  override def bindVarLogFilter: BindVarLogFilter = threadResources.bindVarLogFilter
  def bindVarLogFilter_=(idExpr: String => String) = sys.error("Do not set per thread bindVarLogFilter for ThreadLocalResources. bindVarLogFilter is global")

  private def setProp(f: Resources => Resources): Unit = threadResources = f(threadResources)

  def conn_=(conn: java.sql.Connection) = setProp(_.withConn(conn))
  def metadata_=(metadata: Metadata) = setProp(_.withMetadata(metadata))
  def dialect_=(dialect: CoreTypes.Dialect) = setProp(_.withDialect(dialect))
  def toBindableValue_=(tbv: PartialFunction[Any, Any]) = setProp(_.withToBindableValue(tbv))
  def idExpr_=(idExpr: String => String) = setProp(_.withIdExpr(idExpr))
  def recursiveStackDepth_=(depth: Int) = setProp(_.withRecursiveStackDepth(depth))
  def queryTimeout_=(timeout: Int) =  setProp(_.withQueryTimeout(timeout))
  def fetchSize_=(fetchSize: Int) =  setProp(_.withFetchSize(fetchSize))
  def maxResultSize_=(size: Int) = setProp(_.withMaxResultSize(size))
  def extraResources_=(extra: Map[String, Resources]) = setProp(_.withExtraResources(extra))
  def setMacros(macros: Any) = setProp(_.withMacros(macros))

  def initFromTemplate: Unit = {
    threadResources = resourcesTemplate
  }
}

object Resources {
  def apply(): Resources = new Resources {}
}

/** Resources and configuration for query execution like database connection, metadata, database dialect etc. */
trait Resources extends MacroResources with CacheResources with Logging {

  private [tresql] case class Resources_(
                                          override val conn: java.sql.Connection,
                                          override val metadata: Metadata,
                                          override val dialect: CoreTypes.Dialect,
                                          override val toBindableValue: PartialFunction[Any, Any],
                                          override val idExpr: String => String,
                                          override val queryTimeout: Int,
                                          override val fetchSize: Int,
                                          override val maxResultSize: Int,
                                          override val recursiveStackDepth: Int,
                                          override val cache: Cache,
                                          override val logger: TresqlLogger,
                                          override val bindVarLogFilter: BindVarLogFilter,
                                          override val params: Map[String, Any],
                                          override val extraResources: Map[String, Resources],
                                          macros: MacroResources) extends Resources {
    override def isMacroDefined(name: String) = macros.isMacroDefined(name)
    override def isBuilderMacroDefined(name: String) = macros.isBuilderMacroDefined(name)
    override def isBuilderDeferredMacroDefined(name: String): Boolean = macros.isBuilderDeferredMacroDefined(name)
    override def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp =
      macros.invokeMacro(name, parser, args)
    override def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr =
      macros.invokeBuilderMacro(name, builder, args)
    override def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr =
      macros.invokeBuilderDeferredMacro(name, builder, args)
    override def toString = s"Resources_(conn = $conn, " +
      s"metadata = $metadata, dialect = $dialect, toBindableValue = $toBindableValue, idExpr = $idExpr, " +
      s"queryTimeout = $queryTimeout, fetchSize = $fetchSize, " +
      s"maxResultSize = $maxResultSize, recursiveStackDepth = $recursiveStackDepth, cache = $cache" +
      s"logger =$logger, bindVarLogFilter = $bindVarLogFilter" +
      s" params = $params)"

    override protected def copyResources: Resources#Resources_ = this
  }

  def conn: java.sql.Connection = null
  def metadata: Metadata = null
  def dialect: CoreTypes.Dialect = null
  def toBindableValue: PartialFunction[Any, Any] = null
  def idExpr: String => String = s => "nextval('" + s + "')"
  def queryTimeout = 0
  def fetchSize = 0
  def maxResultSize = 0
  def recursiveStackDepth: Int = 50
  def params: Map[String, Any] = Map()
  def extraResources: Map[String, Resources] = Map()
  override def isMacroDefined(name: String): Boolean = false
  override def isBuilderMacroDefined(name: String): Boolean = false
  override def isBuilderDeferredMacroDefined(name: String): Boolean = false
  override def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp = ???
  override def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr = ???
  override def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr = ???

  //resource construction convenience methods
  def withConn(conn: java.sql.Connection): Resources = copyResources.copy(conn = conn)
  def withMetadata(metadata: Metadata): Resources = copyResources.copy(metadata = metadata)
  def withDialect(dialect: CoreTypes.Dialect): Resources = copyResources.copy(dialect = dialect)
  def withToBindableValue(tbv: PartialFunction[Any, Any]) = copyResources.copy(toBindableValue = tbv)
  def withIdExpr(idExpr: String => String): Resources = copyResources.copy(idExpr = idExpr)
  def withQueryTimeout(queryTimeout: Int): Resources = copyResources.copy(queryTimeout = queryTimeout)
  def withFetchSize(fetchSize: Int): Resources = copyResources.copy(fetchSize = fetchSize)
  def withMaxResultSize(maxResultSize: Int): Resources = copyResources.copy(maxResultSize = maxResultSize)
  def withRecursiveStackDepth(recStackDepth: Int): Resources = copyResources.copy(recursiveStackDepth = recStackDepth)
  def withCache(cache: Cache): Resources = copyResources.copy(cache = cache)
  def withLogger(logger: TresqlLogger): Resources = copyResources.copy(logger = logger)
  def withBindVarLogFilter(filter: BindVarLogFilter): Resources = copyResources.copy(bindVarLogFilter = filter)
  def withParams(params: Map[String, Any]): Resources = copyResources.copy(params = params)
  def withMacros(macros: Any): Resources = copyResources.copy(macros = macros match {
    case mr: MacroResources => mr
    case _ => new MacroResourcesImpl(macros, metadata)
  })
  def withExtraResources(extra: Map[String, Resources]) = copyResources.copy(extraResources = extra)
  def withUpdatedExtra(name: String)(updater: Resources => Resources): Resources =
    copyResources.copy(extraResources = extraResources ++ Map(name -> updater(extraResources(name))))

  protected def copyResources: Resources#Resources_ =
    Resources_(conn, metadata, dialect, toBindableValue, idExpr, queryTimeout,
      fetchSize, maxResultSize, recursiveStackDepth, cache, logger, bindVarLogFilter,
      params, extraResources, this)

  protected def defaultDialect: CoreTypes.Dialect = { case e => e.defaultSQL }

  protected def defaultToBindableValue: PartialFunction[Any, Any] = { case v => v }
}

trait MacroResources {
  def macroResource: String = null
  def isMacroDefined(name: String): Boolean
  def isBuilderMacroDefined(name: String): Boolean
  def isBuilderDeferredMacroDefined(name: String): Boolean
  def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp
  def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr
  def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr
}

class MacroResourcesImpl(scalaMacros: Any, typeMapper: TypeMapper) extends MacroResources {
  private val macros = {
    val ml = new MacrosLoader(typeMapper)
    val tm =
      if (macroResource == null) ml.loadTresqlMacros(ml.load())
      else ml.loadTresqlMacros(ml.load(macroResource).getOrElse(ml.load()))
    tm.merge(ml.loadTresqlScalaMacros(scalaMacros))
  }

  override def isMacroDefined(name: String): Boolean         = macros.parserMacros.contains(name)
  override def isBuilderMacroDefined(name: String): Boolean  = macros.builderMacros.contains(name)
  override def isBuilderDeferredMacroDefined(name: String): Boolean = macros.builderDeferredMacros.contains(name)

  private def findMacro[A, B](name: String, map: Map[String, Seq[TresqlMacro[A, B]]], argsSize: Int) =
    map(name) match {
      case s if s.size == 1 => s.head
      case s => s.find(_.signature.pars.size == argsSize)
        .orElse(s.find(_.signature.hasRepeatedPar))
        .getOrElse(sys.error(s"Cannot find macro '$name' with $argsSize argument(s)"))
    }
  override def invokeMacro(name: String, parser: QueryParsers, args: List[Exp]): Exp = {
    findMacro[QueryParsers, Exp](name, macros.parserMacros, args.size).invoke(parser, args.toIndexedSeq)
  }
  override def invokeBuilderMacro(name: String, builder: QueryBuilder, args: List[Expr]): Expr =
    findMacro[QueryBuilder, Expr](name, macros.builderMacros, args.size).invoke(builder, args.toIndexedSeq)
  override def invokeBuilderDeferredMacro(name: String, builder: QueryBuilder, args: List[Exp]): Expr =
    findMacro[QueryBuilder, Expr](name, macros.builderDeferredMacros, args.size).invoke(builder, args.toIndexedSeq)
}

trait CacheResources {
  /** Parsed statement {{{Cache}}} */
  def cache: Cache = null
}

trait Logging {
  type TresqlLogger = (=> String, => Seq[(String, Any)], LogTopic) => Unit
  type BindVarLogFilter = PartialFunction[(String, Any), String]

  def logger: TresqlLogger = null
  def bindVarLogFilter: BindVarLogFilter = {
    case (fullName, _) if fullName == "password" || fullName.startsWith("password.") => "***"
  }

  def log(msg: => String, params: => Seq[(String, Any)] = Nil, topic: LogTopic = LogTopic.info): Unit =
    if (logger != null) logger(msg, params, topic)
}

private [tresql] trait EnvProvider {
  private[tresql] def env: Env
}

class MissingBindVariableException(val name: String)
  extends RuntimeException(s"Missing bind variable: $name")

class TresqlException(val sql: String, val bindVars: List[(String, Any)], sqlExc: SQLException)
  extends RuntimeException(sqlExc)

class ChildSaveException(val name: String, msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
  def this(name: String, cause: Throwable) = this(name, null, cause)
  def this(name: String, msg: String) = this(name, msg, null)
}

trait LogTopic
object LogTopic {
  case object tresql extends LogTopic
  case object sql extends LogTopic
  case object params extends LogTopic
  case object sql_with_params extends LogTopic
  case object ort extends LogTopic
  case object info extends LogTopic
}
