package org.tresql

import sys._
import scala.util.Try
import parsing.Exp
import metadata.key_
import parsing._

import scala.annotation.tailrec

object QueryBuildCtx {
  trait Ctx
  object ARR_CTX extends Ctx
  object QUERY_CTX extends Ctx
  object DML_CTX extends Ctx
  object FROM_CTX extends Ctx
  object JOIN_CTX extends Ctx
  object WHERE_CTX extends Ctx
  object COL_CTX extends Ctx
  object ORD_CTX extends Ctx
  object GROUP_CTX extends Ctx
  object HAVING_CTX extends Ctx
  object VALUES_CTX extends Ctx
  object LIMIT_CTX extends Ctx
  object FUN_CTX extends Ctx
  object WITH_CTX extends Ctx
  object WITH_TABLE_CTX extends Ctx
}

trait QueryBuilder extends EnvProvider with org.tresql.Transformer with Typer { this: org.tresql.Query =>

  import QueryBuildCtx._

  val STANDART_BIN_OPS = Set("<=", ">=", "<", ">", "!=", "=", "~", "!~", "in", "!in",
      "++", "+",  "-", "&&", "||", "*", "/", "&", "|")
  val OPTIONAL_OPERAND_BIN_OPS = Set("++", "+",  "-", "&&", "||", "*", "/", "&", "|")

  //bind variables for jdbc prepared statement
  class RegisteredBindVariables {
    private val bindVariables = scala.collection.mutable.ListBuffer[Expr]()
    private var bindVars: List[Expr] = null
    private val registeredBindVars = scala.collection.mutable.Set[Int]()
    private[tresql] def register(expr: Expr) = {
      val hash = System.identityHashCode(expr)
      if (!registeredBindVars.contains(hash)) {
        bindVariables += expr
        registeredBindVars += hash
      }
    }
    private[tresql] def variables = {
      if (bindVars != null) bindVars else {
        bindVars = bindVariables.toList
        bindVars
      }
    }

    private[tresql] def clear = {
      bindVariables.clear()
      bindVars = null
      registeredBindVars.clear()
    }
  }
  private val bindVars = new RegisteredBindVariables
  private[tresql] def clearRegisteredBindVariables() = bindVars.clear
  private[tresql] def registeredBindVariables = bindVars.variables

  //children count. Is increased every time buildWithNew method is called or recursive expr created
  private[tresql] var childrenCount = 0

  //set by buildInternal method when building Query for potential use in RecursiveExpr
  private[tresql] var recursiveQueryExp: parsing.Query = _

  //used for non flat updates, i.e. hierarhical object.
  private val _childUpdatesBuildTime = scala.collection.mutable.ListBuffer[(Expr, String)]()
  private lazy val childUpdates = _childUpdatesBuildTime.toList

  //context stack as buildInternal method is called
  protected var ctxStack = List[Ctx]()

  //used internally while building expression
  private var separateQueryFlag = false
  //table defs from Typer. This is used for establishing relationships between tables and hierarchical queries
  protected var tableDefs: List[Def] = Nil
  //table alias and key column(s) (multiple in the case of composite primary key) to be added as
  //hidden columns for query built by this builder in order to establish relation ship with query
  //built by child builder
  private var joinsWithChildren: Set[(String, List[String])] = Set()
  lazy val joinsWithChildrenColExprs = for {
    tc <- this.joinsWithChildren
    c <- tc._2
  } yield ColExpr(IdentExpr(List(tc._1, c)), tc._1 + "_" + c + "_", Some(false), hidden = true)

  /*****************************************************************************
  ****************** methods to be implemented or overriden ********************
  ******************************************************************************/
  private[tresql] def newInstance(env: Env, queryDepth: Int, bindIdx: Int, childIdx: Int): QueryBuilder
  def env: Env = ???
  private[tresql] def queryDepth: Int = ???
  private[tresql] def bindIdx: Int = ???
  private[tresql] def bindIdx_=(idx: Int): Unit = ???
  //childIdx indicates this builder index relatively to it's parent
  private[tresql] def childIdx: Int = ???
  /*****************************************************************************/

  case class ConstExpr(value: Any) extends BaseExpr {
    override def apply() = value match {
      case Null | NullUpdate => null
      case _ => value
    }
    def defaultSQL = value match {
      case v: Int => v.toString
      case v: Number => v.toString
      case v: String => "'" + v.replace("'", "''") + "'"
      case v: Boolean => v.toString
      case null => "null"
      case e: Exp => e.tresql
      case x => String.valueOf(x)
    }
  }

  case class AllExpr() extends PrimitiveExpr {
    def defaultSQL = "*"
  }
  case class IdentAllExpr(name: List[String]) extends PrimitiveExpr {
    def defaultSQL = name.mkString(".") + ".*"
  }

  case class VarExpr(name: String, members: List[String], opt: Boolean) extends BaseVarExpr {
    override def apply() = env(name, members)
    override def defaultSQL = {
      QueryBuilder.this.bindVars.register(this)
      val s = if (!env.reusableExpr && (env contains name) && (members == null | members == Nil)) {
        this() match {
          case l: scala.collection.Iterable[_] =>
            if (l.nonEmpty) "?," * (l size) dropRight 1 else {
              //return null for empty collection (not to fail in 'in' operator)
              "null"
            }
          case _: Array[Byte] => "?"
          case a: Array[_] => if (a.length > 0) "?," * (a length) dropRight 1 else {
            //return null for empty array (not to fail in 'in' operator)
            "null"
          }
          case _ => "?"
        }
      } else "?"
      s
    }
    def fullName = (name :: members).mkString(".")
    override def toString = if (env.contains(name, members)) fullName + " = " + this() else fullName
  }

  case class IdExpr(seqName: String) extends BaseVarExpr {
    private val (seq, bind_var) = {
      val idx = seqName.indexOf(':')
      if (idx == -1) (seqName, env.tableOption(seqName).map(_.key.cols).find(_.size == 1).flatMap(_.headOption))
      else (seqName.substring(0, idx), Option(seqName.substring(idx + 1)))
    }
    override def apply() = idFromEnv(true) getOrElse env.nextId(seq)
    private[tresql] def idFromEnv(updateCurrId: Boolean) = for {
      key <- bind_var if env.containsNearest(key) && env(key) != null
    } yield {
      //if primary key is set as an environment variable use it instead of sequence
      val id = env(key)
      if (updateCurrId) env.currId(seq, id)
      id
    }
    def peek: Any = idFromEnv(false) getOrElse "<seq value>"
    override def toString = s"#$seqName = $peek"
  }

  case class IdRefExpr(seqName: String) extends BaseVarExpr {
    private val (seq, bind_var) = {
      val idx = seqName.indexOf(':')
      if (idx == -1) (seqName, None)
      else (seqName.substring(0, idx), Option(seqName.substring(idx + 1)))
    }
    override def apply() = {
      bind_var.map { key =>
        if (env.containsNearest(key) && env(key) != null) {
          val id = env(key)
          env.currId(seq, id)
          id
        } else null
      }.orElse(getId(seq)).getOrElse(
        error(s"Current id not found for sequence '$seqName' in environment:\n$env"))
    }
    private def getId(name: String): Option[Any] = env.idRefOption(name).orElse {
      env.tableOption(name).flatMap(t=> t.refTable.get(t.key.cols).map(getId))
    }
    def peek: Any = bind_var.flatMap(env.get).orElse(getId(seq)).getOrElse(null)
    override def toString = s":#$seqName = ${getId(seqName).getOrElse("?")}"
  }

  case class ResExpr(nr: Int, col: Any) extends BaseVarExpr {
    override def apply() = env(nr) match {
      case null => error(s"Ancestor result with number $nr not found for column '$col'")
      case r => col match {
        case c: Ident => r(c.ident.mkString("."))
        case c: String => r(c)
        case c: Int if c > 0 => r(c - 1)
        case c: Int => error("column index in result expression must be greater than 0. Is: " + c)
      }
    }
    def name = s"$nr($col)"
    override def toString = s"$name = ${Try(this()).getOrElse("value not available")}"
  }

  class BaseVarExpr extends BaseExpr {
    def defaultSQL = {
      QueryBuilder.this.bindVars.register(this)
      "?"
    }
  }

  case class CastExpr(exp: Expr, typ: String) extends BaseExpr {
    def defaultSQL = exp.sql + "::" + typ
  }

  case class UnExpr(op: String, operand: Expr) extends BaseExpr {
    def defaultSQL = op match {
      case "-" => "-" + operand.sql
      case "!" =>
        (if (operand.exprType == classOf[SelectExpr]) "not exists " else "not ") +
          operand.sql
      case _ => error("unknown unary operation " + op)
    }
    override def exprType: Class[_] = if ("-" == op) operand.exprType else classOf[ConstExpr]
  }

  case class InExpr(lop: Expr, rop: List[Expr], not: Boolean) extends BaseExpr {
    def defaultSQL = lop.sql + (if (not) " not" else "") + rop.map(_.sql).mkString(" in(", ", ", ")")
    override def exprType = classOf[ConstExpr]
  }

  case class BinExpr(op: String, lop: Expr, rop: Expr) extends BaseExpr {
    def cols = {
      def c(e: Expr): QueryBuilder#ColsExpr = e match {
        case e: SelectExpr => e.cols
        case e: BinExpr => c(e.lop)
        case e: BracesExpr => c(e.expr)
        case e: DeleteExpr if e.returning.nonEmpty => e.returning.get
        case WithSelectExpr(_, e) => e.cols
        case WithBinExpr(_, e) => c(e)
        case x => sys.error(s"Unexpected ColExpr type: `${x.getClass}`")
      }
      c(lop)
    }
    def isQuery(e: Expr) = e.exprType == classOf[SelectExpr]
    def isQueryOrReturning(e: Expr): Boolean = e match {
      case BinExpr(_, lop, rop) => isQueryOrReturning(lop) && isQueryOrReturning(rop)
      case _: SelectExpr | _: WithSelectExpr | _: WithBinExpr => true
      case e: DeleteExpr => e.returning.nonEmpty
      case BracesExpr(e) => isQueryOrReturning(e)
      case _ => false
    }

    override def apply() =
      op match {
        case "&&" => sel(sql, cols)
        case "++" => sel(sql, cols)
        case "+" => if (isQueryOrReturning(this)) sel(sql, cols) else super.apply()
        case "-" => if (isQueryOrReturning(this)) sel(sql, cols) else super.apply()
        case "=" => lop match {
          //assign expression
          case VarExpr(variable, _, _) =>
            env(variable) = rop()
            env(variable)
          case x => super.apply()
        }
        case x => super.apply()
      }

    def defaultSQL = op match {
      case "*" => lop.sql + " * " + rop.sql
      case "/" => lop.sql + " / " + rop.sql
      case "||" => lop.sql + " || " + rop.sql
      case "++" => lop.sql + " union all " + rop.sql
      case "&&" => lop.sql + " intersect " + rop.sql
      case "+" => lop.sql + (if (isQueryOrReturning(this)) " union " else " + ") + rop.sql
      case "-" => lop.sql + (if (isQueryOrReturning(this)) " except " else " - ") + rop.sql
      case "=" => rop match {
        case ConstExpr(Null) => lop.sql + " is " + rop.sql
        case _: ArrExpr => lop.sql + " in " + rop.sql
        case _ => lop.sql + " = " + rop.sql
      }
      case "!=" => rop match {
        case ConstExpr(Null) => lop.sql + " is not " + rop.sql
        case _: ArrExpr => lop.sql + " not in " + rop.sql
        case _ => lop.sql + " != " + rop.sql
      }
      case "<" => lop.sql + " < " + rop.sql
      case ">" => lop.sql + " > " + rop.sql
      case "<=" => lop.sql + " <= " + rop.sql
      case ">=" => lop.sql + " >= " + rop.sql
      case "&" => (if (isQuery(lop)) "exists " else "") +
        lop.sql + " and " + (if (isQuery(rop)) "exists " else "") +
        rop.sql
      case "|" => (if (isQuery(lop)) "exists " else "") + lop.sql +
        " or " + (if (isQuery(rop)) "exists " else "") + rop.sql
      case "~" => lop.sql + " like " + rop.sql
      case "!~" => lop.sql + " not like " + rop.sql
      case s @ ("in" | "!in") => lop.sql + (if (s.startsWith("!")) " not" else "") + " in " +
        (rop match {
          case _: BracesExpr | _: ArrExpr => rop.sql
          case _ => "(" + rop.sql + ")"
        })
      case x if x.startsWith("`") && x.endsWith("`") =>
        //sql operator escape syntax
        lop.sql + x.replace('`', ' ') + rop.sql
      case _ => error("unknown operation " + op)
    }
    override def exprType: Class[_] =
      if (List("&&", "++", "+", "-", "*", "/") contains op) {
        if (lop.exprType == rop.exprType) lop.exprType else super.exprType
      }
      else classOf[ConstExpr]
  }

  case class FunExpr(
    name: String,
    params: List[Expr],
    distinct: Boolean = false,
    aggregateOrder: Option[Expr] = None,
    aggregateWhere: Option[Expr] = None
  ) extends BaseExpr {
    override def apply() = {
      call("{call " + sql + "}")
    }
    def defaultSQL = {
      val order = aggregateOrder.map(_.sql).map(" order by " + _).mkString
      val filter = aggregateWhere.map(_.sql).map(w => s" filter (where $w)").mkString
      name + (params map (_.sql)).mkString(
        "(" + (if (distinct) "distinct " else ""),
        ",",
        s"$order)$filter")
    }
    override def toString = name + (params map (_.toString)).mkString("(", ",", ")")
  }

  case class FunAsTableExpr(expr: Expr, colsDefs: Option[List[TableColDefExpr]], withOrdinality: Boolean)
    extends PrimitiveExpr {
    override def defaultSQL: String = expr.sql
    def colsSql: String = colsDefs.map(_.map(_.sql).mkString("(", ", ", ")")).getOrElse("")
  }

  case class TableColDefExpr(name: String, typ: Option[String]) extends PrimitiveExpr {
    override def defaultSQL: String = name
  }

  case class RecursiveExpr(exp: parsing.Query) extends BaseExpr {
    //take this from childrenCount, since it RecursiveExpr is not built with new builder
    private val initChildIdx = QueryBuilder.this.childrenCount
    private val rowConverter = env.rowConverter(QueryBuilder.this.queryDepth, initChildIdx)
    if (queryDepth >= env.recursiveStackDepth)
      error(s"Recursive execution stack depth $queryDepth exceeded, check for loops in data or increase {{{Resources#recursiveStackDepth}}} setting.")
    val qBuilder = newInstance(new Env(QueryBuilder.this, env.db, env.reusableExpr),
      queryDepth + 1, 0, initChildIdx)
    qBuilder.recursiveQueryExp = recursiveQueryExp
    //TODO pass rowConverter to built SelectExpr!
    lazy val expr: Expr = qBuilder.buildInternal(exp, QUERY_CTX)
    override def apply() = expr()
    def defaultSQL = expr sql
  }

  case class ArrExpr(elements: List[Expr]) extends BaseExpr {
    override def apply() = {
      val result = elements map {
        case e: ConstExpr => wrapExprInSelect(e)()
        case e: VarExpr => wrapExprInSelect(e)()
        case e => e()
      }
      env.rowConverter(queryDepth, childIdx).map { conv =>
        new CompiledArrayResult(result, conv)
      }.getOrElse(new DynamicArrayResult(result))
    }
    def defaultSQL = elements map { _.sql } mkString ("(", ", ", ")")
    override def toString = elements map { _.toString } mkString ("[", ", ", "]")
  }

  case class SelectExpr(tables: List[Table], filter: Expr, cols: ColsExpr,
      distinct: Boolean, group: Expr, order: Expr,
      offset: Expr, limit: Expr, aliases: Map[String, Table], parentJoin: Option[Expr]) extends BaseExpr {
    override def apply() = sel(sql, cols)
    def defaultSQL = "select " + (if (distinct) "distinct " else "") +
      cols.sql +
      (tables match {
        case List(Table(ConstExpr(Null), _, _, _, _, _)) => ""
        case _ => " from " + tables.head.sql + join(tables)
      }) +
      //(filter map where).getOrElse("")
      Option(where).map(" where " + _).getOrElse("") +
      (if (group == null) "" else " group by " + group.sql) +
      (if (order == null) "" else Option(order.sql).map(" order by " + _).getOrElse("")) +
      (if (offset == null) "" else " offset " + offset.sql) +
      (if (limit == null) "" else " limit " + limit.sql)
    def join(tables: List[Table]): String = {
      //used to find table if alias join is used
      def find(t: Table) = t match {
        case t @ Table(IdentExpr(n), null, _, _, _, _) => aliases.getOrElse(n.mkString("."), t)
        case t => t
      }
      (tables: @unchecked) match {
        case t :: Nil => ""
        case t :: l => (Option(" " + l.head.sqlJoin(find(t))) filter (_.size > 1) getOrElse "") + join(l)
      }
    }
    def where = {
      val fsql = Option(filter).map(
        e => (if (e.exprType == classOf[SelectExpr]) "exists " else "") + e.sql).orNull
      parentJoin.map(e => Option(fsql).map("(" + _ + ") and " + e.sql)
        .getOrElse(e.sql)).getOrElse(fsql)
    }
    override def toString = sql + " (" + QueryBuilder.this + ")\n" +
      cols.cols.filter(_.separateQuery).map {
        "    " * (queryDepth + 1) + _.toString
      }.mkString
  }
  case class Table(table: Expr, alias: String, join: TableJoin, outerJoin: String, nullable: Boolean, schema: String)
  extends PrimitiveExpr {
    def name = table.sql
    def aliasOrName = if (alias != null) alias else name
    def tableNameWithSchema = table match {
      case IdentExpr(n) => (if (schema == null) n else schema :: n) mkString "."
      case x => error(s"Cannot get table name with schema since table expr is no primitive: $x")
    }
    def sqlJoinCondition(joinTable: Table) = {
      def fkAliasJoin(i: IdentExpr) = (if (i.name.size < 2 && joinTable.alias != null)
        i.copy(name = joinTable.alias :: i.name) else i).sql +
        " = " + aliasOrName + "." +
        env.table(joinTable.tableNameWithSchema).ref(tableNameWithSchema, List(i.name.last)).refCols.mkString
      def defaultJoin(jcols: (key_, key_)) = {
        //may be default join columns had been calculated during table build implicit left outer join calculation
        val j = if (jcols != null) jcols else env.join(joinTable.tableNameWithSchema, tableNameWithSchema)
        (j._1.cols zip j._2.cols map { t =>
          joinTable.aliasOrName + "." + t._1 + " = " + aliasOrName + "." + t._2
        }) mkString " and "
      }
      join match {
        //foreign key join shortcut syntax
        case TableJoin(false, e: IdentExpr, _, _) => fkAliasJoin(e)
        //product join
        case TableJoin(false, null, _, _) => null //no join condition
        //normal join
        case TableJoin(false, e, _, _) => e match { case ArrExpr(List(j)) => j.sql /*remove unnecessary braces*/ case _ => e.sql }
        //default join
        case TableJoin(true, null, _, dj) => defaultJoin(dj)
        //default join with additional expression
        case TableJoin(true, j: Expr, _, dj) =>
          defaultJoin(dj) + " and " + (j match {
          //primary key equals search
          case _: ConstExpr | _: VarExpr | _: ResExpr => joinTable.aliasOrName + "." +
            env.table(joinTable.name).key.cols(0) + " = " + j.sql
          //primary key in search
          case ArrExpr(l) => joinTable.aliasOrName + "." +
            env.table(joinTable.name).key.cols(0) + (l map (_.sql) mkString (" in(", ", ", ")"))
          //normal join expression
          case e => (if (e.exprType == classOf[SelectExpr]) "exists " else "") + e.sql
        })
        case x => error(s"Unrecognized join condition: $x")
      }
    }
    def sqlJoin(joinTable: Table) = {
      def joinPrefix(implicitLeftJoinPossible: Boolean) = (outerJoin match {
        case "l" => "left "
        case "r" => "right "
        case j if j != "i"/*forced inner join*/ && implicitLeftJoinPossible && nullable => "left "
        case _ => ""
      }) + "join "
      join match {
        //no join (used for table alias join)
        case TableJoin(_, _, true, _) => ""
        //product join
        case TableJoin(false, ArrExpr(Nil) | null, _, _) =>
          joinPrefix(false) + s"$sql on ${ConstExpr(true).sql}" //not all db support bool value
        case null => error(s"Cannot build sql query, join not specified between tables '$joinTable' and '$this'.")
        //other types of join
        case _ => joinPrefix(true) + sql + " on " + sqlJoinCondition(joinTable)
      }
    }
    override def defaultSQL = {
      table.sql + Option(alias).map { al =>
        table match {
          case fat: FunAsTableExpr =>
            (if (fat.withOrdinality) " with ordinality" else "") + " " + al + fat.colsSql
          case _ => " " + al
        }
      }.getOrElse("")
    }
  }
  case class TableJoin(default: Boolean, expr: Expr, noJoin: Boolean,
    defaultJoinCols: (key_, key_)) extends PrimitiveExpr {
    def defaultSQL = ???
    override def toString = "TableJoin(\ndefault: " + default + "\nexpr: " + expr +
      "\nno-join flag: " + noJoin + ")\n"
  }
  case class ColsExpr(cols: List[ColExpr],
      hasAll: Boolean, hasIdentAll: Boolean, hasHidden: Boolean) extends PrimitiveExpr {
    override def defaultSQL = cols.withFilter(!_.separateQuery).map(_.sql).mkString(", ")
    override def toString = cols.map(_.toString).mkString("Columns(", ", ", ")")
  }
  case class ColExpr(col: Expr, alias: String, sepQuery: Option[Boolean] = None, hidden: Boolean = false)
    extends PrimitiveExpr {
    val separateQuery = sepQuery.getOrElse(QueryBuilder.this.separateQueryFlag)
    def defaultSQL = col.sql + (if (alias != null) " " + alias else "")
    def name = if (alias != null) alias.stripPrefix("\"").stripSuffix("\"") else col match {
      case IdentExpr(n) => n(n.length - 1)
      case _ => null
    }
    override def toString =
      s"""${col.toString}${if (alias != null) " " + alias
        else ""}, hidden = $hidden, separateQuery = $separateQuery"""
  }
  case class HiddenColRefExpr(expr: Expr, resType: Class[_]) extends PrimitiveExpr {
    override def apply() = {
      val (res, idx) = (env(0), env(this))
      if (resType == classOf[Int]) res.int(idx)
      else if (resType == classOf[Long]) res.long(idx)
      else if (resType == classOf[Double]) res.double(idx)
      else if (resType == classOf[Boolean]) res.boolean(idx)
      else if (resType == classOf[BigDecimal]) res.bigdecimal(idx)
      else if (resType == classOf[String]) res.string(idx)
      else if (resType == classOf[java.util.Date]) res.timestamp(idx)
      else if (resType == classOf[java.sql.Date]) res.date(idx)
      else if (resType == classOf[java.sql.Timestamp]) res.timestamp(idx)
      else if (resType == classOf[java.lang.Integer]) res.jInt(idx)
      else if (resType == classOf[java.lang.Long]) res.jLong(idx)
      else if (resType == classOf[java.lang.Double]) res.jDouble(idx)
      else if (resType == classOf[java.lang.Boolean]) res.jBoolean(idx)
      else if (resType == classOf[Array[Byte]]) res.bytes(idx)
      else if (resType == classOf[java.io.InputStream]) res.stream(idx)
      else if (resType == classOf[java.math.BigDecimal]) res.bigdecimal(idx) match {
        case null => null
        case bd => bd.bigDecimal
      } else res(idx)
    }
    override def defaultSQL = expr.sql
    override def toString() = expr + ":" + resType
  }
  case class IdentExpr(name: List[String]) extends PrimitiveExpr {
    def defaultSQL = name.mkString(".")
  }
  case class Order(ordExprs: List[(Expr, Expr, Expr)]) extends PrimitiveExpr {
    def defaultSQL = ordExprs.filterNot(_._2 == null).map(o => (o._2 match {
      case UnExpr("~", e) => e.sql + " desc"
      case e => e.sql + " asc"
    }) + (if (o._1 != null) " nulls first" else if (o._3 != null) " nulls last" else ""))
    .mkString(", ") match {
      case "" => null
      case s => s
    }
  }
  case class Group(groupExprs: List[Expr], having: Expr) extends PrimitiveExpr {
    def defaultSQL = (groupExprs map (_.sql)).mkString(",") +
      (if (having != null) " having " + having.sql else "")
  }

  case class WithTableExpr(name: String, cols: List[String], recursive: Boolean, query: Expr)
    extends PrimitiveExpr {
    def defaultSQL = name + (if (cols.isEmpty) "" else cols.mkString("(", ", ", ")")) +
      " as (" + query.sql + ")"
  }
  trait WithExpr extends BaseExpr {
    private val recursive = tables.head.recursive
    def tables: List[WithTableExpr]
    def query: Expr
    override def defaultSQL = "with " + (if (recursive) "recursive " else "") +
      tables.map(_.sql).mkString(", ") + " " + query.sql
  }
  case class WithSelectExpr(tables: List[WithTableExpr], query: SelectExpr)
    extends WithExpr {
    override def apply() = sel(sql, query.cols)
  }
  case class WithBinExpr(tables: List[WithTableExpr], query: BinExpr)
    extends WithExpr {
    override def apply() = sel(sql, query.cols)
  }
  class WithInsertExpr(val tables: List[WithTableExpr], val query: InsertExpr)
    extends InsertExpr(query.table, query.alias, query.cols, query.vals, query.returning)
    with WithExpr
  class WithUpdateExpr(val tables: List[WithTableExpr], val query: UpdateExpr)
    extends UpdateExpr(query.table, query.alias, query.filter, query.cols, query.vals, query.returning)
    with WithExpr
  class WithDeleteExpr(val tables: List[WithTableExpr], val query: DeleteExpr)
    extends DeleteExpr(query.table, query.alias, query.filter, query.using, query.returning)
    with WithExpr

  class InsertExpr(table: IdentExpr, alias: String, val cols: List[Expr], val vals: Expr,
      returning: Option[ColsExpr])
    extends DeleteExpr(table, alias, null, null, returning) {
    override def apply() = super.apply() match {
      case r: DMLResult =>
        //include in result id value of the inserted record if it's obtained from IdExpr
        val id = env.currIdOption(table.defaultSQL)
        new InsertResult(r.count, r.children, id)
      case r => r
    }
    override protected def _sql = "insert into " + table.sql + (if (alias == null) "" else " " + alias) +
      " (" + cols.map(_.sql).mkString(", ") + ")" + " " + vals.sql + returningSql
  }
  case class ValuesExpr(vals: List[Expr]) extends PrimitiveExpr {
    def defaultSQL = vals map (_.sql) mkString("values ", ",", "")
  }
  case class ValuesFromSelectExpr(select: SelectExpr) extends PrimitiveExpr {
    // start from clause with second table (first table is updateable)
    def joinToBaseTableSql = select.tables match {
      case head :: next :: _ => Option(next sqlJoinCondition head)
      case _ => None
    }
    def defaultSQL = select.tables match {
      case head :: next :: _ => next.sql + select.join(select.tables.tail)
      case _ => ""
    }
  }
  class UpdateExpr(table: IdentExpr, alias: String, filter: List[Expr],
      val cols: List[Expr], val vals: Expr, returning: Option[ColsExpr])
    extends DeleteExpr(table, alias, filter, null, returning) {
    override def apply() = {
      cols match {
        //execute only child updates since this one does not have any column
        case Nil =>
          //execute any BaseVarExpr in filter to give chance to update currId for corresponding children IdRefExpr have values
          if (filter != null) filter foreach (transform (_, {case id: BaseVarExpr => id(); id}))
          new UpdateResult(children = executeChildUpdates)
        case _ => super.apply() match { case r: DMLResult => new UpdateResult(r) case r => r }
      }
    }
    override protected def _sql = "update " + table.sql + (if (alias == null) "" else " " + alias) +
      " set " + (vals match {
        case ValuesExpr(v) => (cols zip v map { v => v._1.sql + " = " + v._2.sql }).mkString(", ")
        case q: SelectExpr => cols.map(_.sql).mkString("(", ", ", ")") + " = " + "(" + q.sql + ")"
        case f: ValuesFromSelectExpr =>
          val fsql = f.sql
          cols.map(_.sql).mkString(", ") + (if (fsql.isEmpty) "" else " from " + fsql)
        case x => error("Knipis: " + x)
      }) + {
        val filterSql = if (filter == null) null else where
        val joinWithUpdateTableSql = vals match {
          case vfs: ValuesFromSelectExpr => vfs.joinToBaseTableSql
          case _ => None
        }
        joinWithUpdateTableSql ++ Option(filterSql) match {
          case List(j, f) => s" where ($j) and ($f)"
          case List(f) => s" where $f"
          case _ => ""
        }
      } + returningSql
  }
  case class DeleteExpr(table: IdentExpr, alias: String, filter: List[Expr],
                        using: Expr, returning: Option[ColsExpr])
    extends BaseExpr {
    override def apply() =
      returning
        .map(sel(sql, _)) //in the case of returning clause execute statement as select
        .getOrElse {
          val r = update(sql)
          //execute children only if this expression has affected some rows
          if (r > 0)
            if (childUpdates.isEmpty) new DeleteResult(Some(r))
            else executeChildUpdates match {
              case x if x.isEmpty => new DeleteResult(Some(r))
              case x => new DeleteResult(Some(r), x)
            }
          else new DeleteResult(Some(r))
        }
    protected def _sql = "delete from " + table.sql + (if (alias == null) "" else " " + alias) +
      (if (using == null) "" else {
        val usql = using.sql
        if (usql.isEmpty) "" else " using " + usql
      }) + {
        val filterSql = if (filter == null  || filter.isEmpty) null else where
        val joinWithDeleteTableSql = using match {
          case u: ValuesFromSelectExpr => u.joinToBaseTableSql
          case _ => None
        }
        joinWithDeleteTableSql ++ Option(filterSql) match {
          case List(j, f) => s" where ($j) and ($f)"
          case List(f) => s" where $f"
          case _ => ""
        }
      } + returningSql
    override def defaultSQL = _sql
    def where = filter match {
      case (c @ ConstExpr(x)) :: Nil => Option(alias).getOrElse(table.sql) + "." +
        env.table(table.sql).key.cols(0) + " = " + c.sql
      case (v: VarExpr) :: Nil => Option(alias).getOrElse(table.sql) + "." +
        env.table(table.sql).key.cols.head + " = " + v.sql
      case f :: Nil => (if (f.exprType == classOf[SelectExpr]) "exists " else "") + f.sql
      case l => Option(alias).getOrElse(table.sql) + "." + env.table(table.sql).key.cols(0) + " in(" +
        (l map { _.sql }).mkString(",") + ")"
    }
    def returningSql =
      returning.map(rc => s" returning ${rc.sql}").getOrElse("")
  }

  case class BracesExpr(expr: Expr) extends BaseExpr {
    override def apply() = expr()
    def defaultSQL = "(" + expr.sql + ")"
    override def exprType = expr.exprType
  }

  //sql helper expressions to enable advanced syntax. these expression are expected to be
  //create with the help of macros
  case class SQLExpr(sqlSnippet: String, bindVars: List[VarExpr]) extends PrimitiveExpr {
    def defaultSQL = {
      bindVars foreach(_.sql)
      sqlSnippet
    }
  }
  case class SQLConcatExpr(expr: Expr*) extends BaseExpr {
    private def findSQL(expr: Expr): Option[QueryBuilder#ColsExpr] = expr match {
      case e: QueryBuilder#SQLConcatExpr => findSQLInSeq(e.expr)
      case e: QueryBuilder#BracesExpr => findSQL(e.expr)
      case e: QueryBuilder#SelectExpr => Some(e.cols)
      case e: QueryBuilder#BinExpr if e.exprType == classOf[SelectExpr] => Some(e.cols)
      case _ => None
    }
    private def findSQLInSeq(exprs: Seq[Expr]): Option[QueryBuilder#ColsExpr] =
      exprs.toList match {
        case Nil => None
        case expr :: tail => findSQL(expr).orElse(findSQLInSeq(tail))
      }
    val cols = findSQL(this).getOrElse(ColsExpr(List(ColExpr(AllExpr(), null)), true, false, false))
    override def apply() = sel(sql, cols)
    def defaultSQL = expr.filter(_ != null).map(_.sql) mkString ""
  }

  abstract class BaseExpr extends PrimitiveExpr {
    override def apply(params: Map[String, Any]): Any = {
      env update params
      apply()
    }
    override def apply(): Any = wrapExprInSelect(this)()
    override def close = {
      env.closeStatement
      childUpdates foreach { t => t._1.close }
    }
  }

  abstract class PrimitiveExpr extends Expr {
    def builder = QueryBuilder.this
  }

  private def registerChildUpdate(child: Expr, name: String) = {
    _childUpdatesBuildTime += {
      (child, if (name == null) s"_${_childUpdatesBuildTime.size + 1}" else name)
    }
  }

  private def wrapExprInSelect(expr: Expr) = {
    SelectExpr(
      List(Table(ConstExpr(Null), null, null, null, true, null)),
      null, ColsExpr(List(ColExpr(expr, null, Some(false))), false, false, false),
      false, null, null, null, null, Map(), None
    )
  }

  private def executeChildUpdates: Map[String, Any] = {
    def exec(name: String, e: Expr, pars: Option[Map[String, Any]]) =
      try pars.map(e(_)).getOrElse(e()) catch { case e: Exception =>
        throw new ChildSaveException(name, s"Error saving children - '$name'", e)
      }
    childUpdates.map {
      case (ex, n) if !env.contains(n) => (n, exec(n, ex, None))
      case (ex, n) => (n, env(n) match {
        case m: Map[String @unchecked, _] => exec(n, ex, Some(m))
        case t: Iterable[Map[String, _] @unchecked] => t.map(m => exec(n, ex, Some(m)))
        case a: Array[Map[String, _] @unchecked] => (a map {m => exec(n, ex, Some(m)) }).toList
        case null => Nil
        case x =>
          val bvt = Option(x).map(_.getClass.getName).orNull
          throw new ChildSaveException(n,
            s"Unexpected type for child query '$n' environment: '$bvt'. Expected map or sequence."
          )
      })
    }.foldLeft(scala.collection.immutable.ListMap[String, Any]()) {_ + _} //use list map to preserve children order
  }

  //default or fk shortcut join with child.
  //return value in form: (child query table col(s) -> parent query cols(s) alias(es))
  private def joinWithChild(childTable: String,
    refCol: Option[String]): Option[(List[String], List[String])] =
    refCol map { ref =>
      val refTable = env.table(childTable).refTable.getOrElse(List(ref),
        error("No referenced table found for table: " + childTable + ". Reference: " + ref))
      List(ref) -> env.table(refTable).key.cols -> findAliasByName(refTable)
        .getOrElse(error("Unable to find relationship between table " + childTable +
          ", reference column: " + ref + " and tables: " + this.tableDefs))
    } orElse (this.findJoin(childTable)).map(t=> ((t._1._1.cols, t._1._2.cols) -> t._2)) match {
      case None => None
      case Some(((k1: List[String], k2: List[String]), t)) =>
        this.joinsWithChildren += (t -> k2)
        Some(k1 -> k2.map(t + "_" + _ + "_"))
    }
  //default or fk shortcut join with parent
  private def joinWithParent(childTable: String, refCol: Option[String] = None) = env.provider.flatMap {
    case b: QueryBuilder => b.joinWithChild(childTable, refCol)
    case x => None
  }
  //join with ancestor query
  private def joinWithAncestor(ancestorTableAlias: String,
    ancestorTableCol: String): Option[ResExpr] = {
    if (!this.tableDefs.exists(_.alias == ancestorTableAlias))
      joinWithAncestor(ancestorTableAlias, ancestorTableCol, 1)
        .map(ResExpr(_, ancestorTableAlias + "_" + ancestorTableCol + "_"))
    else None
  }

  private def joinWithAncestor(ancestorTableAlias: String, ancestorTableCol: String,
      resIdx: Int): Option[Int] = env.provider.flatMap {
    case b: QueryBuilder =>
      b.joinWithDescendant(ancestorTableAlias, ancestorTableCol).map(_ => resIdx).orElse(
          b.joinWithAncestor(ancestorTableAlias, ancestorTableCol, resIdx + 1))
    case _ => None
  }
  private def joinWithDescendant(tableAlias: String, tableCol: String) = {
    this.tableDefs.find(tableAlias == _.alias).map { x =>
      this.joinsWithChildren += (tableAlias -> List(tableCol))
      x
    }
  }

  //DML statements are defined outside of buildInternal method since they are called from other QueryBuilder
  private def buildInsert(table: Ident, alias: String, cols: List[Col], vals: Exp,
                          returning: Option[Cols], ctx: Ctx) = {
    lazy val insertCols = this.table(table).cols.map(c => IdentExpr(List(c.name)))
    def resolveAsterisk(q: parsing.Query) =
      Option(q.cols)
        .map {
          _.cols.zipWithIndex.map { case (col, idx) =>
            Option(col.alias).map(n => IdentExpr(List(n.toLowerCase))).getOrElse {
              col.col match {
                case Obj(Ident(n), _, _, _, _) =>
                  IdentExpr(List(n.last.toLowerCase))
                case _ =>
                  sys.error(s"Bad column - '${col.tresql}' at index $idx. Please specify alias.")
              }
            }
          }
        }
        .getOrElse(insertCols)
    new InsertExpr(IdentExpr(table.ident), alias,
      cols match {
        //get column clause from metadata
        case Nil => insertCols
        case List(Col(All, _)) => vals match {
          case q: parsing.Query => resolveAsterisk(q)//adjust insertable columns to select columns
          case e =>
            def extractQuery(e: Exp): List[IdentExpr] = e match {
              case BinOp(_, lop, _) => extractQuery(lop)
              case q: parsing.Query => resolveAsterisk(q)
              case Braces(b) => extractQuery(b)
              case With(_, e) => extractQuery(e)
              case _ => insertCols
            }
            extractQuery(e)
        }
        case c => c map (buildInternal(_, COL_CTX)) filter {
          case x @ ColExpr(IdentExpr(_), _, _, _) => true
          case e: ColExpr => registerChildUpdate(e.col, e.name); false
          case _ => sys.error("Unexpected InsertExpr type")
        }
      }, vals match {
        case Values(arr) => ValuesExpr(arr map {
          buildInternal(_, VALUES_CTX) match {
            case ArrExpr(l) => ArrExpr(patchVals(table, cols, l))
            case null => ArrExpr(patchVals(table, cols, Nil))
            case e => e
          }
        } match {
          case Nil => patchVals(table, cols, Nil)
          case l => l
        })
        case q => buildInternal(q, VALUES_CTX)
      },
      returning.map(buildCols(_, ctx))
    )
  }
  private def buildUpdate(table: Ident, alias: String, filter: Arr, cols: List[Col], vals: Exp,
                          returning: Option[Cols], ctx: Ctx) = {
    new UpdateExpr(IdentExpr(table.ident), alias, if (filter != null)
      filter.elements map { buildInternal(_, WHERE_CTX) } else null, cols match {
        //get column clause from metadata
        case Nil => this.table(table).cols.map(c => IdentExpr(List(c.name)))
        case c => c map (buildInternal(_, COL_CTX)) filter {
          case ColExpr(IdentExpr(_), _, _, _) => true
          case ColExpr(BinExpr("=", _, _), _, _, _) => true //update from select
          case e: ColExpr => registerChildUpdate(e.col, e.name); false
          case _ => sys.error("Unexpected UpdateExpr type")
        }
      }, buildInternal(vals, VALUES_CTX) match {
        case v: ArrExpr => ValuesExpr(patchVals(table, cols, v.elements))
        case q: SelectExpr => q
        case f: ValuesFromSelectExpr => f
        case null => ValuesExpr(patchVals(table, cols, Nil))
        case x => error("Knipis: " + x)
      },
      returning.map(buildCols(_, ctx))
    )
  }

  private def buildDelete(table: Ident, alias: String, filter: Arr, using: Exp, returning: Option[Cols],
                          ctx: Ctx) = {
    new DeleteExpr(IdentExpr(table.ident), alias,
      if (filter != null) filter.elements map { buildInternal(_, WHERE_CTX) } else null,
      buildInternal(using, VALUES_CTX),
      returning.map(buildCols(_, ctx))
    )
  }

  private def table(t: Ident) = env.table(t.ident.mkString("."))

  private def patchVals(table: Ident, cols: List[Col], vals: List[Expr]) = {
    val diff = (if (cols.isEmpty) this.table(table).cols else cols).size - vals.size
    val allExprIdx = vals.indexWhere(_.isInstanceOf[AllExpr])
    def v(i: Int) = buildInternal(Variable("?", null, false), VALUES_CTX)
    if (diff > 0 || allExprIdx != -1) allExprIdx match {
      case -1 if vals.isEmpty => 1 to diff map v toList //empty value clause
      case -1 => vals //perhaps hierarchical update
      case i => vals.patch(i, 0 to diff map v, 1)
    }
    else vals
  }

  private def buildArray(a: Arr, ctx: Ctx = ARR_CTX) = a.elements
    .map { buildInternal(_, ctx) } filter (_ != null) match {
      case al if al.nonEmpty => ArrExpr(al) case _ => null
    }

  /* method is used for both select expression and dml returning column build */
  private def buildCols(cols: Cols, ctx: Ctx): ColsExpr =
    if (cols == null)
      ColsExpr(List(ColExpr(AllExpr(), null, Some(false))), hasAll = true, hasIdentAll = false, hasHidden = false)
    else {
      var (hasAll, hasIdentAll, hasHidden) = (false, false, false)
      val colExprs = cols.cols.map(buildInternal(_, COL_CTX) match {
        case c @ ColExpr(_: AllExpr, _, _, _) =>
          hasAll = true
          c
        case c @ ColExpr(_: IdentAllExpr, _, _, _) =>
          hasIdentAll = true
          c
        case c: ColExpr => c
        case x => sys.error(s"ColExpr expected instead found: $x")
      })
      val colsWithLinksToChildren =
        colExprs ++
        //for top level queries add hidden columns used in filters of descendant queries
        (if (ctx == QUERY_CTX) {
          hasHidden |= joinsWithChildrenColExprs.nonEmpty
          joinsWithChildrenColExprs
        } else Nil)
      ColsExpr(colsWithLinksToChildren, hasAll, hasIdentAll, hasHidden)
    }

  private[tresql] def buildInternal(parsedExpr: Exp, parseCtx: Ctx = QUERY_CTX): Expr = {
    def buildSelect(q: parsing.Query, ctx: Ctx) = {
      val tablesAndAliases = buildTables(q.tables)
      if (ctx == QUERY_CTX && this.tableDefs == Nil) this.tableDefs = defs(tablesAndAliases._1)
      val filter = if (q.filter == null) null else buildFilter(tablesAndAliases._1.last, q.filter.filters)
      val cols = buildCols(q.cols, ctx)
      val distinct = q.cols != null && q.cols.distinct
      val group = buildInternal(q.group, GROUP_CTX)
      val order = buildInternal(q.order, ORD_CTX)
      val offset = buildInternal(q.offset, LIMIT_CTX)
      val limit = buildInternal(q.limit, LIMIT_CTX)
      val aliases = tablesAndAliases._2
      //check whether parent query with at least one primitive table exists
      def hasParentQuery = env.provider.exists {
        case b: QueryBuilder => b.tableDefs.nonEmpty
        case _ => false
      }
      //establish link with ancestors
      val parentJoin =
        if (ctx != QUERY_CTX || !hasParentQuery) None else {
          def parentChildJoinExpr(table: Table, refCol: Option[String] = None) = {
            def exp(childCol: String, parentCol: String) = BinExpr("=",
              IdentExpr(List(table.aliasOrName, childCol)), ResExpr(1, parentCol))
            def exps(cols: List[(String, String)]): Expr = cols match {
              case c :: Nil => exp(c._1, c._2)
              case c :: l => BinExpr("&", exp(c._1, c._2), exps(l))
              case _ => sys.error("Unexpected cols type")
            }
            joinWithParent(table.tableNameWithSchema, refCol).map(t => exps(t._1 zip t._2))
          }
          tablesAndAliases._1.headOption.flatMap {
            //default join
            case tb @ Table(IdentExpr(t), _, null, _, _, _) =>
              parentChildJoinExpr(tb)
            case tb @ Table(IdentExpr(t), _, TableJoin(true, null, _, _), _, _, _) =>
              parentChildJoinExpr(tb)
            //foreign key join shortcut syntax
            case tb @ Table(IdentExpr(t), _, TableJoin(false, IdentExpr(fk), _, _), _, _, _) =>
              parentChildJoinExpr(tb, fk.lastOption)
            //product join, i.e. no join
            case Table(_, _, TableJoin(false, ArrExpr(Nil) | null, _, _), _, _, _) => None
            //ancestor join
            //transform ancestor reference: replace IdentExpr referencing parent queries with ResExpr
            case tb @ Table(_, _, TableJoin(false, e, _, _), _, _, _) if e != null =>
              Some(transform(e, {
                case ie @ IdentExpr(List(tab, col)) =>
                  joinWithAncestor(tab, col).getOrElse(ie)
              }))
            case x => error(s"Cannot join with parent, unrecognized table: $x")
          }
        }
      val sel = SelectExpr(tablesAndAliases._1, filter, cols, distinct, group, order, offset, limit,
        tablesAndAliases._2, parentJoin)
      //if select expression is subquery in other's expression where clause, has where clause itself
      //but where clause was removed due to unbound optional variables remove subquery itself
      @tailrec
      def isWhere(cstack: List[Ctx]): Boolean = cstack match {
        case WHERE_CTX :: _ => true
        case x :: _ if x != FUN_CTX => false
        case Nil => false
        case FUN_CTX :: tail => isWhere(tail)
        case _ => false
      }
      if ((ctx == WHERE_CTX || isWhere(ctxStack)) && q.filter.filters != Nil && sel.filter == null) null else sel
    }
    def buildSelectFromObj(o: Obj, ctx: Ctx) =
      buildSelect(parsing.Query(List(o), Filters(Nil), null, null, null, null, null), ctx)
    //build tables, set nullable flag for tables right to default join or foreign key shortcut join,
    //which is used for implicit left outer join, create aliases map
    def buildTables(tables: List[Obj]) = {
      (tables map buildTable).foldLeft((List[Table](), Map[String, Table](), Map[String, String]())) {
        case ((ts, aliases, schemas), t) =>
          val (tbl_with_sch, new_schemas) = t match {
            case Table(IdentExpr(n), _, _, _, _, _) =>
              schemas.get(n mkString ".")
                .map(sch => (t.copy(schema = sch), schemas))
                .getOrElse {
                  (t, if (n.size > 1) schemas + (n.last -> n.dropRight(1).mkString(".")) else schemas)
                }
            case _ => (t, schemas)
          }
          val nt = ts.headOption.map((_, tbl_with_sch)).map {
            case (pt @ Table(IdentExpr(ptn), ptna, _, _, _, _), Table(_: IdentExpr, _, j, null, false, _)) =>
              //get previous table from aliases map if exists
              val prevTable = aliases.getOrElse(ptn.mkString("."), pt)
              j match {
                //foreign key of shortcut join must come from previous table i.e. ptn
                case TableJoin(false, IdentExpr(fk), _, _) if fk.size == 1 ||
                  fk.dropRight(1) == (if (ptna == null) ptn else List(ptna)) =>
                  env.colOption(prevTable.tableNameWithSchema, fk.last).map(col =>
                    //set current table to nullable if foreign key column or previous table is nullable
                    if (prevTable.nullable || col.nullable) tbl_with_sch.copy(nullable = true)
                    else tbl_with_sch).getOrElse(tbl_with_sch)
                //default join
                case TableJoin(true, _, _, _) =>
                  val dj = env.join(prevTable.tableNameWithSchema, tbl_with_sch.tableNameWithSchema)
                  if (prevTable.nullable
                      || dj._2.isInstanceOf[metadata.fk] // (uk, fk) or (fk, fk) then nullable
                      || dj._1.cols.exists(env.col(prevTable.tableNameWithSchema, _).nullable))
                    tbl_with_sch.copy(join = tbl_with_sch.join.copy(defaultJoinCols = dj), nullable = true)
                  else tbl_with_sch.copy(join = tbl_with_sch.join.copy(defaultJoinCols = dj))
                case _ => tbl_with_sch
              }
            case _ => tbl_with_sch
          }.getOrElse(tbl_with_sch)

          ( nt :: ts
          , if (nt.alias != null && !aliases.contains(nt.alias)) aliases + (nt.alias -> nt) else aliases
          , new_schemas
          )
      } match {
        case (tables: List[Table], aliases: Map[String, Table], _) => (tables.reverse, aliases)
      }
    }
    def buildTable(t: Obj) = Table(buildInternal(t.obj, FROM_CTX), t.alias,
      if (t.join != null) TableJoin(t.join.default, buildInternal(t.join.expr, JOIN_CTX),
        t.join.noJoin, null)
      else null, t.outerJoin, t.nullable, null)
    def buildIdentOrBracesExpr(i: Obj) = i match {
      case Obj(Ident(i), null, _, _, _) => IdentExpr(i)
      case Obj(b @ Braces(_), _, _, _, _) => buildInternal(b, parseCtx)
      case o => error("unsupported expression at this place: " + o)
    }
    def buildFilter(pkTable: Table, filterList: List[Arr]): Expr = {
      def transformExpr: PartialFunction[Expr, Expr] = {
        case ArrExpr(List(b @ ConstExpr(true | false))) => b
        case a @ ArrExpr(List(_: ConstExpr | _: VarExpr | _: ResExpr)) => BinExpr("=",
          IdentExpr(List(pkTable.aliasOrName, env.tableOption(pkTable.name)
            .getOrElse(error("Table not found in primary key search: " + pkTable.name))
            .key.cols.head)), a.elements.head)
        case a: ArrExpr if a.elements.size > 1 => InExpr(IdentExpr(List(pkTable.aliasOrName,
          env.table(pkTable.name).key.cols.head)), a.elements, false)
        case ArrExpr(List(f)) => f
        case null => null
      }
      def filterExprArr(fl: List[Arr]): Expr = fl match {
        case Nil => null
        case a :: t => (transformExpr(buildInternal(a, WHERE_CTX)), filterExprArr(t)) match { case (x, y) =>
          def b(e: Expr) = e match { case _: BracesExpr => e case _ => BracesExpr(e) }
          if (x == null) y else if (y == null) b(x) else BinExpr("&", b(x), y)
        }
      }
      filterList match {
        case f :: Nil => transformExpr(buildInternal(f, WHERE_CTX))
        case l => filterExprArr(l)
      }
    }
    def maybeCallMacro(exp: Expr) = {
      var varSet: Set[QueryBuilder#BaseVarExpr] = Set()
      val expb = exp.builder
      expb.transform(
        exp match {
          case fun: QueryBuilder#FunExpr =>
            if (env isBuilderMacroDefined fun.name) {
              env.invokeBuilderMacro(fun.name, expb, fun.params)
            } else if (fun.params.contains(null)) null else fun
          case binExpr: QueryBuilder#BinExpr =>
            if (!(STANDART_BIN_OPS contains binExpr.op)) {
              val macroName = scala.reflect.NameTransformer.encode(binExpr.op)
              if (env isBuilderMacroDefined macroName)
                env.invokeBuilderMacro(macroName, expb, List(binExpr.lop, binExpr.rop))
              else binExpr
            } else binExpr
          case _ => sys.error("Unexpected exp type")
        }, {
          case v: QueryBuilder#BaseVarExpr =>
            if (varSet(v)) {
              v match {
                case ve: QueryBuilder#VarExpr => ve.copy()
                case ie: QueryBuilder#IdExpr => ie.copy()
                case re: QueryBuilder#IdRefExpr => re.copy()
                case res: QueryBuilder#ResExpr => res.copy()
                case x => x
              }
            } else {
              varSet += v
              v
            }
        }
      )
    }

    def buildWithNew(db: Option[String], buildFunc: QueryBuilder => Expr) = {
      val b = newInstance(
        new Env(QueryBuilder.this, db, QueryBuilder.this.env.reusableExpr),
        queryDepth + 1, bindIdx, this.childrenCount)
      val ex = buildFunc(b)
      this.separateQueryFlag = true
      this.bindIdx = b.bindIdx
      this.childrenCount += 1
      ex
    }

    ctxStack ::= parseCtx
    try {
      parsedExpr match {
        case Const(x) => ConstExpr(x)
        case Sql(sqlStr) =>
          //fallback to sql macro
          buildInternal(Fun("sql", List(Const(sqlStr)),false,None,None) , parseCtx)
        case Null => ConstExpr(Null)
        case NullUpdate => ConstExpr(NullUpdate)
        //insert
        case i @ Insert(t, a, c, v, r, db) => parseCtx match {
          case ARR_CTX | COL_CTX | FUN_CTX => buildWithNew(db, _.buildInternal(i, DML_CTX))
          case QUERY_CTX if db.nonEmpty => buildWithNew(db, _.buildInternal(i, DML_CTX))
          case _ => buildInsert(t, a, c, v, r, parseCtx)
        }
        //update
        case u @ Update(t, a, f, c, v, r, db) => parseCtx match {
          case ARR_CTX | COL_CTX | FUN_CTX => buildWithNew(db, _.buildInternal(u, DML_CTX))
          case QUERY_CTX if db.nonEmpty => buildWithNew(db, _.buildInternal(u, DML_CTX))
          case _ => buildUpdate(t, a, f, c, v, r, parseCtx)
        }
        //delete
        case d @ Delete(t, a, f, u, r, db) => parseCtx match {
          case ARR_CTX | COL_CTX | FUN_CTX => buildWithNew(db, _.buildInternal(d, DML_CTX))
          case QUERY_CTX if db.nonEmpty => buildWithNew(db, _.buildInternal(d, DML_CTX))
          case _ => buildDelete(t, a, f, u, r, parseCtx)
        }
        //recursive child query
        case ChildQuery(join: Arr, db) =>
          if (recursiveQueryExp != null) {
            val e = RecursiveExpr({
              val t = recursiveQueryExp.tables
              //copy join condition in the right place for parent child join calculation
              recursiveQueryExp.copy(
                tables = t.head.copy(
                  join = Join(default = false, join, noJoin = false)) :: t.tail,
                //drop first filter for recursive query, since first filter is used to obtain initial set
                filter = recursiveQueryExp.filter.copy(
                  filters = recursiveQueryExp.filter.filters drop 1))
            })
            this.separateQueryFlag = true
            this.childrenCount += 1
            e
        } else null
        //child query
        case ChildQuery(q, db) => buildWithNew(db, _.buildInternal(q, QUERY_CTX))
        case t: Obj => parseCtx match {
          case ARR_CTX =>
            buildWithNew(None, _.buildInternal(t, QUERY_CTX)) //may have other elements in array
          //table in from clause of top level query or in any other subquery
          case QUERY_CTX | FROM_CTX | WITH_CTX | WITH_TABLE_CTX => buildSelectFromObj(t, parseCtx)
          case _ => buildIdentOrBracesExpr(t)
        }
        case q: parsing.Query => parseCtx match {
          case ARR_CTX => buildWithNew(None, _.buildInternal(q, QUERY_CTX)) //may have other elements in array
          case _ =>
            if (recursiveQueryExp == null) recursiveQueryExp = q //set for potential use in RecursiveExpr
            buildSelect(q, parseCtx)
        }
        case wq @ With(tables, query) => parseCtx match {
          case ARR_CTX => buildWithNew(None, _.buildInternal(wq, QUERY_CTX)) //may have other elements in array
          case _ =>
            val withTables = tables.map(buildInternal(_, parseCtx).asInstanceOf[WithTableExpr])
            buildInternal(query, if (parseCtx == QUERY_CTX) QUERY_CTX else WITH_CTX) match {
              case s: SelectExpr => WithSelectExpr(withTables, s)
              case b: BinExpr if b.exprType == classOf[SelectExpr] => WithBinExpr(withTables, b)
              case i: InsertExpr => new WithInsertExpr(withTables, i)
              case u: UpdateExpr => new WithUpdateExpr(withTables, u)
              case d: DeleteExpr => new WithDeleteExpr(withTables, d)
              case x => sys.error(s"""Currently unsupported after "WITH" query: `${query.tresql}`, ${x.getClass}""")
            }
        }
        case WithTable(name, cols, recursive, query) =>
          WithTableExpr(name, cols, recursive, buildInternal(query, WITH_TABLE_CTX))
        case UnOp(op, oper) =>
          val o = buildInternal(oper, parseCtx)
          if (o == null) null else UnExpr(op, o)
        case c @ Cast(exp, typ) => parseCtx match {
          case ARR_CTX => buildWithNew(None, _.buildInternal(c, QUERY_CTX))
          case ctx => buildInternal(exp, ctx) match {
            case null => null
            case e => CastExpr(e, typ)
          }
        }
        case e @ BinOp(op, lop, rop) => parseCtx match {
          case ARR_CTX if op != "=" /*do not create new query builder for assignment or equals operation*/=>
            buildWithNew(None, _.buildInternal(e, QUERY_CTX))
          case ctx =>
            val l = buildInternal(lop, ctx)
            val r = buildInternal(rop, ctx)
            if (l != null && r != null) maybeCallMacro(BinExpr(op, l, r))
            else if (OPTIONAL_OPERAND_BIN_OPS contains op)
              if (l != null) l else if (r != null) r else null
            else null
        }
        case t: TerOp => buildInternal(t.content, parseCtx)
        case in @ In(lop, rop, not) => parseCtx match {
          case ARR_CTX => buildWithNew(None, _.buildInternal(in, QUERY_CTX))
          case _ =>
            val l = buildInternal(lop, parseCtx)
            if (l == null) null else {
              val r = rop.map(buildInternal(_, parseCtx)).filter(_ != null)
              if (r.isEmpty) null else InExpr(l, r, not)
            }
        }
        case fun @ Fun(n, pl: List[_], d, o, f) => parseCtx match  {
          case ARR_CTX => buildWithNew(None, _.buildInternal(fun, FUN_CTX))
          case _ =>
            val pars = pl map { buildInternal(_, FUN_CTX) }
            val order = o.map(buildInternal(_, FUN_CTX))
            val filter = f.map(buildInternal(_, FUN_CTX))
            maybeCallMacro(FunExpr(n, pars, d, order, filter))
        }
        case FunAsTable(f, cds, ord) =>
          FunAsTableExpr(buildInternal(f, parseCtx)
            , cds.map(_.map(c => TableColDefExpr(c.name, c.typ))), ord)
        case Ident(i) => IdentExpr(i)
        case IdentAll(i) => IdentAllExpr(i.ident)
        case a: Arr => parseCtx match {
          case ARR_CTX => buildWithNew(None, _.buildArray(a))
          case QUERY_CTX => buildArray(a)
          case ctx => buildArray(a, ctx)
        }
        case Variable("?", _, o) =>
          this.bindIdx += 1; VarExpr(this.bindIdx.toString, Nil, o)
        case Variable(n, m, o) =>
          if (!env.reusableExpr && o && !(env.contains(n, m))) null else VarExpr(n, m, o)
        case Id(seq) => IdExpr(seq)
        case IdRef(seq) => IdRefExpr(seq)
        case Res(r, c) => ResExpr(r, c)
        case Col(c, a) =>
          separateQueryFlag = false
          val ce = ColExpr(buildInternal(c, parseCtx), a)
          separateQueryFlag = false
          ce
        case Grp(cols, having) => Group(cols map { buildInternal(_, GROUP_CTX) },
          buildInternal(having, HAVING_CTX))
        case Ord(cols) => Order(cols map (c=> (buildInternal(c._1, parseCtx),
            buildInternal(c._2, parseCtx), buildInternal(c._3, parseCtx))))
        case All => AllExpr()
        case ValuesFromSelect(sel) => ValuesFromSelectExpr(buildInternal(sel, FROM_CTX).asInstanceOf[SelectExpr])
        case Braces(expr) =>
          val e = buildInternal(expr, parseCtx)
          if (e == null) null else BracesExpr(e)
        case null => null
        case x => ConstExpr(x)
      }
    } finally ctxStack = ctxStack.tail
  }

  def buildExpr(ex: String): Expr = buildExpr(new QueryParser(env, env.cache).parseExp(ex))
  def buildExpr(ex: Exp): Expr = buildInternal(ex, ctxStack.headOption.getOrElse(QUERY_CTX))

  //for debugging purposes
  def printBuilderChain: Unit = {
    println(s"$this#${envId()}")
    env.provider.foreach {
      case b: QueryBuilder => b.printBuilderChain
      case _ =>
    }
  }
  //for debugging purposes
  def envId() = s"$queryDepth#${System.identityHashCode(this)}"

}

sealed abstract class Expr extends (() => Any) {
  def defaultSQL: String
  def builder: QueryBuilder
  def sql: String = if (builder.env.dialect != null) builder.env.dialect(this) else defaultSQL

  def apply(): Any = ???
  def apply(params: Map[String, Any]): Any = ???
  def close: Unit = ???

  def apply(params: Seq[Any]): Any = apply(org.tresql.Query.normalizePars(params))
  def exprType: Class[_] = this.getClass
  //overrides function toString method since it is of no use
  override def toString = defaultSQL
}
