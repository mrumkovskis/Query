package org.tresql

import java.sql.{CallableStatement, PreparedStatement, ResultSet, SQLException}
import CoreTypes.RowConverter
import org.tresql.ast.Exp
import org.tresql.metadata.TypeMapper

import scala.annotation.tailrec

trait Query extends QueryBuilder with TypedQuery {

  def apply(expr: String, params: Any*)(implicit resources: Resources): DynamicResult =
    exec(expr, normalizePars(params: _*), resources).asInstanceOf[DynamicResult]

  def compiledResult[T <: RowLike](expr: String, params: Any*)(
    implicit resources: Resources): Result[T] =
    exec(expr, normalizePars(params: _*), resources).asInstanceOf[Result[T]]

  private[tresql] def converters: Map[List[Int], RowConverter[_ <: RowLike]] = null

  private def exec(
    expr: String,
    params: Map[String, Any],
    resources: Resources
  ): Result[_ <: RowLike] = {
    val builtExpr = build(expr, params, false)(resources)
    if (builtExpr == null) SingleValueResult(null) else {
      builtExpr() match {
        case r: Result[_] => r
        case x  => SingleValueResult(x)
      }
    }
  }

  def build(
    expr: String,
    params: Map[String, Any] = null,
    reusableExpr: Boolean = true
  )(implicit resources: Resources): Expr = {
    require(resources != null, "Resources cannot be null.")
    resources.log(expr, Nil, LogTopic.tresql)
    val pars =
      if (resources.params.isEmpty) params
      else if (params != null) resources.params ++ params else resources.params
    newInstance(new Env(pars, resources, reusableExpr), 0, 0).buildExpr(expr)
  }

  def buildFromAst(
    expr: Exp,
    params: Map[String, Any] = null,
    reusableExpr: Boolean = true
  )(implicit resources: Resources): Expr = {
    require(resources != null, "Resources cannot be null.")
    val pars =
      if (resources.params.isEmpty) params
      else if (params != null) resources.params ++ params else resources.params
    newInstance(new Env(pars, resources, reusableExpr), 0, 0).buildExpr(expr)
  }

  /** QueryBuilder methods **/
  private[tresql] def newInstance(
    e: Env,
    idx: Int,
    chIdx: Int
  ) = {
    if (converters != null) e.rowConverters = converters
    val qpos = chIdx :: queryPos
    new Query {
      override def env = e
      override private[tresql] def queryPos = qpos
      override private[tresql] var bindIdx = idx
    }
  }

  private[tresql] def normalizePars(pars: Any*): Map[String, Any] = pars match {
    case Seq(m: Map[String @unchecked, Any @unchecked]) => m
    case l => l.zipWithIndex.map { case (v, k) => (k + 1).toString -> v }.toMap
  }

  private[tresql] def sel(sql: String, cols: QueryBuilder#ColsExpr): Result[_ <: RowLike] = try {
    val (rs, columns, visibleColCount) = sel_result(sql, cols)
    val result = env.rowConverter(queryPos).map { conv =>
      new CompiledSelectResult(rs, columns, env, sql,registeredBindVariables,
        env.maxResultSize, visibleColCount, conv)
    }.getOrElse {
      new DynamicSelectResult(rs, columns, env, sql, registeredBindVariables, env.maxResultSize, visibleColCount)
    }
    env.result = result
    result
  } catch {
    case ex: SQLException =>
      throw new TresqlException(sql, bindVarsValues(registeredBindVariables), ex)
  }

  private[this] def sel_result(sql: String, cols: QueryBuilder#ColsExpr):
    (ResultSet, Vector[Column], Int) = { //jdbc result, columns, visible column count
    val st = statement(sql, env)
    var i = 0
    val rs = st.executeQuery
    val md = rs.getMetaData
    var visibleColCount = -1
    def jdbcRcols = (1 to md.getColumnCount).foldLeft(List[Column]()) {
        (l, j) => i += 1; Column(i, md.getColumnLabel(j), null) :: l
      } reverse
    def rcol(c: QueryBuilder#ColExpr) = if (c.separateQuery) Column(-1, c.name, c.col) else {
      i += 1; Column(i, c.name, null)
    }
    def rcols = if (cols.hasHidden) {
      val res = cols.cols.zipWithIndex.foldLeft((List[Column](), Map[Expr, Int](), 0)) {
        (r, c) => (rcol(c._1) :: r._1,
            if (c._1.hidden) r._2 + (c._1.col -> c._2) else r._2,
            if (!c._1.hidden) r._3 + 1 else r._3)
      }
      env.updateExprs(res._2)
      (res._1.reverse, res._3)
    } else (cols.cols map rcol, -1)
    val columns =
      if (cols.hasAll) Vector(cols.cols.flatMap { c =>
        if (c.col.isInstanceOf[QueryBuilder#AllExpr]) jdbcRcols else List(rcol(c))
      }: _*)
      else if (cols.hasIdentAll) Vector(jdbcRcols ++ (cols.cols.filter(_.separateQuery) map rcol) :_*)
      else rcols match {
        case (c, -1) => Vector(c: _*)
        case (c, s) =>
          visibleColCount = s
          Vector(c: _*)
      }
    (rs, columns, visibleColCount)
  }

  private[tresql] def update(sql: String) = try {
    val st = statement(sql, env)
    try {
      st.executeUpdate
    } finally if (!env.reusableExpr) {
      st.close
      env.statement = null
    }
  } catch {
    case ex: SQLException =>
      throw new TresqlException(sql, bindVarsValues(registeredBindVariables), ex)
  }

  private[tresql] def call(sql: String): Result[RowLike] = try {
    val st = statement(sql, env, true).asInstanceOf[CallableStatement]
    var result: Result[RowLike] = null
    var outs: List[Any] = null
    try {
      if (st.execute) {
        val rs = st.getResultSet
        val md = rs.getMetaData
        val res = env.rowConverter(queryPos).map { conv =>
          new CompiledSelectResult(
            rs,
            Vector(1 to md.getColumnCount map { i => Column(i, md.getColumnLabel(i), null) }: _*),
            env,
            sql,
            registeredBindVariables,
            env.maxResultSize,
            -1,
            conv)
        }.getOrElse {
          new DynamicSelectResult(
            rs,
            Vector(1 to md.getColumnCount map { i => Column(i, md.getColumnLabel(i), null) }: _*),
            env,
            sql,
            registeredBindVariables,
            env.maxResultSize
          )
        }
        env.result = res
        result = res
      }
      outs = registeredBindVariables map (_()) collect { case x: OutPar =>
        val p = x.asInstanceOf[OutPar]
        p.value = p.value match {
          case null => st.getObject(p.idx)
          case i: Int =>
            val x = st.getInt(p.idx); if (st.wasNull) null else x
          case l: Long =>
            val x = st.getLong(p.idx); if (st.wasNull) null else x
          case d: Double =>
            val x = st.getDouble(p.idx); if (st.wasNull) null else x
          case f: Float =>
            val x = st.getFloat(p.idx); if (st.wasNull) null else x
          // Allow the user to specify how they want the Date handled based on the input type
          case t: java.sql.Timestamp => st.getTimestamp(p.idx)
          case d: java.sql.Date => st.getDate(p.idx)
          case t: java.sql.Time => st.getTime(p.idx)
          /* java.util.Date has to go last, since the java.sql date/time classes subclass it. By default we
* assume a Timestamp value */
          case d: java.util.Date => st.getTimestamp(p.idx)
          case b: Boolean => st.getBoolean(p.idx)
          case s: String => st.getString(p.idx)
          case bn: java.math.BigDecimal => st.getBigDecimal(p.idx)
          case bd: BigDecimal => val x = st.getBigDecimal(p.idx); if (st.wasNull) null else BigDecimal(x)
        }
        p.value
      }
    } finally if (result == null && !env.reusableExpr) {
      st.close
      env.statement = null
    }
    if (outs.isEmpty) result
    else env.rowConverter(queryPos).map { conv =>
      new CompiledArrayResult(if (result== null) outs else result :: outs, conv)
    }.getOrElse(new DynamicArrayResult(if (result== null) outs else result :: outs))
  } catch {
    case ex: SQLException =>
      throw new TresqlException(sql, bindVarsValues(registeredBindVariables), ex)
  }

  private def statement(sql: String, env: Env, call: Boolean = false) = {
    log(sql, registeredBindVariables)
    val conn = env.conn
    if (conn == null) throw new NullPointerException(
      """Connection not found in environment.""")
    val st = if (env.reusableExpr)
      if (env.statement == null) {
        val s = if (call) conn.prepareCall(sql) else {
          conn.prepareStatement(sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
        }
        env.statement = s
        s
      } else env.statement
    else if (call) conn.prepareCall(sql)
    else conn.prepareStatement(sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
    if (env.queryTimeout > 0) st.setQueryTimeout(env.queryTimeout)
    if (env.fetchSize > 0) st.setFetchSize(env.fetchSize)

    val bindValues = registeredBindVariables.map {
      case v: QueryBuilder#VarExpr  => (v.allowArrBind, v())
      case e                        => (false,          e())
    }
    bindVars(st, bindValues)
    st
  }

  private def bindVars(st: PreparedStatement, bindVariables: List[(Boolean, Any)]) = {
    var idx = 1
    def bindVar(allowArrBind: Boolean, p: Any): Unit = {
      try p match {
        case null => st.setNull(idx, java.sql.Types.NULL)
        case i: Int => st.setInt(idx, i)
        case l: Long => st.setLong(idx, l)
        case d: Double => st.setDouble(idx, d)
        case f: Float => st.setFloat(idx, f)
        case i: java.lang.Integer => st.setInt(idx, i)
        case l: java.lang.Long => st.setLong(idx, l)
        case d: java.lang.Double => st.setDouble(idx, d)
        case f: java.lang.Float => st.setFloat(idx, f)
        // Allow the user to specify how they want the Date handled based on the input type
        case t: java.sql.Timestamp => st.setTimestamp(idx, t)
        case d: java.sql.Date => st.setDate(idx, d)
        case t: java.sql.Time => st.setTime(idx, t)
        /* java.util.Date has to go last, since the java.sql date/time classes subclass it. By default we
* assume a java.sql.Date value */
        case d: java.util.Date => st.setTimestamp(idx, new java.sql.Timestamp(d.getTime))
        case c: java.util.Calendar => st.setTimestamp(idx, new java.sql.Timestamp(c.getTime.getTime))
        case d: java.time.LocalDate => st.setDate(idx, java.sql.Date.valueOf(d))
        case t: java.time.LocalDateTime => st.setTimestamp(idx, java.sql.Timestamp.valueOf(t))
        case t: java.time.LocalTime => st.setTime(idx, java.sql.Time.valueOf(t))
        case b: Boolean => st.setBoolean(idx, b)
        case b: java.lang.Boolean => st.setBoolean(idx, b)
        case s: String => st.setString(idx, s)
        case bn: java.math.BigDecimal => st.setBigDecimal(idx, bn)
        case bd: BigDecimal => st.setBigDecimal(idx, bd.bigDecimal)
        case bi: java.math.BigInteger => st.setBigDecimal(idx, new java.math.BigDecimal(bi))
        case bi: BigInt => st.setBigDecimal(idx, new java.math.BigDecimal(bi.bigInteger))
        case in: java.io.InputStream => st.setBinaryStream(idx, in)
        case bl: java.sql.Blob => st.setBlob(idx, bl)
        case rd: java.io.Reader => st.setCharacterStream(idx, rd)
        case cl: java.sql.Clob => st.setClob(idx, cl)
        case ab: Array[Byte] => st.setBytes(idx, ab)
        case ar: java.sql.Array => st.setArray(idx, ar)
        //array binding
        case a: Array[_] =>
          if (allowArrBind) bindColl(a.toSeq)
          else bindArr(st, a, idx) // unable to construct sql array since type is unknown
        case o: Option[_] =>
          bindVar(allowArrBind, o.orNull)
          idx -= 1
        case i: scala.collection.Iterable[_] =>
          if (allowArrBind) bindColl(i)
          else st.setString(idx, toJsonString(i)) // convert complex object to json string for not to fail
        case p@InOutPar(v) =>
          bindVar(allowArrBind, v)
          idx -= 1
          registerOutPar(st.asInstanceOf[CallableStatement], p, idx)
        //OutPar must be matched bellow InOutPar since it is superclass of InOutPar
        case p@OutPar(_) => registerOutPar(st.asInstanceOf[CallableStatement], p, idx)
        //unknown object
        case obj => st.setObject(idx, obj)
      } catch {
        case e:Exception => throw new RuntimeException("Failed to bind variable at index " +
            (idx - 1) + ". Value: " + (String.valueOf(p) match {
              case x if x.length > 100 => x.substring(0, 100) + "..."
              case x => x
            }) + " of class " + (if (p == null) "null" else p.getClass),e)
      }
      idx += 1
    }
    def bindColl(i: Iterable[_]) = {
      i.foreach(bindVar(true, _))
      idx -= 1
    }
    def bindArr(st: java.sql.PreparedStatement, value: Array[_], idx: Int) = {
      def normalizeArr(arr: Array[_]) = {
        def n[T](a: Array[T], m: T => Object): Array[Object] = a map { case null => null case x => m(x) }
        arr match {
          case a: Array[BigDecimal] => n[BigDecimal](a, _.bigDecimal)
          case a: Array[BigInt] => n[BigInt](a, _.bigInteger)
          case a: Array[java.util.Date] => n[java.util.Date](a, d => new java.sql.Timestamp(d.getTime))
          case a: Array[java.util.Calendar] => n[java.util.Calendar](a, c => new java.sql.Timestamp(c.getTime.getTime))
          case a: Array[java.time.LocalDate] => n[java.time.LocalDate](a, java.sql.Date.valueOf)
          case a: Array[java.time.LocalDateTime] => n[java.time.LocalDateTime](a, java.sql.Timestamp.valueOf)
          case a: Array[java.time.LocalTime] => n[java.time.LocalTime](a, java.sql.Time.valueOf)
          case a => a.asInstanceOf[Array[Object]]
        }
      }
      val conn = st.getConnection
      val vendor = env.dialect(SQLVendorExpr())
      val arrayScalaType = Manifest.classType(value.getClass.getComponentType).toString
      val arraySqlType = env.metadata
        .to_sql_type(vendor, env.metadata.from_jdbc_type(TypeMapper.scalaToJdbc(arrayScalaType)))
      val bv = conn.createArrayOf(arraySqlType, normalizeArr(value))
      st.setArray(idx, bv)
    }

    bindVariables.foreach { case (arrb, v) => bindVar(arrb, v) }
  }

  private def registerOutPar(st: CallableStatement, par: OutPar, idx: Int) = {
    import java.sql.Types._
    par.idx = idx
    par.value match {
      case null => st.registerOutParameter(idx, NULL)
      case i: Int => st.registerOutParameter(idx, INTEGER)
      case l: Long => st.registerOutParameter(idx, BIGINT)
      case d: Double => st.registerOutParameter(idx, DECIMAL)
      case f: Float => st.registerOutParameter(idx, DECIMAL)
      case i: java.lang.Integer => st.registerOutParameter(idx, INTEGER)
      case l: java.lang.Long => st.registerOutParameter(idx, BIGINT)
      case d: java.lang.Double => st.registerOutParameter(idx, DECIMAL)
      case f: java.lang.Float => st.registerOutParameter(idx, DECIMAL)
      // Allow the user to specify how they want the Date handled based on the input type
      case t: java.sql.Timestamp => st.registerOutParameter(idx, TIMESTAMP)
      case d: java.sql.Date => st.registerOutParameter(idx, DATE)
      case t: java.sql.Time => st.registerOutParameter(idx, TIME)
      /* java.util.Date has to go last, since the java.sql date/time classes subclass it. By default we
* assume a Timestamp value */
      case d: java.util.Date => st.registerOutParameter(idx, TIMESTAMP)
      case b: Boolean => st.registerOutParameter(idx, BOOLEAN)
      case b: java.lang.Boolean => st.registerOutParameter(idx, BOOLEAN)
      case s: String => st.registerOutParameter(idx, VARCHAR)
      case bn: java.math.BigDecimal => st.registerOutParameter(idx, DECIMAL, bn.scale)
      case bd: BigDecimal => st.registerOutParameter(idx, DECIMAL, bd.scale)
      //unknown object
      case obj => st.registerOutParameter(idx, OTHER)
    }
  }

  private def bindVarsValues(bindVars: List[Expr]) = {
    // scala 3
    //val lf = Option(env.bindVarLogFilter).map(_ orElse { case (_, v) => v }).getOrElse(_._2)
    val lf: ((String, Any)) => Any = Option(env.bindVarLogFilter)
      .map(_ orElse { case (_, v) => v }: PartialFunction[(String, Any), Any]).getOrElse(_._2)
    bindVars.flatMap {
      case v: VarExpr => List(v.fullName -> lf((v.fullName, v())))
      case r: ResExpr => List(r.name -> r())
      case id: IdExpr => List(s"#${id.seqName}" -> id.peek)
      case ir: IdRefExpr => List(s":#${ir.seqName}" -> ir.peek)
      case irid: ORT#IdRefIdExpr => List(s":#${irid.idRefSeq}#${irid.idSeq}" -> irid())
      case _ => Nil
    }
  }

  private def log(sql: String, bindVars: List[Expr]) = {
    env.log(sql, Nil, LogTopic.sql)
    env.log(sql, bindVarsValues(bindVars), LogTopic.sql_with_params)
    env.log(
      bindVarsValues(bindVars)
        .map { case (n, v) => s"$n -> ${String.valueOf(v)}"}
        .mkString("[", ", ", "]"),
      Nil, LogTopic.params
    )
  }

  // TODO make this method accessible to resources for unknown type binding customization
  private def toJsonString(value: Any) = {
    def dumpJson(value: Any, sb: StringBuilder): Unit = value match {
      case m: Map[String @unchecked, _] =>
        sb.append('{')
        printSeq(m, sb.append(',')) { m =>
          printString(m._1, sb)
          sb.append(':')
          dumpJson(m._2, sb)
        }
        sb.append('}')
      case seq: Seq[_] =>
        sb.append('[')
        printSeq(seq, sb.append(','))(dumpJson(_, sb))
        sb.append(']')
      case s: String => printString(s, sb)
      case n: Number => sb.append(n)
      case b: Boolean => sb.append(b)
      case x => printString(String.valueOf(x), sb)
    }
    // copied from spray.json.JsonPrinter to avoid dependency
    def printString(s: String, sb: StringBuilder): Unit = {
      @tailrec def firstToBeEncoded(ix: Int = 0): Int =
        if (ix == s.length) -1 else if (requiresEncoding(s.charAt(ix))) ix else firstToBeEncoded(ix + 1)
      sb.append('"')
      firstToBeEncoded() match {
        case -1 => sb.append(s)
        case first =>
          sb.append(s, 0, first)
          @tailrec def append(ix: Int): Unit =
            if (ix < s.length) {
              s.charAt(ix) match {
                case c if !requiresEncoding(c) => sb.append(c)
                case  '"' => sb.append("\\\"")
                case '\\' => sb.append("\\\\")
                case '\b' => sb.append("\\b")
                case '\f' => sb.append("\\f")
                case '\n' => sb.append("\\n")
                case '\r' => sb.append("\\r")
                case '\t' => sb.append("\\t")
                case x if x <= 0xF => sb.append("\\u000").append(Integer.toHexString(x))
                case x if x <= 0xFF => sb.append("\\u00").append(Integer.toHexString(x))
                case x if x <= 0xFFF => sb.append("\\u0").append(Integer.toHexString(x))
                case x => sb.append("\\u").append(Integer.toHexString(x))
              }
              append(ix + 1)
            }
          append(first)
      }
      sb.append('"')
    }
    def printSeq[A](iterable: Iterable[A], printSeparator: => Unit)(f: A => Unit): Unit = {
      var first = true
      iterable.foreach { a =>
        if (first) first = false else printSeparator
        f(a)
      }
    }
    def requiresEncoding(c: Char): Boolean =
      c match {
        case '"'  => true
        case '\\' => true
        case c    => c < 0x20
      }

    val sb = new StringBuilder
    dumpJson(value, sb)
    sb.toString
  }
}

/** Out parameter box for callable statement */
class OutPar(var value: Any) {
  private[tresql] var idx = 0
  def this() = this(null)
  override def toString = "OutPar(" + value + ")"
}
object OutPar {
  def apply() = new OutPar()
  def apply(value: Any) = new OutPar(value)
  def unapply(par: OutPar): Option[Any] = Some(par.value)
}

/** In out parameter box for callable statement */
class InOutPar(v: Any) extends OutPar(v) {
  override def toString = "InOutPar(" + value + ")"
}
object InOutPar {
  def apply(value: Any) = new InOutPar(value)
  def unapply(par: InOutPar): Option[Any] = Some(par.value)
}

object Query extends Query
