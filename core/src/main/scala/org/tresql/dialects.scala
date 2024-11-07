package org.tresql

import org.tresql.ast.CompilerAst.ExprType

package object dialects {

  object ANSISQLDialect extends CoreTypes.Dialect {
    def isDefinedAt(e: Expr) = exec(e)._1
    def apply(e: Expr) = exec(e)._2
    private def exec(e: Expr) = {
      val b = e.builder
      e match {
        case b.FunExpr("case", pars, false, None, None) if pars.size > 1 => (true, pars.grouped(2).map(l =>
          if (l.size == 1) "else " + l.head.sql else "when " + l.head.sql + " then " + l(1).sql)
          .mkString("case ", " ", " end"))
        case _ => (false, "<none>")
      }
    }
  }

  val VariableNameDialect: CoreTypes.Dialect = {
    case v: QueryBuilder#VarExpr => v.defaultSQL match {
      case s if (s.indexWhere (_ == '?') match {
        case -1 => false
        case x => s.indexWhere (_ == '?', x + 1) match {
          case -1 => false
          case _ => true
        }}) => s"/*${v.fullName}[*/$s/*]${v.fullName}*/"
      case s => s + s"/*${v.fullName}*/"
    }
    case r: QueryBuilder#ResExpr => r.defaultSQL + s"/*${r.name}*/"
    case id: QueryBuilder#IdExpr => id.defaultSQL + s"/*#${id.seqName}*/"
    case idref: QueryBuilder#IdRefExpr => idref.defaultSQL + s"/*:#${idref.seqName}*/"
    case idrefid: ORT#IdRefIdExpr => idrefid.defaultSQL + s"/*:#${idrefid.idRefSeq}#${idrefid.idSeq}*/"
  }

  def commonDialect(vendor: String): CoreTypes.Dialect = {
    case f: QueryBuilder#FunExpr
      if f.name == "cast" && f.params.size == 2 && f.params(1).isInstanceOf[QueryBuilder#ConstExpr] =>
      s"cast(${f.params(0).sql} as ${f.builder.env.metadata.to_sql_type(vendor,
        String.valueOf(f.params(1).asInstanceOf[QueryBuilder#ConstExpr].value))})"
  }

  val HSQLRawDialect: CoreTypes.Dialect = {
    case f: QueryBuilder#FunExpr if f.name == "lower" && f.params.size == 1 => "lcase(" + f.params.head.sql + ")"
    case f: QueryBuilder#FunExpr if f.name == "translate" && f.params.size == 3 =>
      val b = f.builder
      val List(col, b.ConstExpr(from: String), b.ConstExpr(to: String)) = f.params
      (from zip to).foldLeft(col.sql)((s, a) => "replace(" + s + ", '" + a._1 + "', '" + a._2 + "')")
    case f: QueryBuilder#FunExpr if f.name == "nextval" && f.params.size == 1 =>
      val b = f.builder
      val List(b.ConstExpr(seq: String)) = f.params
      "next value for " + seq
    case v: QueryBuilder#VarExpr if is_sql_array(v) =>
      v.defaultSQL // register bind variable
      s"array[${sql_arr_bind_vars(v())}]"
    case c: QueryBuilder#CastExpr => s"cast(${c.exp.sql} as ${c.builder.env.metadata.to_sql_type("hsqldb", c.typ) match {
      case "varchar array" => "longvarchar array" // size of varchar unknown and required by hsqldb - use longvarchar instead
      case "varchar" => "longvarchar" // size of varchar unknown and required by hsqldb - use longvarchar instead
      case x => x
    }})"
    case c: QueryBuilder#TableColDefExpr => c.name
    case b: QueryBuilder#BinExpr if b.op == "`~`" => s"regexp_matches(${b.lop.sql}, ${b.rop.sql})"
    case s: QueryBuilder#SelectExpr if s.tables.size == 1 &&
      s.tables.head.table.isInstanceOf[QueryBuilder#ConstExpr] &&
      s.tables.head.table.asInstanceOf[QueryBuilder#ConstExpr].value.isInstanceOf[ast.Null] =>
      val b = s.builder
      s.asInstanceOf[b.SelectExpr]
        .copy(tables = List(s.asInstanceOf[b.SelectExpr].tables.head
          .copy(table = b.IdentExpr(List("(values(0))"))))).sql
    case i: QueryBuilder#InsertExpr if i.insertConflict != null =>
      val ic = i.insertConflict
      val (a, vc) = (ic.valuesAlias, ic.valuesCols)
      s"merge into ${i.table.sql} using (${i.vals.sql}) as $a${vc.sql} on ${
        ic.targetFilter.sql} when matched then update set ${ic.vals match {
        case a: QueryBuilder#ArrExpr =>
          (ic.cols zip a.elements map { v => v._1.sql + " = " + v._2.sql }).mkString(", ")
        case q: QueryBuilder#SelectExpr => ic.cols.map(_.sql).mkString("(", ", ", ")") + " = " + "(" + q.sql + ")"
        case x => sys.error("Knipis: " + x)
      }} when not matched then insert (${i.cols.map(_.sql).mkString(", ")}) values ${
        vc.cols.map(c => s"$a.${c.sql}").mkString(", ")}"
  }

  object OracleRawDialect extends CoreTypes.Dialect {
    def isDefinedAt(e: Expr) = {
      val b = e.builder
      e match {
        case b.ConstExpr(_: Boolean) => true
        case b.BinExpr("-", _, _) => true
        case _: QueryBuilder#TableColDefsExpr => true
        case e: QueryBuilder#SelectExpr if e.limit != null || e.offset != null => true
        case e: QueryBuilder#SelectExpr if e.cols.cols.headOption.exists(_.col match {
          case f: QueryBuilder#FunExpr if f.name == "optimizer_hint" && f.params.size == 1 &&
            f.params.head.isInstanceOf[QueryBuilder#ConstExpr] => true
          case _ => false
        }) => true
        case _ => false
      }
    }
    def apply(e: Expr) = {
      val b = e.builder
      e match {
        case b.ConstExpr(true) => "1 = 1"
        case b.ConstExpr(false) => "1 = 0"
        case b.TableColDefsExpr(_) => ""
        case b.FunExpr("optimizer_hint", List(b.ConstExpr(s: String)), false, None, None) => s
        case b.BinExpr("-", lop, rop) =>
          lop.sql + (if (e.exprType.getSimpleName == "SelectExpr") " minus " else " - ") + rop.sql
        case e: QueryBuilder#SelectExpr if e.limit != null || e.offset != null =>
          val b = e.builder //cannot match SelectExpr if builder is not extracted!!!
          e match {
            case s @ b.SelectExpr(_, _, _, _, _, _, o, l, _, _) =>
              val ns = s.copy(offset = null, limit = null)
              (o, l) match {
                case (null, null) => ns.sql
                case (null, l) =>
                  "select * from (" + ns.sql + ") where rownum <= " + l.sql
                case (o, null) =>
                  "select * from (select w.*, rownum rnum from (" + ns.sql + ") w) where rnum > " + o.sql
                case _ =>
                  "select * from (select w.*, rownum rnum from (" + ns.sql +
                    ") w where rownum <= " + l.sql + ") where rnum > " + o.sql
              }
            case x => sys.error(s"Unexpected case: $x")
          }
        case e: QueryBuilder#SelectExpr if e.cols.cols.headOption.exists(_.col match {
          case f: QueryBuilder#FunExpr if f.name == "optimizer_hint" && f.params.size == 1 &&
            f.params.head.isInstanceOf[QueryBuilder#ConstExpr] => true
          case _ => false
        }) =>
          val b = e.builder
          e match {
            case s @ b.SelectExpr(_, _,
              c @ b.ColsExpr(b.ColExpr(b.FunExpr(_, List(b.ConstExpr(h)), _, _, _), _, _, _) :: t, _, _, _, _),
              _, _, _, _, _, _, _) =>
                   "select " + String.valueOf(h) + (s.copy(cols = c.copy(cols = t)).sql substring 6)
          }
      }
    }
  }

  val PostgresqlRawDialect: CoreTypes.Dialect = {
    case c: QueryBuilder#ColExpr if c.alias != null => Option(c.col).map(_.sql).getOrElse("null") + " as " + c.alias
    case c: QueryBuilder#CastExpr => c.exp.sql + "::" + c.builder.env.metadata.to_sql_type("postgresql", c.typ)
    case c: QueryBuilder#TableColDefExpr => c.name +
      c.typ.map(t => c.builder.env.metadata.to_sql_type("postgresql", t)).map(" " + _).mkString
    case v: QueryBuilder#VarExpr if is_sql_array(v) =>
      v.defaultSQL // register bind variable
      s"array[${sql_arr_bind_vars(v())}]"
    case f: QueryBuilder#FunExpr if f.name == "decode" && f.params.size > 2 =>
      f.params.tail.grouped(2).map { g =>
        if (g.size == 2) s"when ${g(0).sql} then ${g(1).sql}"
        else s"else ${g(0).sql}"
      }.mkString(s"case ${f.params(0).sql} ", " ", " end")
    case i: QueryBuilder#InsertExpr =>
      //pg insert as select needs column cast if bind variables are from 'from' clause select
      val b = i.builder
      i.vals match {
        case ivals@b.SelectExpr(
        List(valstable@b.Table(b.BracesExpr(vals: b.SelectExpr), _, _, _, _, _)),
        _, _, _, _, _, _, _, _, _) =>
          val table = i.table.name.last
          //insertable column names
          val colNames = i.cols.collect { case b.ColExpr(b.IdentExpr(name), _, _, _) => name.last } toSet
          //second level query which needs column casts matching insertable column names
          val colsWithCasts =
            vals.cols.cols.map {
              case c@b.ColExpr(e, a, _, _) if colNames(a) =>
                val md = c.builder.env.metadata
                md.colOption(table, a).map {
                  case org.tresql.metadata.Col(_, _, ExprType(tn)) => md.to_sql_type("postgresql", tn)
                }
                .map(typ => c.copy(col = b.CastExpr(e, typ)))
                .getOrElse(c)
              case x => x
            }
          //copy modified cols to second level query cols
          val colsExprWithCasts = vals.cols.copy(cols = colsWithCasts)
          new b.InsertExpr(i.table.asInstanceOf[b.IdentExpr], i.alias, i.cols, ivals
            .copy(tables = List(valstable.copy(
              table = b.BracesExpr(vals.copy(cols = colsExprWithCasts))))),
            i.insertConflict.asInstanceOf[b.InsertConflictExpr],
            i.returning.asInstanceOf[Option[b.ColsExpr]]
          ).defaultSQL
        case _ => i.defaultSQL
      }
  }

  def is_sql_array(variable: QueryBuilder#VarExpr): Boolean = !variable.allowArrBind && {
    val value = variable()
    !value.isInstanceOf[Array[Byte]] && value.isInstanceOf[Array[_]] || value.isInstanceOf[Iterable[_]]
  }
  def sql_arr_bind_vars(value: Any) = {
    def sql(i: Iterable[_]) = i.iterator.map(_ => "?").mkString(", ")
    value match {
      case a: Array[_] => sql(a.toSeq)
      case i: Iterable[_] => sql(i)
      case x => sys.error(s"Cannot make bind vars string from $x, iterable needed")
    }
  }

  def HSQLDialect: CoreTypes.Dialect =
    HSQLRawDialect orElse commonDialect("hsqldb") orElse ANSISQLDialect
  def OracleDialect: CoreTypes.Dialect =
    OracleRawDialect orElse commonDialect("oracle") orElse ANSISQLDialect
  def PostgresqlDialect: CoreTypes.Dialect =
    PostgresqlRawDialect orElse commonDialect("postgresql") orElse ANSISQLDialect
}
