package org.tresql.test

import org.tresql._
import org.tresql.ast.{Arr, Col, Cols, Exp, Filters, Fun, IntConst, Join, Null, Obj, TransformerExp, With, WithTable, Query => PQuery}

class Macros extends org.tresql.Macros {
  import macro_._

  /**
   * Dumb regexp to find bind variables (tresql syntax) in sql string.
   * Expects whitespace, colon, identifier, optional question mark.
   * Whitespace before colon is a workaround to ignore postgresql typecasts.
   */
  private val varRegex = "\\s:[_a-zA-Z]\\w*\\??"r
  override def sql(b: QueryBuilder, const: QueryBuilder#ConstExpr): b.SQLExpr = {
    val value = String.valueOf(const.value)
    val vars = varRegex.findAllIn(value).toList
      .map(_ substring 2)
      .map(v => b.VarExpr(v.replace("?", ""), Nil, v endsWith "?", allowArrBind = false))
    val sqlSnippet = varRegex.replaceAllIn(value, " ?")
    if (vars.exists(v => v.opt && !(b.env contains v.name)))
      b.SQLExpr("null", Nil)
    else b.SQLExpr(sqlSnippet, vars)
  }
  def in_twice(implicit p: parsing.QueryParsers,
               expr: Exp,
               in: Exp) = macro_"$expr in ($in, $in)"
  def null_macros(b: QueryBuilder): Expr = null
  def dummy(b: QueryBuilder) = b.buildExpr("dummy")
  def dummy_table(b: QueryBuilder) = b.IdentExpr(List("dummy"))
  // add this macro to function signatures to specify return type
  def plus(b: QueryBuilder, e1: Expr, e2: Expr) =
    b.BinExpr("+", e1, e2)

  def build_values_cursor(p: parsing.QueryParsers, data: Exp): Exp = {
    def withQ(q: Exp) = {
      val sel = PQuery(List(Obj(Null)), Filters(Nil), Cols(List(Col(IntConst(0))), null))
      With(List(WithTable("int_values", List("value"), false, sel)), q)
    }

    lazy val transformer: p.Transformer = {
      p.transformer {
        case q@PQuery(
          ol@Obj(_, _, Join(_,
          Arr(List(Fun("build_values_cursor", _, _, _, _))), _
          ), _, _) :: os, _, _, _, _, _, _
        ) =>
          withQ(q.copy(tables = ol.head.copy(join = null) :: os))
        case o@Obj(_, _, Join(_,
          Arr(List(Fun("build_values_cursor", _, _, _, _))), _
          ), _, _
        ) =>
          withQ(o.copy(join = null))
        case With(tables, query) =>
          val tr_tables = (tables map transformer).asInstanceOf[List[WithTable]]
          transformer(query) match {
            case w: With => With(w.tables ++ tr_tables, w.query) //put bind var cursor tables first
            case q: Exp => With(tr_tables, q)
          }
      }
    }
    TransformerExp(transformer)
  }
  def build_values_cursor(b: QueryBuilder, v: QueryBuilder#VarExpr): Expr = {
    val data = b.env(v.fullName).asInstanceOf[Seq[Int]]
    lazy val transformer: PartialFunction[Expr, Expr] = {
      case s@b.SelectExpr(
        b.Table(_, _,
          b.TableJoin(_, b.ArrExpr(List(b.FunExpr("build_values_cursor", _, _, _, _))), _, _)
          , _, _, _
        ) :: _,
        _, _, _, _, _, _, _, _, _
      ) =>
        def value(i: Int) =
          if (i < 0) b.ConstExpr(i)
          else b.VarExpr(v.name, v.members :+ i.toString, opt = false, allowArrBind = false)
        def sel(i: Int, filter: Expr) =
          b.SelectExpr(List(
            b.Table(b.ConstExpr(ast.Null), null, b.TableJoin(false, null, true, null), null, false, null)),
            filter, b.ColsExpr(List(b.ColExpr(value(i), "value")), false, false, false),
            null, null, null, null, null, Map(), None
          )

        def union(e1: Expr, e2: Expr) = b.BinExpr("++", e1, e2)

        def cursor =
          if (data.isEmpty) sel(-1, b.ConstExpr(false))
          else
            (1 until data.size)
              .foldLeft[Expr](sel(0, null)) { (c, i) => union(c, sel(i, null)) }

        b.WithSelectExpr(List(b.WithTableExpr("int_values", List("value"), false, cursor)), s)
      case b.WithSelectExpr(tables, q) =>
        val tr_tables = tables.map(b.transform(_, transformer)).asInstanceOf[List[b.WithTableExpr]]
        b.transform(q, transformer) match {
          case w: b.WithSelectExpr => b.WithSelectExpr(w.tables ++ tr_tables, w.query)
          case s: b.SelectExpr => b.WithSelectExpr(tr_tables, s)
          case x => x
        }
    }
    b.TransformerExpr(transformer)
  }
}

class Macros1 extends Macros {
  // '(' || e1 || ', ' || e2 || ')'
  def format_tuple_b(b: QueryBuilder, e1: Expr, e2: Expr) = {
    def c(s: String) = b.ConstExpr(s)
    List(c("("), e1, c(", "), e2, c(")")).reduceLeft(b.BinExpr("||", _, _))
  }
}

object Macros extends Macros
object Macros1 extends Macros1