package org.tresql.parsing

import org.tresql.ast._
trait ExpTransformer { this: QueryParsers =>

  type Transformer = PartialFunction[Exp, Exp]
  type TransformerWithState[T] = T => PartialFunction[Exp, Exp]
  type Traverser[T] = T => PartialFunction[Exp, T]

  def transformer(fun: Transformer): Transformer = {
    lazy val transform_traverse = fun orElse traverse
    //helper function
    def tt[A <: Exp](exp: A) = transform_traverse(exp).asInstanceOf[A]
    lazy val traverse: Transformer = {
      case All => All
      case n: Null => n
      case c: Const => c
      case s: Sql => s
      case e: Ident => e
      case e: Id => e
      case e: IdRef => e
      case e: Res => e
      case e: IdentAll => e
      case e: Variable => e
      case Fun(n, pars, d, o, f) => Fun(n, pars map tt, d, o map tt, f map tt)
      case FunAsTable(f, cds, ord) => FunAsTable(tt(f), cds, ord)
      case Cast(e, t) => Cast(tt(e), t)
      case UnOp(o, op) => UnOp(o, tt(op))
      case ChildQuery(q, db) => ChildQuery(tt(q), db)
      case BinOp(o, lop, rop) => BinOp(o, tt(lop), tt(rop))
      case TerOp(lop, op1, mop, op2, rop) => TerOp(tt(lop), op1, tt(mop), op2, tt(rop))
      case In(lop, rop, not) => In(tt(lop), rop map tt, not)
      case Obj(t, a, j, o, n) => Obj(tt(t), a, tt(j), o, n)
      case Join(d, j, n) => Join(d, tt(j), n)
      case Col(c, a) => Col(tt(c), a)
      case Cols(cols, d) => Cols(cols map tt, tt(d))
      case Distinct(on) => Distinct(on map tt)
      case Grp(cols, hv) => Grp(cols map tt, tt(hv))
      case OrdCol(nf, e, nl) => OrdCol(nf, tt(e), nl)
      case Ord(cols) => Ord(cols map tt)
      case Query(objs, filters, cols, gr, ord, off, lim) =>
        Query(objs map tt, tt(filters), tt(cols), tt(gr), tt(ord), tt(off), tt(lim))
      case WithTable(n, c, r, q) => WithTable(n, c, r, tt(q))
      case With(ts, q) => With(ts map tt, tt(q))
      case Insert(t, a, cols, vals, r, db, cf) => Insert(tt(t), a, cols map tt, tt(vals), r map tt, db, tt(cf))
      case Update(t, a, filter, cols, vals, r, db) => Update(tt(t), a, tt(filter), cols map tt, tt(vals), r map tt, db)
      case Delete(t, a, filter, u, r, db) => Delete(tt(t), a, tt(filter), tt(u), r map tt, db)
      case Arr(els) => Arr(els map tt)
      case Filters(f) => Filters(f map tt)
      case Values(v) => Values(v map tt)
      case ValuesFromSelect(s) => ValuesFromSelect(tt(s))
      case InsertConflict(a, t, al, cd) => InsertConflict(tt(a), tt(t), al, cd)
      case InsertConflictAction(c, f, v) => InsertConflictAction(c map tt, tt(f), tt(v))
      case InsertConflictTarget(t, f) => InsertConflictTarget(t map tt, tt(f))
      case Braces(expr) => Braces(tt(expr))
      case null => null
    }
    transform_traverse
  }

  def transformerWithState[T](fun: TransformerWithState[T]): TransformerWithState[T] = {
    def transform_traverse(state: T): Transformer = fun(state) orElse traverse(state)
    //helper function
    def tt[A <: Exp](state: T)(exp: A) = transform_traverse(state)(exp).asInstanceOf[A]
    def traverse(state: T): Transformer = {
      case All => All
      case n: Null => n
      case c: Const => c
      case s: Sql => s
      case e: Ident => e
      case e: Id => e
      case e: IdRef => e
      case e: Res => e
      case e: IdentAll => e
      case e: Variable => e
      case Fun(n, pars, d, o, f) =>
        Fun(n, pars map tt(state), d, o map tt(state), f map tt(state))
      case FunAsTable(f, cds, ord) => FunAsTable(tt(state)(f), cds, ord)
      case Cast(e, t) => Cast(tt(state)(e), t)
      case UnOp(o, op) => UnOp(o, tt(state)(op))
      case ChildQuery(q, db) => ChildQuery(tt(state)(q), db)
      case BinOp(o, lop, rop) => BinOp(o, tt(state)(lop), tt(state)(rop))
      case TerOp(lop, op1, mop, op2, rop) => TerOp(tt(state)(lop), op1, tt(state)(mop), op2, tt(state)(rop))
      case In(lop, rop, not) => In(tt(state)(lop), rop map tt(state), not)
      case Obj(t, a, j, o, n) => Obj(tt(state)(t), a, tt(state)(j), o, n)
      case Join(d, j, n) => Join(d, tt(state)(j), n)
      case Col(c, a) => Col(tt(state)(c), a)
      case Cols(cols, d) => Cols(cols map tt(state), tt(state)(d))
      case Distinct(on) => Distinct(on map tt(state))
      case Grp(cols, hv) => Grp(cols map tt(state), tt(state)(hv))
      case OrdCol(nf, e, nl) => OrdCol(nf, tt(state)(e), nl)
      case Ord(cols) => Ord(cols map tt(state))
      case Query(objs, filters, cols, gr, ord, off, lim) =>
        Query(
          objs map tt(state),
          tt(state)(filters),
          tt(state)(cols),
          tt(state)(gr),
          tt(state)(ord),
          tt(state)(off),
          tt(state)(lim)
        )
      case WithTable(n, c, r, q) => WithTable(n, c, r, tt(state)(q))
      case With(ts, q) => With(ts map { wt => tt(state)(wt) }, tt(state)(q))
      case Insert(t, a, cols, vals, r, db, cf) => Insert(tt(state)(t), a,
          cols map { c => tt(state)(c) }, tt(state)(vals), r map tt(state), db, tt(state)(cf))
      case Update(table, alias, filter, cols, vals, r, db) =>
        Update(
          tt(state)(table),
          alias,
          tt(state)(filter),
          cols map { c => tt(state)(c) },
          tt(state)(vals),
          r map tt(state),
          db
        )
      case Delete(table, alias, filter, using_, returning, db) =>
        Delete(tt(state)(table), alias, tt(state)(filter), tt(state)(using_), returning map tt(state), db)
      case Arr(els) => Arr(els map tt(state))
      case Filters(f) => Filters(f map tt(state))
      case Values(v) => Values(v map tt(state))
      case ValuesFromSelect(s) => ValuesFromSelect(tt(state)(s))
      case InsertConflict(a, t, al, cd) => InsertConflict(tt(state)(a), tt(state)(t), al, cd)
      case InsertConflictAction(c, f, v) => InsertConflictAction(c map tt(state), tt(state)(f), tt(state)(v))
      case InsertConflictTarget(t, f) => InsertConflictTarget(t map tt(state), tt(state)(f))
      case Braces(expr) => Braces(tt(state)(expr))
      case null => null
    }
    transform_traverse
  }

  def traverser[T](fun: Traverser[T]): Traverser[T] = {
    def fun_traverse(state: T) = fun(state) orElse traverse(state)
    def tr(r: T, e: Exp): T = fun_traverse(r)(e)
    def trl(r: T, l: List[Exp]) = l.foldLeft(r) { (fr, el) => tr(fr, el) }
    def tro(r: T, o: Option[Exp]) = o.map(tr(r, _)).getOrElse(r)
    def traverse(state: T): PartialFunction[Exp, T] = {
      case _: Ident | _: Id | _: IdRef | _: Res | All | _: IdentAll | _: Variable | _: Null | _: Const |
           _: Sql | null => state
      case Fun(_, pars, _, o, f) =>
        val ps = trl(state, pars)
        val os = o.map(tr(ps, _)).getOrElse(ps)
        f.map(tr(os, _)).getOrElse(os)
      case FunAsTable(f, _, _) => tr(state, f)
      case Cast(e, _) => tr(state, e)
      case UnOp(_, operand) => tr(state, operand)
      case ChildQuery(query, _) => tr(state, query)
      case BinOp(_, lop, rop) => tr(tr(state, lop), rop)
      case In(lop, rop, _) => trl(tr(state, lop), rop)
      case TerOp(lop, _, mop, _, rop) => tr(tr(tr(state, lop), mop), rop)
      case Obj(t, _, j, _, _) => tr(tr(state, j), t) //call tr method in order of writing tresql statement
      case Join(_, j, _) => tr(state, j)
      case Col(c, _) => tr(state, c)
      case Cols(cols, d) => tr(trl(state, cols), d)
      case Distinct(on) => trl(state, on)
      case Grp(cols, hv) => tr(trl(state, cols), hv)
      case OrdCol(_, e, _) => tr(state, e)
      case Ord(cols) => trl(state, cols)
      case Query(objs, filters, cols, gr, ord, off, lim) =>
        tr(tr(tr(tr(tr(tr(trl(state, objs), filters), cols), gr), ord), off), lim)
      case WithTable(_, _, _, q) => tr(state, q)
      case With(ts, q) => tr(trl(state, ts), q)
      case Insert(t, _, cols, vals, r, _, cf) => tr(tro(tr(trl(tr(state, t), cols), vals), r), cf)
      case Update(t, _, filter, cols, vals, r, _) => tro(tr(trl(tr(tr(state, t), filter), cols), vals), r)
      case Delete(t, _, filter, u, r, _) => tro(tr(tr(tr(state, t), u), filter), r)
      case Arr(els) => trl(state, els)
      case Filters(f) => trl(state, f)
      case Values(v) => trl(state, v)
      case ValuesFromSelect(s) => tr(state, s)
      case InsertConflict(a, t, _, _) => tr(tr(state, a), t)
      case InsertConflictAction(c, f, v) => tr(tr(trl(state, c), f), v)
      case InsertConflictTarget(t, f) => tr(trl(state, t), f)
      case Braces(expr) => tr(state, expr)
    }
    fun_traverse
  }

  /** Extract variables in reverse order. Variable names '?' are replaced with index starting with 1 */
  def variableExtractor: Traverser[List[Variable]] = {
    var bindIdx = 0
    vars => {
      case v @ Variable("?", _, _) =>
        bindIdx += 1
        (v copy bindIdx.toString) :: vars
      case v: Variable => v :: vars
    }
  }
  /** Extract database names. */
  def dbExtractor: Traverser[List[String]] = dbs => {
    case ChildQuery(_, db) => db.map(_ :: dbs).getOrElse(dbs)
    case dml: DMLExp => dml.db.map(_ :: dbs).getOrElse(dbs)
  }
}
