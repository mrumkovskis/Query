package org.tresql.parsing

import org.tresql.MacroResources
import org.tresql.ast._

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers


trait QueryParsers extends JavaTokenParsers with MemParsers with ExpTransformer {

  protected def macros: MacroResources = null

  def parseExp(exp: String): Exp = {
    phrase(exprList)(new scala.util.parsing.input.CharSequenceReader(exp)) match {
      case Success(r, _) => maybeTransform(r, transformers) match {
        case _: TransformerExp => sys.error("Parsing error - cannot return TransformerExp!")
        case e => e
      }
      case x => sys.error(x.toString)
    }
  }

  private var transformers: List[Transformer] = Nil

  val reserved = Set("in", "null", "false", "true")

  //comparison operator regular expression
  val comp_op = """!?in\b|[<>=!~%$]+|`[^`]+`"""r

  val NoJoin = Join(default = false, null, noJoin = true)
  val DefaultJoin = Join(default = true, null, noJoin = false)

  //JavaTokenParsers overrides
  //besides standart whitespace symbols consider as a whitespace also comments in form /* comment */ and //comment
  override val whiteSpace = """([\h\v]*+(/\*(.|[\h\v])*?\*/)?(//.*+(\n|$))?)+"""r

  override def stringLiteral: MemParser[String] = ("""("[^"]*+")|('[^']*+')"""r) ^^ {
    case s => s.substring(1, s.length - 1)
  } named "string-literal"

  override def ident: MemParser[String] = super.ident ^? ({ case x if !(reserved contains x) => x },
      { x => s"'$x' is one of the reserved words: ${reserved.mkString(", ")}" }) named "ident"

  override def wholeNumber: MemParser[String] = ("""\d+"""r) named "number"

  def decimalNr: MemParser[BigDecimal] = decimalNumber ^^ (BigDecimal(_)) named "decimal-nr"

  //literals
  def TRUE: MemParser[Boolean] = ("\\btrue\\b"r) ^^^ true named "true"
  def FALSE: MemParser[Boolean] = ("\\bfalse\\b"r) ^^^ false named "false"
  def NULL: MemParser[Null.type] = ("\\bnull\\b"r) ^^^ Null named "null"
  def ALL: MemParser[All.type] = "*" ^^^ All named "all"

  def const: MemParser[Const] = (TRUE | FALSE | decimalNr | stringLiteral) ^^ {
    case b: Boolean => BooleanConst(b)
    case bd: BigDecimal => BigDecimalConst(bd)
    case s: String => StringConst(s)
    case i: Int => IntConst(i)
    case x => sys.error(s"Unexpected const value: '$x'. Expected `String` or `Boolean` or `BigDecimal` or `Int`")
  } named "const"
  def sql: MemParser[Fun] = "`" ~> ("[^`]+"r) <~ "`" ^^ { sql_str =>
    Fun("sql", List(StringConst(sql_str)), false, None, None)
  } named "sql"
  def qualifiedIdent: MemParser[Ident] = rep1sep(ident, ".") ^^ Ident named "qualified-ident"
  def qualifiedIdentAll: MemParser[IdentAll] = qualifiedIdent <~ ".*" ^^ IdentAll named "ident-all"
  def variable: MemParser[Variable] = ((":" ~> (
      rep1sep(ident | stringLiteral | wholeNumber, ".") ~ opt("?"))) | "?") ^^ {
    case "?" => Variable("?", Nil, opt = false)
    case ((i: String) :: (m: List[String @unchecked])) ~ o =>
      Variable(i, m, o != None)
  } named "variable"
  def id: MemParser[Id] = "#" ~> qualifiedIdent ~ opt(":" ~> ident) ^^ {
    case id ~ mayBeBindVar => Id(id.ident.mkString(".") + mayBeBindVar.map(":" + _).getOrElse(""))
  } named "id"
  def idref: MemParser[IdRef] = ":#" ~> qualifiedIdent ~ opt(":" ~> ident) ^^ {
    case id  ~ mayBeBindVar => IdRef(id.ident.mkString(".") + mayBeBindVar.map(":" + _).getOrElse(""))
  } named "id-ref"
  def result: MemParser[Res] = {
    val resArg = wholeNumber | stringLiteral | qualifiedIdent
    ((("^" ~> wholeNumber <~ ".") ~ resArg) | ((":" ~> wholeNumber <~ "(") ~ resArg <~ opt(")"))) ^^ {
      case r ~ c => Res(r.toInt,
        c match {
          case s: String =>
            try { IntConst(s.toInt) } catch { case _: NumberFormatException => StringConst(s) }
          case i: Ident => i
        })
    } named "result"
  }
  def braces: MemParser[Braces] = "(" ~> expr <~ ")" ^^ Braces named "braces"
  /* Function parser must be applied before query because of the longest token
     * matching, otherwise qualifiedIdent of query will match earlier.
     * withQuery parser must be applied before function parser
     * array parser must be applied after query parser because it matches join parser
     * of the query.
     * insert, delete, update parsers must be applied before query parser
     * result parser must be applied before variable parser */
  def operand: MemParser[Exp] = (const | ALL | withQuery | function  | sql | insert | update | result |
    variable | query | id | idref  | array) named "operand"
  /* function(<#> <arglist> <order by>). Maybe used in from clause so filter is not confused with join syntax.  */
  def functionWithoutFilter: MemParser[Fun] = (qualifiedIdent /* name */ <~ "(") ~
    opt("#") /* distinct */ ~ repsep(expr, ",") /* arglist */ ~
    ")" ~ opt(order) /* aggregate order */ ^^ {
    case Ident(n) ~ d ~ p ~ _ ~ o =>
      Fun(n.mkString("."), p, d.isDefined, o, None)
  }  named "fun-without-filter"
  /* function(<#> <arglist> <order by>)<[filter]> */
  def function: MemParser[Exp] = (qualifiedIdent /* name */ <~ "(") ~
    opt("#") /* distinct */ ~ repsep(expr, ",") /* arglist */ ~
    ")" ~ opt(order) /* aggregate order */ ~ opt(filter) /* aggregate filter */ ^?({
    case Ident(n) ~ d ~ p ~ _ ~ o ~ f if f.map(_.elements.size).getOrElse(0) <= 1 =>
      Fun(n.mkString("."), p, d.isDefined, o, f.flatMap(_.elements.lift(0)))
  }, {
    case _ ~ _ ~ _ ~ _ ~ _ ~ f => s"Aggregate function filter must contain only one elements, instead of ${
      f.map(_.elements.size).getOrElse(0)}"
  }) ^^ { f =>
    if (isMacro(f.name)) {
      if (f.distinct || f.aggregateOrder.nonEmpty || f.aggregateWhere.nonEmpty) {
        sys.error(s"Macro '${f.name}' invocation error. " +
          s"Neither distinct nor order by nor filter clause can be used in macro invocation.")
      } else {
        macros.invokeMacro(f.name, QueryParsers.this, f.parameters) match {
          case TransformerExp(t) =>
            transformers ::= t
            f
          case e => e
        }
      }
    } else f
  } named "function"
  def array: MemParser[Arr] = "[" ~> repsep(expr, ",") <~ "]" ^^ Arr named "array"

  //query parsers
  def join: MemParser[Join] = (("/" ~ opt("[" ~> expr <~ "]")) | (opt("[" ~> expr <~ "]") ~ "/") |
    ";" | array) ^^ {
      case ";" => NoJoin
      case "/" ~ Some(e: Exp @unchecked) => Join(default = true, e, noJoin = false)
      case "/" ~ None => DefaultJoin
      case Some(e: Exp @unchecked) ~ "/" => Join(default = true, e, noJoin = false)
      case None ~ "/" => DefaultJoin
      case a: Exp => Join(default = false, a, noJoin = false)
    } named "join"
  def filter: MemParser[Arr] = array named "filter"
  def filters: MemParser[Filters] = rep(filter) ^^ Filters named "filters"
  /** objContent is meant to be table, column or division operation operand */
  private def objContent: MemParser[Exp] =
    (const | functionWithoutFilter | result | variable | qualifiedIdent | sql | braces) named "obj-content"
  private def alias: MemParser[(String, Option[List[TableColDef]], Boolean)] =
    ident ~ opt("(" ~> opt("#") ~ rep1sep(ident ~ opt(cast), ",") <~ ")") ^^ {
      case id ~ Some(mbOrd ~ colDefs) =>
        (id, Some(colDefs.map { case cn ~ typ => TableColDef(cn, typ) }), mbOrd.isDefined)
      case id ~ None => (id, None, false)
    } named "alias"
  def obj: MemParser[Obj] = opt(join) ~ opt("?") ~ objContent ~
    opt(opt("?" | "!") ~ alias ~ opt("?" | "!")) ^^ {
    case _ ~ Some(_) ~ _ ~ Some(Some(_) ~ _ ~ _ | _ ~ _ ~ Some(_)) =>
      sys.error("Cannot be right and left join at the same time")
    case join ~ rightoj ~ o ~ Some(leftoj ~ alias ~ leftoj1) =>
      def processAlias(coldefs: Option[List[TableColDef]], ord: Boolean) = {
        o match {
          case f: Fun => FunAsTable(f, coldefs, ord)
          case x if coldefs == None => x
          case x => sys.error(s"Table definition is allowed only after function. Instead found: ${x.tresql}")
        }
      }
      Obj(processAlias(alias._2, alias._3), alias._1, join.orNull,
        rightoj.map(x => "r") orElse (leftoj orElse leftoj1).map(j => if(j == "?") "l" else "i") orNull,
        (leftoj orElse leftoj1).exists(_ == "?"))
    case join ~ rightoj ~ o ~ None => Obj(o, null, join orNull, rightoj.map(x => "r") orNull)
  } named "obj"
  def objWithJoin: MemParser[Obj] = obj ^? ({
    case obj if obj.join != null => obj
  } , {
    case obj => s"no join condition found on object: ${obj.tresql}"
  }) named "obj-with-join"
  def objs: MemParser[List[Obj]] = obj ~ rep(objWithJoin ~ opt("?" | "!")) ^^ {
    case o ~ l =>
      var prev: Obj = null
      val res = (o :: l.map {
        case o ~ Some(oj) if "r" == o.outerJoin => sys.error("Cannot be right and left join at the same time")
        case o ~ Some(oj) => o.copy(outerJoin = if (oj == "?") "l" else "i", nullable = oj == "?")
        case o ~ None => o
      }).flatMap { thisObj =>
        val (prevObj, prevAlias) = (prev, if (prev == null) null else prev.alias)
        prev = thisObj
        //process foreign key shortcut join
        thisObj match {
          case o @ Obj(_, a, j @ Join(false, Arr(l @ List(o1 @ Obj(_, a1, _, oj1, n1), _*)), false), oj, n) =>
            //o1.alias prevail over o.alias, o.{outerJoin, nullable} prevail over o1.{outerJoin, nullable}
            List(o.copy(alias = Option(a1).getOrElse(a), join =
              j.copy(expr = o1.copy(alias = null, outerJoin = null, nullable = false)),
              outerJoin = if (oj == null) oj1 else oj, nullable = n || n1)) ++
              (if (prevObj == null) List() else l.tail.flatMap {
                //flattenize array of foreign key shortcut joins
                case o2 @ Obj(_, a2, _, oj2, n2) => List(if (prevAlias != null) Obj(Ident(List(prevAlias)),
                  null, NoJoin, null)
                else Obj(prevObj.obj, null, NoJoin, null),
                  o.copy(alias = if (a2 != null) a2 else o.alias, join =
                    j.copy(expr = o2.copy(alias = null, outerJoin = null, nullable = false)),
                    outerJoin = if (oj == null) oj2 else oj, nullable = n || n2))
                case x => List(o.copy(join = Join(default = false, x, noJoin = false)))
              })
          case o => List(o)
        }
      }
      Vector(res: _*).lastIndexWhere(_.outerJoin == "r") match {
        case -1 => res
        //set nullable flag for all objs right to the last obj with outer join
        case x => res.zipWithIndex.map(t => if (t._2 < x && !t._1.nullable) t._1.copy(nullable = true) else t._1)
      }
  } named "objs"
  def column: MemParser[Col] =
    (qualifiedIdentAll | (expr ~ opt(stringLiteral | qualifiedIdent))) ^^ {
      def aliasToStr(maybeAlias: Option[_]): String = {
        maybeAlias map {
          case Ident(i) => i.mkString; case s => "\"" + s + "\""
        } orNull
      }
      {
        case (c: Col) ~ (a: Option[_]) =>
          val al = aliasToStr(a)
          if (al == null) c else c.copy(alias = al)
        case i: IdentAll => Col(i, null)
        //move object alias to column alias
        case (o @ Obj(_, a, _, _, _)) ~ Some(ca) if a != null =>
          sys.error(s"Column cannot have two aliases: `$a`, `$ca`")
        case (o @ Obj(_, a, _, _, _))  ~ None => Col(o.copy(alias = null), a)
        case (e: Exp @unchecked) ~ (a: Option[_]) => Col(e, aliasToStr(a))
      }
    } ^^ {
      def extractAlias(expr: Exp): (String, Exp) = expr match {
        case t: TerOp => extractAlias(t.content)
        case o@BinOp(_, _, rop) =>
          val x = extractAlias(rop)
          (x._1, o.copy(rop = x._2))
        case o@UnOp(_, op) =>
          val x = extractAlias(op)
          (x._1, o.copy(operand = x._2))
        case o@ChildQuery(op, _) =>
          val x = extractAlias(op)
          (x._1, o.copy(query = x._2))
        case o@Obj(_, alias, _, null, _) if alias != null => (alias, o.copy(alias = null))
        case o => (null, o)
      }
      {
        case c @ Col(_: IdentAll | _: Obj, _) => c
        case c @ Col(e, null) => extractAlias(e) match {
          case (null, _) => c
          case (a, e) =>
            c.copy(col = e, alias = a)
        }
        case r => r
      }
    } named "column"
  def distinct: MemParser[Distinct] = ("#" ~> opt("(" ~> rep1sep(expr, ",") <~ ")")) ^^
    (on => Distinct(on.getOrElse(Nil))) named "distinct"
  def columns: MemParser[Cols] = (opt(distinct) <~ "{") ~ rep1sep(column, ",") <~ "}" ^^ {
    case d ~ c => Cols(c, d.orNull)
  } named "columns"
  def group: MemParser[Grp] = ("(" ~> rep1sep(expr, ",") <~ ")") ~
    opt(("^" ~ "(") ~> expr <~ ")") ^^ { case g ~ h => Grp(g, if (h.isEmpty) null else h.get) }  named "group"
  def orderMember: MemParser[OrdCol] = opt(NULL) ~ expr ~ opt(NULL) ^^ {
    case Some(nf) ~ e ~ Some(nl) => sys.error("Cannot be nulls first and nulls last at the same time")
    case nf ~ e ~ nl => OrdCol(if (nf.isEmpty) null else nf.get, e, if (nl.isEmpty) null else nl.get)
  } named "order-member"
  def order: MemParser[Ord] = ("#" ~ "(") ~> rep1sep(orderMember, ",") <~ ")" ^^ Ord named "order"
  def offsetLimit: MemParser[(Exp, Exp)] = ("@" ~ "(") ~> (wholeNumber | variable) ~ opt(",") ~
    opt(wholeNumber | variable) <~ ")" ^^ { pr =>
      {
        def c(x: Any) =
          x match { case v: Variable => v case s: String => BigDecimalConst(BigDecimal(s)) }
        pr match {
          case o ~ comma ~ Some(l) => (c(o), c(l))
          case o ~ Some(comma) ~ None => (c(o), null)
          case o ~ None ~ None => (null, c(o))
        }
      }
    } named "offset-limit"
  def query: MemParser[Exp] = ((objs | NULL) ~ filters ~ opt(columns ~ opt(group)) ~ opt(order) ~
    opt(offsetLimit) | (columns ~ filters)) ^^ {
      case Null ~ Filters(Nil) ~ None ~ None ~ None => Null //null literal
      case List(t) ~ Filters(Nil) ~ None ~ None ~ None => t match {
        case Obj(b: Braces, null, j, null, _) =>
          //transform potential join to parent into query
          Option(j).map(transformHeadJoin(_)(b)).getOrElse(b)
        case x => x
      }
      case t ~ (f: Filters) ~ cgo ~ (o: Option[Ord @unchecked]) ~ (l: Option[(Exp, Exp) @unchecked]) =>
        val cg: (Cols, Grp) = cgo match {
          case Some((c: Cols) ~ Some(g: Grp)) => (c, g)
          case Some((c: Cols) ~ None) => (c, null)
          case None => (null, null)
        }
        Query(t match {
          case tables: List[Obj @unchecked] => tables
          case Null => List(Obj(Null, null, null, null))
        }, f, cg._1, cg._2, o.orNull, l.map(_._1) orNull, l.map(_._2) orNull)
      case (c: Cols) ~ (f: Filters) => Query(List(Obj(Null, null, null, null)), f, c,
        null, null, null, null)
    } ^^ { pr =>
      def toDivChain(objs: List[Obj]): Exp = {
        def parseChain(os: List[Obj]): (Exp, String) =
          os match {
            case o :: Nil => (o.obj, o.alias)
            case l =>
              val tailRes = parseChain(l.tail)
              (BinOp("/", l.head.obj, tailRes._1), tailRes._2)
          }
        parseChain(objs) match {
          case (e, null) => e
          case (e, a) => Col(e, a)
        }
      }
      pr match {
        case Query(objs, Filters(Nil), null, null, null, null, null) if objs forall {
          case Obj(_, _, DefaultJoin, null, _) | Obj(_, _, null, null, _) => true
          case _ => false
        } => toDivChain(objs)
        case q: Exp => q
      }
    } named "query"

  def queryWithCols: MemParser[Exp] = query ^? ({
      case q @ Query(objs, _, cols, _, _, _, _) if cols != null => q
    }, {case x => "Query must contain column clause: " + x}) named "query-with-cols"

  def withTable: MemParser[WithTable] =
    ((ident <~ "(") ~ opt("#") ~ (ALL | repsep(ident, ",")) <~ (")" ~ "{")) ~ (expr <~ "}") ^^ {
      case name ~ distinct ~ (cols: List[String@unchecked]) ~ exp => WithTable(name, cols, distinct.isEmpty, exp)
      case name ~ distinct ~ All ~ exp => WithTable(name, Nil, distinct.isEmpty, exp)
    } named "with-table"
  def withQuery: MemParser[With] = opt(join) ~ rep1sep(withTable, ",") ~ opt(expr) ^^ {
    case optJoin ~ wts ~ None =>
      val q = Obj(Ident(List(wts.last.name)), null, null, null) // select * from <last cursor>
      With(wts, optJoin.map(transformHeadJoin(_)(q)).getOrElse(q))
    case optJoin ~ wts ~ Some(q) => With(wts, optJoin.map(transformHeadJoin(_)(q)).getOrElse(q))
  } named "with-query"

  def values: MemParser[Values] = rep1sep(array, ",") ^^ Values named "values"
  def valuesSelect: MemParser[Exp] = (withQuery | queryWithCols) ~ rep(
    ("++" | "+" | "-" | "&&") ~ (withQuery | queryWithCols)) ^^ binOp named "values-select"

  private def isAssignmentCols(c: Cols) = c.cols.nonEmpty && c.cols.forall {
    case Col(BinOp("=", Obj(_: Ident, _, _, _, _), _), _) => true
    case _ => false
  }
  private def splitAssignmentCols(c: Cols): (List[Col], List[Exp]) =
    c.cols.map {
      case Col(BinOp("=", col, value), a) => (Col(col, a), value)
      case x => sys.error(s"Knipis: $x")
    }.unzip
  private def optDb: MemParser[Option[String]] =
    opt(opt(ident) <~ ":") ^^ (_.map(_.getOrElse(""))) named "opt-db"
  private def fullInsert: MemParser[Insert] = "+" ~> optDb ~
    qualifiedIdent ~ opt(ident) ~ opt(columns) ~ opt(valuesSelect | values) ~
    opt(insertConflict) ~ opt(columns) ^^ {
      case db ~ t ~ a ~ Some(c) ~ None ~ ic ~ maybeReturning if isAssignmentCols(c) =>
        val (cols, vals) = splitAssignmentCols(c)
        // insert in form +table{col1 = val2, col2 = val2, ...}
        Insert(t, a.orNull, cols, Values(List(Arr(vals))), maybeReturning, db, ic.orNull)
      case db ~ t ~ a ~ c ~ v ~ ic ~ maybeReturning =>
        Insert(
          t, a.orNull, c.map(_.cols).getOrElse(Nil),
          v.getOrElse(Values(Nil)),
          maybeReturning, db, ic.orNull
        )
    }
  def insert: MemParser[Insert] =
    fullInsert | ((qualifiedIdent ~ opt(ident) ~ opt(columns) <~ "+") ~ values ~ opt(columns)) ^^ {
      case (t: Ident) ~ a ~ c ~ (v: Values @unchecked) ~ maybeCols =>
        Insert(t, a.orNull, c.map(_.cols).getOrElse(Nil), v, maybeCols, None)
      case x => sys.error(s"Unexpected insert parse result: $x")
    } named "insert"
  private def insertConflict: MemParser[InsertConflict] = {
    def processFilter(f: Option[Arr]) = f.collect {
      case x if x.elements.size == 1 => x
      case x => sys.error(s"Invalid insert conflict target filter: $x")
    }.orNull
    def insertConflictTarget: MemParser[(String, List[TableColDef], InsertConflictTarget)] =
      opt(alias) ~ opt("(" ~> exprList <~ ")") ~ opt(filter) ^^ {
        case vd ~ t ~ tf =>
          val target = InsertConflictTarget(t.map { case a: Arr => a.elements case e => List(e) }.getOrElse(Nil),
            processFilter(tf))
          vd.map(d => (d._1, d._2.getOrElse(Nil), target)).getOrElse((null, Nil, target))
      } named "insert-conflict-target"
    def insertConflictAction: MemParser[InsertConflictAction] =
      "=" ~> opt(filter) ~ columns ~ opt(valuesSelect | array) ^^ {
      case f ~ c ~ None =>
        val (cols, vals) = splitAssignmentCols(c)
        InsertConflictAction(cols, f.orNull, Arr(vals))
      case f ~ c ~ Some(v) =>
        InsertConflictAction(c.cols, f.orNull, v)

    } named "insert-conflict-action"
    opt("?" ~> insertConflictTarget) ~ opt(insertConflictAction) ^^ {
      case None ~ None => null
      case t ~ a =>
        val (va, vc, target) = t.getOrElse((null, null, null))
        def processTarget(t: InsertConflictTarget) = if (t == null) null else t
        InsertConflict(a.orNull, processTarget(target), va, vc)
    }
  } named "insert-conflict"

  //UPDATE parsers
  // update table set col1 = val1, col2 = val2 where ...
  private def simpleUpdate: MemParser[Update] =
    (("=" ~> optDb ~ qualifiedIdent ~ opt(ident) ~ opt(filter) ~ opt(columns) ~ opt(array)) |
      ((qualifiedIdent ~ opt(ident) ~ opt(filter) ~ opt(columns) <~ "=") ~ array)) ~ opt(columns) ^^ {
        case (db: Option[String@unchecked]) ~ (t: Ident) ~ (a: Option[String@unchecked] ) ~ f ~ c ~ v ~ maybeCols =>
          Update(
            t, a orNull, f orNull, c.map(_.cols).getOrElse(Nil), v match {
              case a: Arr => a
              case Some(vals: Exp @unchecked) => vals
              case None => null
            },
            maybeCols, db
          )
        case (t: Ident) ~ (a: Option[String] @unchecked) ~ f ~ c ~ v ~ maybeCols =>
          Update(
            t, a orNull, f orNull, c.map(_.cols).getOrElse(Nil), v match {
              case a: Arr => a
              case Some(vals: Exp @unchecked) => vals
              case None => null
            },
            maybeCols, None
          )
     } named "simple-update"
  //update table set (col1, col2) = (select col1, col2 from table 2 ...) where ...
  private def updateColsSelect: MemParser[Update] =
    "=" ~> optDb ~ qualifiedIdent ~ opt(ident) ~ opt(filter) ~ columns ~ valuesSelect ~ opt(columns) ^^
      {
        case (db: Option[String]) ~ (t: Ident) ~ (a: Option[String] @unchecked) ~ f ~ c ~ v ~ maybeCols =>
          Update(t, a orNull, f orNull, c.cols, v, maybeCols, db)
      } ^?
      ({
        case u if u.cols.nonEmpty && u.cols.forall {
          case Col(BinOp("=", _, _), _) => false
          case _ => true
        } => u
      }, {
        case x => s"Columns must not contain assignement operation. Instead encountered: ${x.tresql}"
      }) named "update-cols-select"
  //update table set col1 = sel_col1, col2 = sel_col2 from (select sel_col1, sel_col2 from table2 ...) values_table where ....
  private def updateFromSelect: MemParser[Update] =
    "=" ~> optDb ~ objs ~ opt(filter) ~ columns ~ opt(columns) ^? ({
    case (db: Option[String]) ~  (tables @ Obj(updateTable @ Ident(_), alias, _, _, _) :: _) ~ f ~ c ~ maybeCols =>
      Update(
        updateTable,
        alias,
        f.orNull,
        c.cols,
        ValuesFromSelect(
          Query(tables = tables, Filters(Nil), null, null, null, null, null)
        ),
        maybeCols, db
      )
  }, {
    case _ ~ (objs: List[Obj] @unchecked) ~ _ ~ _ ~ _ => "Update tables clause must as the first element have " +
     "qualifiedIdent with optional alias as a table to be updated" +
     s"Instead encountered: ${objs.map(_.tresql).mkString}"
  }) ^? ({
    case u if u.cols.nonEmpty && u.cols.forall {
      case Col(BinOp("=", Obj(_: Ident, _, _, _, _), _), _) => true
      case _ => false
    } => u.copy(cols = u.cols.map {
      //set assignable value to NullUpdate so null update expression is col = null, not col is null
      case c @ Col(a @ BinOp(_, _, Null), _) => c.copy(col = a.copy(rop = NullUpdate))
      case x => x
    })
  }, {
    case _ => "Update from select columns must be assignment expressions"
  }) named "update-from-select"
  def update: MemParser[Update] = (updateColsSelect | updateFromSelect | simpleUpdate) named "update"
  //END UPDATE parsers

  def delete: MemParser[Delete] = {
    def valsFromSel(tables: Any) = tables match {
      case t: List[Obj@unchecked] =>
        if (t.tail.nonEmpty)
          ValuesFromSelect(
            Query(
              tables = t,
              Filters(Nil),
              null, null, null, null, null
            )
          )
        else null
    }
    (("-" ~> optDb ~ objs ~ filter) | ((objs <~ "-") ~ filter)) ~ opt(columns) ^? ({
      case (db: Option[String@unchecked]) ~ (tables @ Obj(delTable: Ident, alias, _, _, _) :: _) ~ (f: Arr) ~
        (maybeCols: Option[Cols]) =>
        Delete(delTable, alias, f, valsFromSel(tables), maybeCols, db)
      case (tables @ Obj(delTable: Ident, alias, _, _, _) :: _) ~ (f: Arr) ~ (maybeCols: Option[Cols]) =>
        Delete(delTable, alias, f, valsFromSel(tables), maybeCols, None)
    }, {
      case (objs: List[Obj@unchecked]) ~ _ ~ _ => "Delete tables clause must as the first element have " +
        "qualifiedIdent with optional alias as a table to be deleted from" +
        s"Instead encountered: ${objs.map(_.tresql).mkString}"
    }) named "delete"
  }
  //operation parsers
  //delete must be before alternative since it can start with - sign and
  //so it is not translated into minus expression!
  def unaryExpr: MemParser[Exp] = delete | (opt(("|" ~ optDb) | "-" | "!" | "~") ~ operand) ^^ {
    case None ~ e => e
    case Some(o: String) ~ (e: Exp) => UnOp(o, e)
    case Some(_ ~ (db: Option[String]@unchecked)) ~ (q: Exp) => ChildQuery(q, db)
    case del: Exp => del
  } named "unary-exp"
  private def cast: MemParser[String] = ("::" ~> (ident | stringLiteral)) named "cast"
  def castExpr: MemParser[Exp] = unaryExpr ~ opt(cast) ^^ {
    case e ~ Some(t) => Cast(e, t)
    case e ~ None => e
  } named "cast-expr" //postgresql style type conversion operator
  def mulDiv: MemParser[Exp] = castExpr ~ rep("*" ~ castExpr | "/" ~ castExpr) ^^ binOp named "mul-div"
  def plusMinus: MemParser[Exp] = mulDiv ~ rep(("++" | "+" | "-" | "&&" | "||") ~ mulDiv) ^^ binOp named "plus-minus"
  def comp: MemParser[Exp] = plusMinus ~ rep(comp_op ~ plusMinus) ^? (
      {
        case lop ~ Nil => lop
        case lop ~ ((o ~ rop) :: Nil) =>
          if (BinOp.STANDART_BIN_OPS contains o) BinOp(o, lop, rop)
          else Fun("bin_op_function", List(StringConst(o), lop, rop), false, None, None) // non standart op
        case lop ~ List((o1 ~ mop), (o2 ~ rop)) => TerOp(lop, o1, mop, o2, rop)
      },
      {
        case lop ~ x => "Ternary comparison operation is allowed, however, here " + (x.size + 1) +
          " operands encountered."
      }) named "comp"
  def in: MemParser[In] = plusMinus ~ ("in" | "!in") ~ "(" ~ rep1sep(plusMinus, ",") <~ ")" ^^ {
    case lop ~ "in" ~ "(" ~ rop => In(lop, rop, not = false)
    case lop ~ "!in" ~ "(" ~ rop => In(lop, rop, not = true)
  } named "in"
  //in parser should come before comp so that it is not taken for in function which is illegal
  def logicalOp: MemParser[Exp] = in | comp named "logical-op"
  def expr: MemParser[Exp] = logicalOp ~ rep("&" ~ logicalOp | "|" ~ logicalOp) ^^ binOp named "expr"
  def exprList: MemParser[Exp] = repsep(expr, ",") ^^ {
    case e :: Nil => e
    case l => Arr(l)
  } named "expr-list"

  private def binOp(p: Exp ~ List[String ~ Exp]) = p match {
    case op ~ Nil => op
    case op ~ l => bin_op_rec(l, op)
  }
  /** Create BinOp with non recursive function (tailrec annotation) to avoid StackOverflowError */
  @tailrec private final def bin_op_rec(p: List[String ~ Exp], r: Exp): Exp = p match {
    case Nil => r
    case (o ~ rop) :: l => bin_op_rec(l, BinOp(o, r, rop))
  }

  private def transformHeadJoin(join: Join): Transformer = transformer {
    case Braces(e) => Braces(transformHeadJoin(join)(e))
    case With(wt, q) => With(wt, transformHeadJoin(join)(q))
    case q: Query =>
      q.copy(tables = q.tables.updated(0, transformHeadJoin(join)(q.tables.head).asInstanceOf[Obj]))
    case o: Obj => o.copy(join = join) //set join to parent
  }

  protected def isMacro(name: String): Boolean = macros != null && macros.isMacroDefined(name) &&
    !macros.isBuilderMacroDefined(name) && !macros.isBuilderDeferredMacroDefined(name)

  protected def maybeTransform(e: Exp, ts: List[Transformer]): Exp = ts match {
    case Nil => e
    case t :: tail => maybeTransform(t(e), tail)
  }

  /** Copied from scala.util.parsing.combinator.SubSequence since it cannot be instantiated. */
  // A shallow wrapper over another CharSequence (usually a String)
  //
  // See SI-7710: in jdk7u6 String.subSequence stopped sharing the char array of the original
  // string and began copying it.
  // RegexParsers calls subSequence twice per input character: that's a lot of array copying!
  private class SubSequence(s: CharSequence, start: Int, val length: Int) extends CharSequence {
    def this(s: CharSequence, start: Int) = this(s, start, s.length - start)

    def charAt(i: Int) =
      if (i >= 0 && i < length) s.charAt(start + i) else throw new IndexOutOfBoundsException(s"index: $i, length: $length")

    def subSequence(_start: Int, _end: Int) = {
      if (_start < 0 || _end < 0 || _end > length || _start > _end)
        throw new IndexOutOfBoundsException(s"start: ${_start}, end: ${_end}, length: $length")

      new SubSequence(s, start + _start, _end - _start)
    }

    override def toString = s.subSequence(start, start + length).toString
  }
}
