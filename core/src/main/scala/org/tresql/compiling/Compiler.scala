package org.tresql
package compiling

import org.tresql.parsing._
import org.tresql.ast._
import org.tresql.ast.CompilerAst._
import org.tresql.metadata._

trait Compiler extends QueryParsers { thisCompiler =>

  def metadata: Metadata
  def extraMetadata: Map[String, Metadata] = Map()

  protected def error(msg: String, cause: Exception = null) = throw new CompilerException(msg, cause = cause)

  trait Scope {
    def tableNames: List[String]
    def table(table: String): Option[Table]
    def column(col: String): Option[org.tresql.metadata.Col] = None
    def isEqual(exp: SQLDefBase): Boolean
  }

  trait TableMetadata {
    def tableOption(name: String)(db: Option[String]): Option[Table]
  }

  object EnvMetadata extends TableMetadata {
    override def tableOption(name: String)(database: Option[String]): Option[Table] =
      database
        .flatMap(db => extraMetadata.getOrElse(db, error(s"Unknown database: $db")).tableOption(name))
        .orElse(metadata.tableOption(name))
  }

  case class WithTableMetadata(scopes: List[Scope]) extends TableMetadata {
    override def tableOption(name: String)(database: Option[String]): Option[Table] = {
      def to(lScopes: List[Scope]): Option[Table] = lScopes match {
        case Nil => EnvMetadata.tableOption(name)(database)
        case scope :: tail => scope.table(name).orElse(to(tail))
      }
      to(scopes)
    }
  }

  object ExpToScope {
    class SQLDefScope(exp: SQLDefBase) extends Scope {
      protected def table_from_selectdef(name: String, sd: SelectDefBase) =
        Table(name, sd.cols map col_from_coldef, null, Map())

      protected def col_from_coldef(cd: ColDef) =
        org.tresql.metadata.Col(name = cd.name, nullable = true, colType = cd.typ)

      def tableNames: List[String] = exp.tables.collect {
        //collect table names in this sql (i.e. exclude tresql no join aliases)
        case TableDef(t, Obj(_: TableObj, _, _, _, _)) => t.toLowerCase
      }

      def table(table: String): Option[Table] = {
        def name_matches(name: String) =
          name == table || name.substring(name.lastIndexOf('.') + 1) == table

        def table_alias(name: String) = Table(name, Nil, null, Map())

        exp.tables.filter(t => name_matches(t.name.toLowerCase)).flatMap {
          case TableDef(_, Obj(TableObj(Ident(name)), _, _, _, _)) =>
            List(table_alias(name mkString "."))
          case TableDef(n, Obj(TableObj(s: SelectDefBase), _, _, _, _)) =>
            List(table_from_selectdef(n, s))
          case TableDef(_, Obj(TableObj(_: Null), _, _, _, _)) =>
            List(table_alias(null))
          case TableDef(_, Obj(TableObj(FunAsTableDef(_, Some(cols), _)), alias, _, _, _)) =>
            List(Table(alias,
              cols.map(c =>
                org.tresql.metadata.Col(name = c.name,
                  nullable = true,
                  colType =
                    c.typ
                      .map(ExprType.apply)
                      .getOrElse(ExprType.Any))), null, Map()))
          case TableDef(_, Obj(_: TableAlias, _, _, _, _)) => Nil
          case x => error(
            s"Unrecognized table clause: '${x.tresql}' [$x]. Try using Query(...)")
        } match {
          case Nil => None
          case l@List(_) => l.headOption
          case x => x.filter(_.name == table) match {
            case Nil => None
            case t@List(_) => t.headOption
            case x => error(s"Ambiguous table name. Tables found - $x")
          }
        }
      }
      override def isEqual(e: SQLDefBase): Boolean = e == exp
    }

    class SelectDefScope(exp: SelectDefBase) extends SQLDefScope(exp) {
      override def column(col: String) = exp.cols.find(_.name == col) map col_from_coldef
    }

    class WithTableDefScope(exp: WithTableDef) extends SQLDefScope(exp) {
      override def table(table: String): Option[Table] = exp.tables.find(_.name == table).map {
        _ => table_from_selectdef(table, exp)
      }
    }

    case class TranslatedScope(tableNames: List[String],
                               tableMap: String => Option[String],
                               colReverseMap: String => String => Option[String])(
      md: TableMetadata, db: Option[String]) extends Scope {
      private val tables = tableNames.flatMap { tn => tableMap(tn)
        .flatMap(Compiler.this.table(Nil)(_)(md, db))
        .map(t => t.copy(name = tn, cols = t.cols.flatMap(c => colReverseMap(tn)(c.name)
          .map(cn => c.copy(name = cn)).toList))).toList
      }
      override def table(table: String): Option[Table] =
        tables.find(_.name == table.toLowerCase)
      override def column(col: String): Option[org.tresql.metadata.Col] = col.lastIndexOf(".") match {
        case -1 => tableNames.flatMap(tn => column(s"$tn.${col.toLowerCase}").toList) match {
          case List(c) => Some(c)
          case Nil => None
          case l => sys.error(s"Ambiguous column name.")
        }
        case i =>
          for (t <- table(col.substring(0, i).toLowerCase); c <- t.colOption(col.substring(i + 1).toLowerCase))
            yield c
      }
      override def isEqual(exp: SQLDefBase): Boolean = false
    }

    implicit def expToScope(exp: SQLDefBase): Scope = exp match {
      case e: WithTableDef => new WithTableDefScope(e)
      case e: SelectDefBase => new SelectDefScope(e)
      case e => new SQLDefScope(e)
    }
    implicit def expListToScopeList(l: List[SQLDefBase]): List[Scope] = l map expToScope
  }

  //metadata
  /** Table of dml statement or table in from clause of any scope in the {{{scopes}}} list */
  def declaredTable(scopes: List[Scope])(tableName: String)(md: TableMetadata,
                                                            db: Option[String]): Option[Table] = {
    val tn = tableName.toLowerCase
    scopes match {
      case Nil => None
      case scope :: tail => scope.table(tn).flatMap {
        case Table(n, Nil, _, _) => table(tail)(n)(md, db) //alias is decoded ask parent scope
        case t => Some(t)
      } orElse declaredTable(tail)(tn)(md, db)
    }
  }
  /** Table from metadata or defined in from clause or dml table of any scope in {{{scopes}}} list */
  def table(scopes: List[Scope])(tableName: String)(md: TableMetadata,
                                                    db: Option[String]): Option[Table] =
    Option(tableName).flatMap { tn =>
      val table = tn.toLowerCase
      declaredTable(scopes)(table)(md, db) orElse md.tableOption(table)(db)
    }
  /** Column declared in any scope in {{{scopes}}} list */
  def column(scopes: List[Scope])(colName: String)(md: TableMetadata,
                                                   db: Option[String]): Option[org.tresql.metadata.Col] = {
    val col = colName.toLowerCase
    (scopes match {
      case Nil => None
      case scope :: tail => col.lastIndexOf('.') match {
        case -1 =>
          scope.tableNames
            .map(tn => tn -> declaredTable(scopes)(tn)(md, db).flatMap(_.colOption(col)))
            .collect { case (tn, col @ Some(_)) => (tn, col) } match {
              case List((_, c)) => c
              case Nil => column(tail)(col)(md, db)
              case x => error(
                s"Ambiguous columns: ${x.map { case (t, c) =>
                  s"$t.${c.map(_.name).iterator.mkString}"
                }.mkString(", ")}"
              )
            }
        case x => declaredTable(scopes)(col.substring(0, x))(md, db).flatMap(_.colOption(col.substring(x + 1)))
      }
    }).orElse(procedure(s"$col#0")(db).map(p => //check for empty pars function declaration
      org.tresql.metadata.Col(name = col, nullable = true, colType = p.resolveReturnType)))
  }
  /** Method is used to resolve column names in group by or order by clause, since they can reference columns by name from column clause. */
  def declaredColumn(scopes: List[Scope])(colName: String)(md: TableMetadata,
                                                           db: Option[String]): Option[org.tresql.metadata.Col] = {
    val col = colName.toLowerCase
    scopes.head.column(col) orElse column(scopes)(col)(md, db)
  }
  def procedure(procedure: String)(database: Option[String]) =
    database
      .flatMap(db => extraMetadata
        .getOrElse(db, error(s"Unknown database: $db"))
        .procedureOption(procedure))
      .orElse(metadata.procedureOption(procedure))

  def buildTypedDef(exp: Exp) = {
    trait Ctx
    object QueryCtx extends Ctx //root context
    object TablesCtx extends Ctx //from clause
    object ColsCtx extends Ctx //column clause
    object BodyCtx extends Ctx //where, group by, having, order, limit clauses
    object InsConflCtx extends Ctx // on conflict do (upsert) clause

    case class BuildCtx(ctx: Ctx, db: Option[String])

    //helper function
    def tr_with_c(bCtx: BuildCtx, ctx: Ctx, x: Exp): Exp = builder(bCtx.copy(ctx = ctx))(x)
    def tr(bCtx: BuildCtx, x: Exp): Exp = builder(bCtx)(x)
    def buildTables(ctx: BuildCtx, tables: List[Obj]): List[TableDef] = {
      val td1 = tables.zipWithIndex map { case (table, idx) =>
        val newTable = tr_with_c(ctx, TablesCtx, table.obj)
        val join = tr_with_c(ctx, BodyCtx, table.join).asInstanceOf[Join]
        val name = Option(table.alias).getOrElse(table match {
          case Obj(Ident(name), _, _, _, _) => name mkString "."
          case _ => s"_${idx + 1}"
        })
        TableDef(name, table.copy(obj = TableObj(newTable), join = join)) -> idx
      }
      //process no join aliases
      val td2 = td1 map { case (table, idx) => table match {
          //NoJoin alias
          case td @ TableDef(_, o @ Obj(TableObj(Ident(name)), _,
            Join(_, _, true), _, _)) => //check if alias exists
            val qn = name.mkString(".")
            if (td1.view(0, idx).exists {
              case (TableDef(n, _), _) => n == qn || n.substring(n.lastIndexOf('.') + 1) == qn
            }) td.copy(exp = o.copy(obj = TableAlias(Ident(name))))
            else error(s"Referenced table not found in scope: $qn")
          case TableDef(_, Obj(x, _, Join(_, _, true), _, _)) =>
            error(s"Unsupported table reference: $x, ${td1.map(_._1.tresql)}, $td1")
          case x => x //ordinary table def
        }
      }
      //add table alias to foreign key alias joins with unqualified names
      td2.tail.scanLeft(td2.head) {
        case (left,
          right @ TableDef(_,
            exp @ Obj(_: TableObj, _,
              join @ Join(_,
                expr @ Obj(Ident(List(fkAlias)), _, _, _, _), false), _, _))) =>
              //add left table alias to foreign key alias join ident
              right.copy(exp =
                exp.copy(join =
                  join.copy(expr =
                    expr.copy(obj =
                      Ident(List(left.name, fkAlias))))))
        case (_, right) => right
      }
    }
    def buildCols(ctx: BuildCtx, cols: Cols): List[ColDef] = {
      if (cols != null) (cols.cols map {
          //child dml statement in select
          case c @ ast.Col(_: DMLExp @unchecked, _) => tr_with_c(ctx, QueryCtx, c)
          case c => tr_with_c(ctx, ColsCtx, c)
        }).asInstanceOf[List[ColDef]] match {
          case l if l.exists(_.name == null) => //set names of columns
            l.zipWithIndex.map { case (c, i) =>
              if (c.name == null) c.copy(name = s"_${i + 1}") else c
            }
          case l => l
        }
      else List[ColDef](ColDef(null, All, ExprType()))
    }
    lazy val builder: TransformerWithState[BuildCtx] = transformerWithState(ctx => {
      case f: Fun => procedure(s"${f.name}#${f.parameters.size}")(ctx.db).map { p =>
        val retType = p.resolveReturnType
        FunDef(p.name, f.copy(
          parameters = f.parameters map(tr(ctx, _)),
          aggregateOrder = f.aggregateOrder.map(tr(ctx, _).asInstanceOf[Ord]),
          aggregateWhere = f.aggregateWhere.map(tr(ctx, _))
        ), retType, p)
      }.getOrElse(error(s"Unknown function: ${f.name}"))
      case ftd: FunAsTable => FunAsTableDef(tr(ctx, ftd.fun).asInstanceOf[FunDef], ftd.cols, ftd.withOrdinality)
      case c: ast.Col if ctx.ctx != InsConflCtx => // types not relevant for insert conflict update cols
        val alias = if (c.alias != null) c.alias else c.col match {
          case Obj(Ident(name), _, _, _, _) => name.last //use last part of qualified ident as name
          case Cast(Obj(Ident(name), _, _, _, _), _) => name.last //use last part of qualified ident as name
          case _ => null
        }
        ColDef(
          alias,
          tr(ctx, c.col) match {
            case x: DMLDefBase @unchecked => ChildDef(x, x.db)
            case x: ReturningDMLDef => ChildDef(x, x.exp.db)
            case x => x
          },
          ExprType()
        )
      case Obj(b: Braces, _, _, _, _) if ctx.ctx == QueryCtx =>
        builder(ctx)(b) //unwrap braces top level expression
      case o: Obj if ctx.ctx == QueryCtx | ctx.ctx == TablesCtx => //obj as query
        builder(ctx)(Query(List(o), Filters(Nil), null, null, null, null, null))
      case o: Obj if ctx.ctx == BodyCtx =>
        o.copy(obj = builder(ctx)(o.obj), join = builder(ctx)(o.join).asInstanceOf[Join])
      case PrimitiveExp(q) => PrimitiveDef(builder(ctx)(q), ExprType())
      case q: Query =>
        val tables = buildTables(ctx, q.tables)
        val cols = buildCols(ctx, q.cols)
        val (filter, grp, ord, limit, offset) =
          (tr_with_c(ctx, BodyCtx, q.filter).asInstanceOf[Filters],
           tr_with_c(ctx, BodyCtx, q.group).asInstanceOf[Grp],
           tr_with_c(ctx, BodyCtx, q.order).asInstanceOf[Ord],
           tr_with_c(ctx, BodyCtx, q.limit),
           tr_with_c(ctx, BodyCtx, q.offset))
        SelectDef(
          cols,
          tables,
          Query(
            tables = Nil,
            filter = filter,
            cols = Cols(Nil, Option(q.cols).map(_.distinct).orNull),
            group = grp,
            order = ord,
            limit = limit,
            offset = offset))
      case b: BinOp =>
        (tr(ctx, b.lop), tr(ctx, b.rop)) match {
          case (lop: SelectDefBase @unchecked, rop: SelectDefBase @unchecked) =>
            BinSelectDef(lop, rop, b)
          //asume left operand select definition is provided by db function or macro
          case (lop: FunDef @unchecked, rop: SelectDefBase @unchecked) =>
            BinSelectDef(FunSelectDef(Nil, Nil, lop), rop, b)
          //asume right operand select definition is provided by db function or macro
          case (lop: SelectDefBase @unchecked, rop: FunDef @unchecked) =>
            BinSelectDef(lop, FunSelectDef(Nil, Nil, rop), b)
          case (lop, rop) => b.copy(lop = lop, rop = rop)
        }
      case ChildQuery(q: Exp @unchecked, db) =>
        val nctx = ctx.copy(db = db)
        val exp = q match {
          //recursive expression
          case a: Arr => RecursiveDef(tr_with_c(nctx, BodyCtx, a))
          //ordinary child
          case _ => tr_with_c(nctx, QueryCtx, q)
        }
        ChildDef(exp, db)
      case Braces(exp: Exp @unchecked) if ctx.ctx == TablesCtx => tr(ctx, exp) //remove braces around table expression, so it can be accessed directly
      case Braces(exp: Exp @unchecked) => tr(ctx, exp) match {
        case sdb: SelectDefBase => BracesSelectDef(sdb)
        case e => Braces(e)
      }
      case a: Arr if ctx.ctx == QueryCtx => ArrayDef(
        a.elements.zipWithIndex.map { case (el, idx) =>
          ColDef(
            s"_${idx + 1}",
            tr(ctx, el) match {
              case r: RowDefBase => ChildDef(r, None)
              case e => e
            },
            ExprType())
        }
      )
      case dml: DMLExp =>
        val db = dml.db
        val nctx = ctx.copy(db = db)
        val table = TableDef(if (dml.alias == null) dml.table.ident mkString "." else dml.alias,
          Obj(TableObj(dml.table), null, null, null))
        val cols =
          if (dml.cols != null) dml.cols.map {
            case c @ ast.Col(Obj(_: Ident, _, _, _, _), _) => tr_with_c(nctx, ColsCtx, c) //insertable, updatable col
            case c @ ast.Col(BinOp("=", Obj(_: Ident, _, _, _, _), _), _) if dml.isInstanceOf[Update] =>
              tr_with_c(nctx, ColsCtx, c) //updatable col
            case c @ast.Col(Fun("if_defined", List(_, Obj(_: Ident, _, _, _, _)), _, _, _), _) =>
              tr_with_c(nctx, ColsCtx, c) //optional column
            case c => tr_with_c(nctx, QueryCtx, c) //child expression
          }.asInstanceOf[List[ColDef]]
          else Nil
        val filter = if (dml.filter != null) tr_with_c(nctx, BodyCtx, dml.filter).asInstanceOf[Arr] else null
        val vals = if (dml.vals != null) tr_with_c(nctx, BodyCtx, dml.vals) else null
        val retCols = dml.returning.map(buildCols(nctx, _))
        val dmlDef = dml match {
          case i: Insert =>
            val ic = tr_with_c(nctx, InsConflCtx, i.insertConflict).asInstanceOf[InsertConflict]
            InsertDef(cols, List(table), Insert(table = null, alias = null, cols = Nil, vals = vals, None, db, ic))
          case _: Update =>
            UpdateDef(cols, List(table), Update(
              table = null, alias = null, cols = Nil, filter = filter, vals = vals, returning = None, db = db))
          case _: Delete =>
            DeleteDef(List(table), Delete(table = null, alias = null, filter = filter, using = vals, None, db))
        }
        retCols
          .map(rc =>
            ReturningDMLDef(rc,
              vals match {
                case x: ValuesFromSelectDef => x.tables
                case _ => List(table)
              },
              dmlDef))
          .getOrElse(dmlDef)
      case ValuesFromSelect(sel) =>
        ValuesFromSelectDef(tr_with_c(ctx, QueryCtx, sel).asInstanceOf[SelectDefBase])
      case WithTable(name, wtCols, recursive, table) =>
        val exp = tr_with_c(ctx, QueryCtx, table) match {
          case s: SQLDefBase => s
          case x => error(s"Table in with clause must be query. Instead found: ${x.getClass.getName}(${x.tresql})")
        }
        val tables: List[TableDef] = List(TableDef(name, Obj(TableObj(Ident(List(name))), null, null, null)))
        val cols =
          wtCols.map { c =>
            ColDef(c, Ident(List(c)), ExprType())
          }
        WithTableDef(cols, tables, recursive, exp)
      case With(tables, query) =>
        val withTables = (tables map builder(ctx)).asInstanceOf[List[WithTableDef]]
        tr_with_c(ctx, QueryCtx, query) match {
          case s: SelectDefBase => WithSelectDef(s, withTables)
          case i: DMLDefBase => WithDMLDef(i, withTables)
          case x => error(s"with clause must be select query. Instead found: ${x.getClass.getName}(${x.tresql})")
        }
      case null => null
    })
    tr(BuildCtx(QueryCtx, None), exp)
  }

  // implicit conversion from SQLDefBase to Scope
  import ExpToScope.{expToScope, expListToScopeList}

  def resolveColAsterisks(exp: Exp) = {
    def createCol(col: String): ast.Col =
      phrase(column)(new scala.util.parsing.input.CharSequenceReader(col)).get

    case class Ctx(scopes: List[Scope], db: Option[String])

    lazy val resolver: TransformerWithState[Ctx] = transformerWithState { ctx =>
      def resolveWithQuery[T <: SQLDefBase](withQuery: WithQuery): (T, List[WithTableDef]) = {
        val wtables: List[WithTableDef] = withQuery.withTables.foldLeft(List[WithTableDef]()) { (tables, table) =>
          val nctx = ctx.copy(scopes = ExpToScope.expListToScopeList(tables) ++ ctx.scopes)
          (resolver(if (table.recursive) ctx.copy(scopes = table :: ctx.scopes) else nctx)(table) match {
            case wtd @ WithTableDef(Nil, _, _, _) =>
              wtd.copy(cols = wtd.exp.cols.map(col =>
                ColDef(
                  col.name,
                  Obj(Ident(List(wtd.tables.head.name, col.name)), null, null, null),
                  col.typ
                )))
            case x: WithTableDef =>
              if (x.exp.cols.size != x.cols.size)
                error(
                  s"""Column count mismatch in column name list:
                     | ${x.exp.cols.size} != ${x.cols.size}""".stripMargin)
              else x
            case x => sys.error(s"Compiler error, expected WithTableDef, encountered: $x")
          }) :: tables
        }
        val exp = resolver(ctx.copy(scopes =
          ExpToScope.expListToScopeList(wtables) ++ ctx.scopes))(withQuery.exp).asInstanceOf[T]
        (exp, wtables.reverse)
      }
      def resolveCols(ctx: Ctx, sql: SQLDefBase) = {
        sql.cols.flatMap {
          case ColDef(_, All, _) => sql.tables.flatMap { td =>
            table(ctx.scopes)(td.name)(EnvMetadata, ctx.db).map(_.cols.map { c =>
              ColDef(c.name, createCol(s"${td.name}.${c.name}").col, c.colType)
            }).getOrElse(error(s"Cannot find table: ${td.name}\nScopes:\n${ctx.scopes}"))
          }
          case ColDef(_, IdentAll(Ident(ident)), _) =>
            val alias = ident mkString "."
            table(ctx.scopes)(alias)(EnvMetadata, ctx.db)
              .map(_.cols.map { c => ColDef(c.name, createCol(s"$alias.${c.name}").col, c.colType) })
              .getOrElse(error(s"Cannot find table: $alias"))
          case ColDef(n, e, t) => List(ColDef(n, resolver(ctx)(e), t))
        }
      }

      {
        case sd: SelectDef =>
          val nsd = sd.copy(tables = {
            sd.tables.map {
              case td @ TableDef(_, Obj(TableObj(_: SelectDefBase), _, _, _, _)) =>
                resolver(ctx)(td).asInstanceOf[TableDef]
              case td => td
            }
          })
          val nctx = ctx.copy(nsd :: ctx.scopes)
          nsd.copy (
            cols = resolveCols(nctx, nsd),
            exp = resolver(nctx)(nsd.exp).asInstanceOf[Query]
          )
        case rd: ReturningDMLDef =>
          rd.copy(
            cols = resolveCols(ctx.copy(scopes = rd :: ctx.scopes), rd),
            exp = resolver(ctx)(rd.exp).asInstanceOf[DMLDefBase]
          )
        case wsd: WithSelectDef =>
          val (exp, wtables) = resolveWithQuery[SelectDefBase](wsd)
          wsd.copy(exp, wtables)
        case wdmld: WithDMLDef =>
          val (exp, wtables) = resolveWithQuery[DMLDefBase](wdmld)
          wdmld.copy(exp, wtables)
        case ins @ InsertDef(List(ColDef(_, All, _)), _, exp) =>
          val nexp = resolver(ctx)(exp).asInstanceOf[Insert]
          nexp.vals match {
            case s: SQLDefBase =>
              val cols = s.cols map { c =>
                if (c.name == null)
                  error("Null column name in select for insert with asterisk columns")
                else ColDef(c.name, Ident(List(c.name)), ExprType.Any)
              }
              ins.copy(cols = cols, exp = nexp)
            case _ => ins.copy(exp = nexp)
          }
        case ChildDef(exp, db) => ChildDef(resolver(ctx.copy(db = db))(exp), db)
      }
    }
    resolver(Ctx(Nil, None))(exp)
  }

  def resolveNamesAndJoins(exp: Exp) = {
    trait Ctx
    object TableCtx extends Ctx
    object ColumnCtx extends Ctx
    //isGrpOrd indicates group, order clauses where column clause column aliases can be referenced
    case class Context(colScopes: List[Scope],
                       tblScopes: List[Scope],
                       ctx: Ctx, isGrpOrd: Boolean,
                       withTableMetadata: Option[WithTableMetadata],
                       dbs: List[Option[String]]) {
      def tableMetadata: TableMetadata = withTableMetadata.getOrElse(EnvMetadata)
      def addTable(scope: Scope) = withTableMetadata
        .map { case WithTableMetadata(scopes) => WithTableMetadata(scope :: scopes)}
        .map (wtmd => this.copy(withTableMetadata = Some(wtmd)))
        .getOrElse(this)
      def withMetadata = withTableMetadata.map(_ => this)
        .getOrElse(this.copy(withTableMetadata = Some(WithTableMetadata(Nil))))
      def db = dbs.headOption.flatten
    }
    def checkDefaultJoin(scopes: List[Scope], table1: TableDef, table2: TableDef, db: Option[String]) = {
      if (table1 != null) {
        for {
          t1 <- table(scopes)(table1.name)(EnvMetadata, db)
          t2 <- table(scopes)(table2.name)(EnvMetadata, db)
        } yield try metadata.join(t1.name, t2.name) catch {
          case e: Exception => error(e.getMessage, cause = e)
        }
      }
    }
    lazy val namer: Traverser[Context] = traverser(ctx => {
      case sd: SelectDef =>
        val nctx = ctx.copy(colScopes = sd :: ctx.colScopes, isGrpOrd = false)
        var prevTable: TableDef = null
        sd.tables foreach { t =>
          namer(ctx)(t.exp.obj) //table definition check goes within parent scope
          Option(t.exp.join).map { j =>
            //join definition check goes within this select scope
            namer(nctx)(j)
            j match {
              case Join(true, _, _) => checkDefaultJoin(nctx.tblScopes, prevTable, t, ctx.db)
              case _ =>
            }
          }
          prevTable = t
        }
        sd.cols foreach namer(nctx)
        namer(nctx)(sd.exp.cols.distinct)
        namer(nctx)(sd.exp.filter)
        val grpOrdCtx = nctx.copy(isGrpOrd = true)
        Option(sd.exp.group).foreach { g =>
          g.cols.foreach(namer(grpOrdCtx))
          namer(nctx)(g.having)
        }
        namer(grpOrdCtx)(sd.exp.order)
        namer(nctx)(sd.exp.offset)
        namer(nctx)(sd.exp.limit)
        //return old scope
        ctx
      case wtd: WithTableDef if wtd.recursive =>
        namer(ctx.addTable(wtd))(wtd.exp)
        ctx
      case wsd: WithQuery =>
        val wtCtx = wsd.withTables.foldLeft(ctx.withMetadata) {
          case (ctx, table @ WithTableDef(_, _, _, _: SelectDefBase)) => //add scope only for select definition
            namer(ctx)(table)
            ctx.addTable(table)
          case (ctx, _) => ctx
        }
        namer(wtCtx)(wsd.exp)
        ctx
      case dml: DMLDefBase =>
        val dmlTableCtx = Context(Nil, Nil, TableCtx, isGrpOrd = false, None, List(dml.db)) // root scope
        val dmlColCtx = Context(colScopes = List(dml), tblScopes = Nil, ColumnCtx, isGrpOrd = false, None, List(dml.db)) // dml table scope
        val thisCtx = ctx.copy(colScopes = dml :: ctx.colScopes, dbs = dml.db :: ctx.dbs)
        dml.tables foreach (t => namer(dmlTableCtx)(t.exp.obj)) // dml table must come from root scope
        dml match {
          case ins: InsertDef =>
            dml.cols foreach namer(dmlColCtx) // dml columns must come from dml table
            namer(ctx)(ins.exp.vals) //do not change scope for insert value clause name resolving
            val ic = ins.exp.insertConflict
            if (ic != null) {
              val tableName = if (ic.valuesAlias == null) "excluded" /*pg style name*/ else ic.valuesAlias
              val tableMap = (tn: String) => if (tn == tableName) Option(dml.tableNames.head) else None
              val colMap: String => String => Option[String] = {
                val c = dml.cols.map(_.name)
                val m =
                  (c zip (if (ic.valuesCols.nonEmpty) ic.valuesCols.map(_.name) else c)).toMap
                tn => if (tn == tableName) m.get else _ => None
              }
              val icScope = ExpToScope.TranslatedScope(List(tableName), tableMap, colMap)(
                thisCtx.tableMetadata, thisCtx.db)
              val icCtx = thisCtx.copy(colScopes = icScope :: thisCtx.colScopes)
              val ict = ic.conflictTarget
              if (ict != null) namer(icCtx)(ict.filter)
              val ica = ic.conflictAction
              if (ica != null) {
                ica.cols foreach namer(dmlColCtx)
                namer(icCtx)(ica.vals)
                namer(icCtx)(ica.filter)
              }
            }
          case upd: UpdateDef => upd.exp.vals match {
            case vfsd: ValuesFromSelectDef =>
              namer(ctx)(vfsd) //vals clause
              val colsCtx = thisCtx.copy(colScopes = vfsd :: thisCtx.colScopes)
              upd.cols foreach {
                // column must consist of assignement operation, on left side of which must be updateable column
                case ColDef(_, BinOp("=" ,lop @ Obj(Ident(_), _, _, _, _), rop), _) =>
                  namer(dmlColCtx)(lop) //updateable column must come from update statement table
                  namer(colsCtx)(rop)
                case x => error(s"For updateable column clause expected assignement operation to column. Instead found: $x")
              }
              namer(colsCtx)(upd.exp.filter) //filter clause
            case _ =>
              dml.cols foreach namer(dmlColCtx) // dml columns must come from dml table
              namer(thisCtx)(upd.exp) //filters and vals check with this context
          }
          case del: DeleteDef => del.exp.using match {
            case u: ValuesFromSelectDef =>
              namer(ctx)(u) //using clause
              del.exp.returning.map(namer(thisCtx)) //returning clause
              val filterCtx = thisCtx.copy(colScopes = u :: thisCtx.colScopes)
              namer(filterCtx)(del.exp.filter) //filter clause, can have references from tables in using clause
            case _ => namer(thisCtx)(del.exp)
          }
        }
        //return old scope
        ctx
      case rdml: ReturningDMLDef =>
        val tableCtx = Context(Nil, Nil, TableCtx, isGrpOrd = false, None, List(rdml.exp.db)) // root scope
        namer(tableCtx)(rdml.tables.head.exp.obj) //first table must come from root scope since it is modified
        rdml.tables.tail foreach (t => namer(ctx)(t.exp.obj))
        val colCtx = ctx.copy(colScopes = List(rdml), dbs = List(rdml.exp.db))
        rdml.cols foreach namer(colCtx)
        namer(ctx)(rdml.exp)
        ctx
      case t: TableObj => namer(ctx.copy(ctx = TableCtx))(t.obj) //set table context
      case _: TableAlias => ctx //do not check table alias is already checked
      case o: Obj => namer(namer(ctx.copy(ctx = ColumnCtx))(o.join))(o.obj) //set column context
      case Ident(ident) if ctx.ctx == TableCtx => //check table
        val tn = ident mkString "."
        table(ctx.tblScopes)(tn)(ctx.tableMetadata, ctx.db).orElse(error(s"Unknown table: $tn"))
        ctx
      case Ident(ident) if ctx.ctx == ColumnCtx => //check column
        val cn = ident mkString "."
        if (ctx.dbs.distinct.flatMap { db =>
          (if (ctx.isGrpOrd) declaredColumn(ctx.colScopes)(cn)(ctx.tableMetadata, db) else
            column(ctx.colScopes)(cn)(ctx.tableMetadata, db)).toList
        } isEmpty) error(s"Unknown column: $cn")
        ctx
      case ChildDef(exp, db) => namer(ctx.copy(dbs = db :: ctx.dbs))(exp)
    })
    namer(Context(Nil, Nil, ColumnCtx, isGrpOrd = false, None, List(None)))(exp)
    exp
  }

  def resolveColTypes(exp: Exp) = {
    case class Ctx(scopes: List[Scope], db: Option[String], exprType: ExprType)

    def value_to_ctx_with_type(value: Any) = {
      def expr_type(clazz: Class[_]) = {
        val mfName = Manifest.classType(clazz).toString
        ExprType(metadata.from_jdbc_type(TypeMapper.scalaToJdbc(mfName)))
      }

      val et = value match {
        case c: Const => c match {
            case _: StringConst => expr_type(classOf[String])
            case _: BooleanConst => expr_type(classOf[java.lang.Boolean])
            case _: IntConst => expr_type(classOf[java.lang.Integer])
            case _: BigDecimalConst => expr_type(classOf[scala.math.BigDecimal])
          }
        case _: Boolean => expr_type(classOf[java.lang.Boolean])
        case t: ExprType => t
        case _ => expr_type(classOf[Object])
      }
      Ctx(Nil, None, et)
    }

    def manifestFromExprType(exprType: ExprType) = metadata.to_scala_type(exprType.toString) match {
      case "Any" => Manifest.Any
      case "Unit" => Manifest.Unit
      case "Array[Byte]" => Manifest.classType(classOf[Array[Byte]])
      case mf => Manifest.classType(Class.forName(mf))
    }

    val s_mf = Manifest.classType(classOf[String])
    val b_mf = Manifest.classType(classOf[java.lang.Boolean])

    lazy val typer: Traverser[Ctx] = traverser(ctx => {
      case c: Const => value_to_ctx_with_type(c)
      case _: Null => value_to_ctx_with_type(Null)
      case Ident(ident) =>
        Ctx(ctx.scopes, ctx.db, column(ctx.scopes)(ident mkString ".")(EnvMetadata, ctx.db).map(_.colType).get)
      case UnOp(_, operand) => typer(ctx)(operand)
      case BinOp(op, lop, rop) =>
        comp_op.findAllIn(op).toList match {
          case Nil => lop match {
            case _: Variable => typer(ctx)(rop) //variable is Any, try right operand
            case l => rop match {
              case _: Variable => typer(ctx)(l) //variable is Any try left operand
              case r =>
                val (lt, rt) = (typer(ctx)(l).exprType, typer(ctx)(r).exprType)
                val mf =
                  if (manifestFromExprType(lt) == s_mf) lt else if (manifestFromExprType(rt) == s_mf) rt
                  else if (manifestFromExprType(lt) == b_mf) lt else if (manifestFromExprType(rt) == b_mf) rt
                  else if (manifestFromExprType(lt) <:< manifestFromExprType(rt)) rt else lt
                Ctx(ctx.scopes, ctx.db, mf)
            }
          }
          case _ => value_to_ctx_with_type(true)
        }
      case _: TerOp => value_to_ctx_with_type(true)
      case _: In => value_to_ctx_with_type(true)
      case s: SelectDefBase =>
        if (s.cols.size > 1)
          error(s"Select must contain only one column, instead:${
            s.cols.map(_.tresql).mkString(", ")}")
        else value_to_ctx_with_type(s.cols.head.typ)
      case f: FunDef =>
        if (f.typ != null && f.typ != ExprType()) value_to_ctx_with_type(f.typ)
        else f.procedure.returnType match {
          case FixedReturnType(tn) => value_to_ctx_with_type(tn)
          case ParameterReturnType(idx) =>
            if (idx == -1) value_to_ctx_with_type(ExprType.Any) else typer(ctx)(f.exp.parameters(idx))
        }
      case Cast(_, typ) => value_to_ctx_with_type(ExprType(typ))
      case PrimitiveDef(e, _) => typer(ctx)(e)
    })
    case class ResolverCtx(scopes: List[Scope], db: Option[String])
    lazy val type_resolver: TransformerWithState[ResolverCtx] = transformerWithState { ctx =>
      def resolveWithQuery[T <: SQLDefBase](withQuery: WithQuery): (T, List[WithTableDef]) = {
        val wtables = withQuery.withTables.foldLeft(List[WithTableDef]()) { (tables, table) =>
          type_resolver(ctx.copy(scopes =
            ExpToScope.expListToScopeList(tables) ++ ctx.scopes))(table).asInstanceOf[WithTableDef] :: tables
        }
        (type_resolver(ctx.copy(scopes =
          ExpToScope.expListToScopeList(wtables) ++ ctx.scopes))(withQuery.exp).asInstanceOf[T], wtables.reverse)
      }
      {
        case s: SelectDef =>
          //resolve column types for potential from clause select definitions
          val nsd = s.copy(tables =
            (s.tables map type_resolver(ctx.copy(scopes = s :: ctx.scopes))).asInstanceOf[List[TableDef]])
          //resolve types for column defs
          nsd.copy(cols =
            (nsd.cols map type_resolver(ctx.copy(scopes = nsd :: ctx.scopes))).asInstanceOf[List[ColDef]])
        case wtd: WithTableDef =>
          val nctx = if (wtd.recursive) ctx.copy(scopes = wtd :: ctx.scopes) else ctx
          val exp = type_resolver(nctx)(wtd.exp).asInstanceOf[SQLDefBase]
          val cols = wtd.cols zip exp.cols map { case (col, ecol) => col.copy(typ = ecol.typ) }
          wtd.copy(cols = cols, exp = exp)
        case wsd: WithSelectDef =>
          val (exp, wtables) = resolveWithQuery[SelectDefBase](wsd)
          wsd.copy(exp = exp, withTables = wtables)
        case wdmld: WithDMLDef =>
          val (exp, wtables) = resolveWithQuery[DMLDefBase](wdmld)
          wdmld.copy(exp = exp, withTables = wtables)
        case dml: DMLDefBase if ctx.scopes.isEmpty || !ctx.scopes.head.isEqual(dml) =>
          type_resolver(ctx.copy(scopes = dml :: ctx.scopes, db = dml.db))(dml)
        case rdml: ReturningDMLDef =>
          //resolve column types for potential from clause select definitions
          val nrdml = rdml.copy(tables =
            (rdml.tables map type_resolver(ctx.copy(scopes = rdml :: ctx.scopes, db = rdml.exp.db)))
              .asInstanceOf[List[TableDef]])
          //resolve types for column defs
          nrdml.copy(cols =
            (nrdml.cols map type_resolver(ctx.copy(scopes = nrdml :: ctx.scopes, db = rdml.exp.db)))
              .asInstanceOf[List[ColDef]])
        case ColDef(n, ChildDef(ch, db), t) =>
          ColDef(n, ChildDef(type_resolver(ctx.copy(db = db))(ch), db), t)
        case ColDef(n, exp, typ) if typ == null || typ == ExprType() =>
          val nexp = type_resolver(ctx)(exp) //resolve expression in the case it contains select
          ColDef(n, nexp, typer(Ctx(ctx.scopes, ctx.db, ExprType.Any))(nexp).exprType)
        //resolve return type only for root level function
        case fd @ FunDef(_, f, typ, p) if ctx.scopes.isEmpty && (typ == null || typ == ExprType()) =>
          val t = p.returnType match {
            case ParameterReturnType(idx) =>
              if (idx == -1) ExprType.Any
              else typer(Ctx(ctx.scopes, ctx.db, ExprType.Any))(f.parameters(idx)).exprType
            case FixedReturnType(mf) => mf
          }
          fd.copy(typ = t)
        case PrimitiveDef(exp, _) =>
          val res_exp = type_resolver(ctx)(exp)
          PrimitiveDef(res_exp, typer(Ctx(ctx.scopes, ctx.db, ExprType.Any))(res_exp).exprType)
        case ChildDef(ch, db) => ChildDef(type_resolver(ctx.copy(db = db))(ch), db)
      }
    }
    type_resolver(ResolverCtx(Nil, None))(exp)
  }

  /** Override not to take into account existance of builder macros */
  override protected def isMacro(name: String): Boolean = macros != null && macros.isMacroDefined(name)

  def compile(exp: Exp) = {
    def normalized(e: Exp): Exp = {
      def isPrimitiveType(e: Exp) = e match {
        case _: Fun | _: Const | _: Variable | _: Cast | _: UnOp | _: Null | _: In => true
        case _ => false
      }
      def isPrimitive(e: Exp): Boolean = e match {
        case BinOp("++", _, _) => false // union all
        case BinOp("&&", _, _) => false // intersect
        case BinOp("=", _: Variable, _) => false // assign expression
        case BinOp(_, l, r) => isPrimitive(l) || isPrimitive(r)
        case Braces(b) => isPrimitive(b)
        case f: Fun =>
          f.name != "sql_concat" && macros != null && !macros.isBuilderMacroDefined(f.name)
        case x => isPrimitiveType(x)
      }
      e match {
        case Arr(exps) => Arr(exps map normalized)
        case _ if isPrimitive(e) =>
          PrimitiveExp(phrase(query)(
            new scala.util.parsing.input.CharSequenceReader(s"{${e.tresql}}")).get)
        case _ => e
      }
    }
    resolveColTypes(
      resolveNamesAndJoins(
        resolveColAsterisks(
          buildTypedDef(normalized(exp)))))
  }

  override def transformer(fun: Transformer): Transformer = {
    lazy val local_transformer = fun orElse traverse
    lazy val transform_traverse = local_transformer orElse super.transformer(local_transformer)
    def tt[A <: Exp](exp: A) = transform_traverse(exp).asInstanceOf[A]
    lazy val traverse: Transformer = {
      case cd: ColDef => cd.copy(col = tt(cd.col))
      case cd: ChildDef => cd.copy(exp = tt(cd.exp))
      case fd: FunDef => fd.copy(exp = tt(fd.exp))
      case ftd: FunAsTableDef => ftd.copy(exp = tt(ftd.exp))
      case td: TableDef => td.copy(exp = tt(td.exp))
      case to: TableObj => to.copy(obj = tt(to.obj))
      case ta: TableAlias => ta.copy(obj = tt(ta.obj))
      case PrimitiveExp(q) => PrimitiveExp(tt(q))
      case PrimitiveDef(exp, typ) => PrimitiveDef(tt(exp), typ)
      case sd: SelectDef =>
        val t = sd.tables map tt
        val c = sd.cols map tt
        val q = tt(sd.exp)
        sd.copy(cols = c, tables = t, exp = q)
      case bd: BinSelectDef => bd.copy(
        leftOperand = tt(bd.leftOperand),
        rightOperand = tt(bd.rightOperand)
      )
      case id: InsertDef =>
        val t = id.tables map tt
        val c = id.cols map tt
        val i = tt(id.exp)
        InsertDef(c, t, i)
      case ud: UpdateDef =>
        val t = ud.tables map tt
        val c = ud.cols map tt
        val u = tt(ud.exp)
        UpdateDef(c, t, u)
      case dd: DeleteDef => DeleteDef(dd.tables map tt, tt(dd.exp))
      case ad: ArrayDef => ad.copy(cols = ad.cols map tt)
      case rd: RecursiveDef => rd.copy(exp = tt(rd.exp))
      case wtd: WithTableDef => wtd.copy(
        cols = wtd.cols map tt,
        tables = wtd.tables map tt,
        exp = tt(wtd.exp)
      )
      case wsd: WithSelectDef => WithSelectDef(tt(wsd.exp), wsd.withTables map tt)
      case wdml: WithDMLDef => WithDMLDef(tt(wdml.exp), wdml.withTables map tt)
      case rdml: ReturningDMLDef =>
        ReturningDMLDef(
          cols = rdml.cols map tt,
          tables = rdml.tables map tt,
          exp = tt(rdml.exp)
        )
      case BracesSelectDef(sdb) => BracesSelectDef(tt(sdb))
      case fsd: FunSelectDef => fsd.copy(exp = tt(fsd.exp))
      case vfsd: ValuesFromSelectDef => vfsd.copy(exp = tt(vfsd.exp))
    }
    transform_traverse
  }

  override def transformerWithState[T](fun: TransformerWithState[T]): TransformerWithState[T] = {
    lazy val local_transformer: TransformerWithState[T] = (state: T) => fun(state) orElse traverse(state)
    def transform_traverse(state: T): Transformer =
      local_transformer(state) orElse super.transformerWithState(local_transformer)(state)
    def tt[A <: Exp](state: T)(exp: A) = transform_traverse(state)(exp).asInstanceOf[A]
    def traverse(state: T): Transformer = {
      case cd: ColDef => cd.copy(col = tt(state)(cd.col))
      case cd: ChildDef => cd.copy(exp = tt(state)(cd.exp))
      case fd: FunDef => fd.copy(exp = tt(state)(fd.exp))
      case ftd: FunAsTableDef => ftd.copy(exp = tt(state)(ftd.exp))
      case td: TableDef => td.copy(exp = tt(state)(td.exp))
      case to: TableObj => to.copy(obj = tt(state)(to.obj))
      case ta: TableAlias => ta.copy(obj = tt(state)(ta.obj))
      case PrimitiveExp(q) => PrimitiveExp(tt(state)(q))
      case PrimitiveDef(exp, typ) => PrimitiveDef(tt(state)(exp), typ)
      case sd: SelectDef =>
        val t = sd.tables map tt(state)
        val c = sd.cols map tt(state)
        val q = tt(state)(sd.exp)
        sd.copy(cols = c, tables = t, exp = q)
      case bd: BinSelectDef => bd.copy(
        leftOperand = tt(state)(bd.leftOperand),
        rightOperand = tt(state)(bd.rightOperand)
      )
      case id: InsertDef =>
        val t = id.tables map tt(state)
        val c = id.cols map tt(state)
        val i = tt(state)(id.exp)
        InsertDef(c, t, i)
      case ud: UpdateDef =>
        val t = ud.tables map tt(state)
        val c = ud.cols map tt(state)
        val u = tt(state)(ud.exp)
        UpdateDef(c, t, u)
      case dd: DeleteDef => DeleteDef(dd.tables map tt(state), tt(state)(dd.exp))
      case ad: ArrayDef => ad.copy(cols = ad.cols map tt(state))
      case rd: RecursiveDef => rd.copy(exp = tt(state)(rd.exp))
      case wtd: WithTableDef => wtd.copy(
        cols = wtd.cols map tt(state),
        tables = wtd.tables map tt(state),
        exp = tt(state)(wtd.exp)
      )
      case wsd: WithSelectDef => WithSelectDef(tt(state)(wsd.exp), wsd.withTables map tt(state))
      case wdml: WithDMLDef => WithDMLDef(tt(state)(wdml.exp), wdml.withTables map tt(state))
      case rdml: ReturningDMLDef =>
        ReturningDMLDef(
          rdml.cols map tt(state),
          rdml.tables map tt(state),
          tt(state)(rdml.exp)
        )
      case BracesSelectDef(sdb) => BracesSelectDef(tt(state)(sdb))
      case fsd: FunSelectDef => fsd.copy(exp = tt(state)(fsd.exp))
      case vfsd: ValuesFromSelectDef => vfsd.copy(exp = tt(state)(vfsd.exp))
    }
    transform_traverse
  }

  override def traverser[T](fun: Traverser[T]): Traverser[T] = {
    lazy val local_traverse: Traverser[T] = (state: T) => fun(state) orElse traverse(state)
    def fun_traverse(state: T) = local_traverse(state) orElse super.traverser(local_traverse)(state)
    //helper functions
    def tr(state: T, e: Exp): T = fun_traverse(state)(e)
    def trl(state: T, l: List[Exp]) = l.foldLeft(state) { (fr, el) => tr(fr, el) }
    def traverse(state: T): PartialFunction[Exp, T] = {
      case cd: ColDef => tr(state, cd.col)
      case cd: ChildDef => tr(state, cd.exp)
      case fd: FunDef => tr(state, fd.exp)
      case ftd: FunAsTableDef => tr(state, ftd.exp)
      case td: TableDef => tr(state, td.exp)
      case to: TableObj => tr(state, to.obj)
      case ta: TableAlias => tr(state, ta.obj)
      case bd: BinSelectDef => tr(tr(state, bd.leftOperand), bd.rightOperand)
      case ad: ArrayDef => trl(state, ad.cols)
      case rd: RecursiveDef => tr(state, rd.exp)
      case wtd: WithTableDef => tr(state, wtd.exp)
      case wq: WithQuery => tr(trl(state, wq.withTables), wq.exp)
      case bsd: BracesSelectDef => tr(state, bsd.exp)
      case fsd: FunSelectDef => tr(state, fsd.exp)
      case vfsd: ValuesFromSelectDef => tr(state, vfsd.exp)
      case sql: SQLDefBase => tr(trl(trl(state, sql.tables), sql.cols), sql.exp)
      case PrimitiveExp(q) => tr(state, q)
      case PrimitiveDef(exp, _) => tr(state, exp)
    }
    fun_traverse
  }

  override def parseExp(expr: String): Exp = try {
    super.parseExp(expr)
  } catch {
    case e: Exception => error(e.getMessage, cause = e)
  }
}
