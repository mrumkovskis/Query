package org.tresql

import org.tresql.OrtMetadata.{Filters, SaveOptions, SaveTo, TresqlValue, View, ViewValue}

import sys._

/** Object Relational Transformations - ORT */
trait ORT extends Query {

  /**
  *  Children property format:
  *  table[:ref]*[#table[:ref]*]*[[options]] [alias][|insertFilterTresql, deleteFilterTresql, updateFilterTresql]
  *  Options are to be specified in such order: insert, delete, update, i.e. '+-='
  *  Examples:
  *    dept#car:deptnr:nr#tyres:carnr:nr
  *    dept[+=] alias
  *    emp[+-=] e|:deptno in currentUserDept(:current_user), null /* no statement */, e.deptno in currentUserDept(:current_user)
  */
  val PropPattern = {
    val ident = """[^:\[\]\s#]+"""
    val table = s"$ident(?::$ident)*"
    val tables = s"""($table(?:#$table)*)"""
    val options = """(?:\[(\+?-?=?)\])?"""
    val alias = """(?:\s+(\w+))?"""
    val filters = """(?:\|(.+))?"""
    (tables + options + alias + filters)r
  }
  /**
  *  Resolvable property key value format:
  *  key: "saveable_property->", value: "saveable_column=tresql_expression"
  *  Resolver tresqls where clause can contain placeholder _
  *  which will be replaced with bind variable ':saveable_property'.
  *  Statement must return no more than one row.
  *  Example:
  *    "dept_name->" -> "dept_id=dept[dname = _]{id}"
  */
  val ResolverPropPattern = "(.*?)->"r
  val ResolverExpPattern = "([^=]+)=(.*)"r

  type ObjToMapConverter[T] = (T) => (String, Map[String, _])

  /** QueryBuilder methods **/
  override private[tresql] def newInstance(e: Env, depth: Int, idx: Int, chIdx: Int) =
    new ORT {
      override def env = e
      override private[tresql] def queryDepth = depth
      override private[tresql] var bindIdx = idx
      override private[tresql] def childIdx = chIdx
    }

  case class ParentRef(table: String, ref: String)
  case class SaveContext(
    name: String,
    view: View,
    parents: List[ParentRef],
    table: metadata.Table,
    refToParent: String,
    pk: String)

  /* Expression is built only from macros to ensure ORT lookup editing. */
  case class LookupEditExpr(
    obj: String,
    idName: String,
    insertExpr: Expr,
    updateExpr: Expr)
  extends BaseExpr {
    override def apply() = env(obj) match {
      case m: Map[String @unchecked, Any @unchecked] =>
        if (idName != null && (m contains idName) && m(idName) != null) {
          val lookupObjId = m(idName)
          updateExpr(m)
          lookupObjId
        } else extractId(insertExpr(m))
      case null => null
      case x => error(s"Cannot set environment variables for the expression. $x is not a map.")
    }
    def extractId(result: Any) = result match {
      case i: InsertResult => i.id.get //insert expression
      case a: ArrayResult[_] => a.values.last match { case i: InsertResult => i.id.get } //array expression
      case x => error(s"Unable to extract id from expr result: $x, expr: $insertExpr")
    }
    def defaultSQL = s"LookupEditExpr($obj, $idName, $insertExpr, $updateExpr)"
  }
  /* Expression is built from macros to ensure ORT children editing */
  case class InsertOrUpdateExpr(table: String, insertExpr: Expr, updateExpr: Expr)
  extends BaseExpr {
    val idName = env.table(table).key.cols.headOption.orNull
    private val upsertExpr = UpsertExpr(updateExpr, insertExpr)
    override def apply() =
      if (idName != null && env.containsNearest(idName) && env(idName) != null)
        upsertExpr() else insertExpr()
    def defaultSQL = s"InsertOrUpdateExpr($idName, $insertExpr, $updateExpr)"
  }
  /* Expression is built from macros to ensure ORT one to one relationship setting */
  case class UpsertExpr(updateExpr: Expr, insertExpr: Expr) extends BaseExpr {
    override def apply() = updateExpr() match {
      case r: DMLResult if r.count.getOrElse(0) > 0 || r.children.nonEmpty => r // return result if update affects some row
      case _ => insertExpr() // execute insert if update affects no rows
    }
    def defaultSQL = s"UpsertExpr($updateExpr, $insertExpr)"
  }
  /* Expression is built from macros to ensure ORT children editing */
  case class DeleteChildrenExpr(obj: String, table: String, expr: Expr)
  extends BaseExpr {
    val idName = env.table(table).key.cols.headOption.orNull
    override def apply() = {
      env(obj) match {
        case s: Seq[Map[String, _] @unchecked] =>
          expr(
            if (idName != null) Map("ids" -> s.map(_.getOrElse(idName, null)).filter(_ != null))
            else Map[String, Any]())
        case m: Map[String @unchecked, _] => expr(
          if (idName != null) Map("ids" -> m.get(idName).filter(_ != null).toList)
          else Map[String, Any]())
      }
    }
    override def defaultSQL = s"DeleteChildrenExpr($obj, $idName, $expr)"
  }
  case class NotDeleteIdsExpr(expr: Expr) extends BaseExpr {
    override def defaultSQL = env.get("ids").map {
      case ids: Seq[_] if ids.nonEmpty => expr.sql
      case _ =>
        //add 'ids' bind variable to query builder so it can be bound later in the case
        expr.sql
        "true"
    }.getOrElse("true")
  }
  /* Expression is built from macro.
   * Effectively env.currId(idSeq, IdRefExpr(idRefSeq)())*/
  case class IdRefIdExpr(idRefSeq: String, idSeq: String) extends BaseVarExpr {
    val idRefExpr = IdRefExpr(idRefSeq)
    override def apply() = {
      val id = idRefExpr()
      env.currId(idSeq, id)
      id
    }
    override def toString = s"$idRefExpr#$idSeq"
  }

  def insert(name: String, obj: Map[String, Any], filter: String = null)
    (implicit resources: Resources): Any = {
    val name_with_filter = if (filter == null) name else s"$name|$filter, null, null"
    save(name_with_filter, obj, insert_tresql, "Cannot insert data. Table not found for object: " + name)
  }

  def update(name: String, obj: Map[String, Any], filter: String = null)
    (implicit resources: Resources): Any = {
    val name_with_filter = if (filter == null) name else s"$name|null, null, $filter"
    save(name_with_filter, obj, update_tresql,
      s"Cannot update data. Table not found or no primary key or no updateable columns found for the object: $name")
  }

  def delete(name: String, id: Any, filter: String = null, filterParams: Map[String, Any] = null)
            (implicit resources: Resources): Any = {
    val Array(tableName, alias) = name.split("\\s+").padTo(2, null)
    val delete =
      (for {
        table <- resources.metadata.tableOption(tableName)
        pk <- table.key.cols.headOption
        if table.key.cols.size == 1
      } yield {
        s"-${table.name}${Option(alias).map(" " + _).getOrElse("")}[$pk = ?${Option(filter)
          .map(f => s" & ($f)").getOrElse("")}]"
      }) getOrElse {
        error(s"Table $name not found or table primary key not found or table primary key consists of more than one column")
      }
    build(delete, Map("1" -> id) ++ Option(filterParams).getOrElse(Map()),
      reusableExpr = false)(resources)()
  }

  /** insert methods to multiple tables
   *  Tables must be ordered in parent -> child direction. */
  def insertMultiple(obj: Map[String, Any], names: String*)(filter: String = null)
                    (implicit resources: Resources): Any = insert(names.mkString("#"), obj, filter)

  /** update to multiple tables
   *  Tables must be ordered in parent -> child direction. */
  def updateMultiple(obj: Map[String, Any], names: String*)(filter: String = null)
                    (implicit resources: Resources): Any = update(names.mkString("#"), obj, filter)

  //object methods
  def insertObj[T](obj: T, filter: String = null)(
    implicit resources: Resources, conv: ObjToMapConverter[T]): Any = {
    val v = conv(obj)
    insert(v._1, v._2, filter)
  }
  def updateObj[T](obj: T, filter: String = null)(
    implicit resources: Resources, conv: ObjToMapConverter[T]): Any = {
    val v = conv(obj)
    update(v._1, v._2, filter)
  }

  def insert(metadata: View, obj: Map[String, Any])(implicit resources: Resources): Any = {
    val tresql = insertTresql(metadata)
    build(tresql, obj, reusableExpr = false)(resources)()
  }

  def update(metadata: View, obj: Map[String, Any])(implicit resources: Resources): Any = {
    val tresql = updateTresql(metadata)
    build(tresql, obj, reusableExpr = false)(resources)()
  }

  def save(metadata: View, obj: Map[String, Any])(implicit resources: Resources): Any = {
    val tresql = s"_upsert(${updateTresql(metadata)}, ${insertTresql(metadata)})"
    build(tresql, obj, reusableExpr = false)(resources)()
  }

  def delete(name: String, key: List[String], params: Map[String, Any], filter: String)(implicit
                                                                                        resources: Resources): Any = {
    val tresql = deleteTresql(name, key, filter)
    build(tresql, params, reusableExpr = false)(resources)()
  }

  def insertTresql(metadata: View)(implicit resources: Resources): String = {
    save_tresql(null, metadata, Nil, insert_tresql)
  }

  def updateTresql(metadata: View)(implicit resources: Resources): String = {
    save_tresql(null, metadata, Nil, update_tresql)
  }

  def deleteTresql(name: String, key: List[String], filter: String)(implicit resources: Resources): String = {
    val Array(tableName, alias) = name.split("\\s+").padTo(2, null)
    resources.metadata.tableOption(tableName).map { table =>
      s"-${table.name}${if (alias == null) "" else " " + alias}[${
        key.map(c => c + " = :" + c).mkString(" & ")}${if (filter == null) "" else s" & ($filter)"}]"
    }.getOrElse(s"Table $name not found")
  }

  private def save(name: String,
                   obj: Map[String, Any],
                   save_tresql_fun: SaveContext => String,
                   errorMsg: String)(implicit resources: Resources): Any = {
    val view = ort_metadata(name, obj)
    resources log s"$view"
    val tresql = save_tresql(null, view, Nil, save_tresql_fun)
    if(tresql == null) error(errorMsg)
    build(tresql, obj, reusableExpr = false)(resources)()
  }

  private def ort_metadata(tables: String, saveableMap: Map[String, Any])(implicit resources: Resources): View = {
    def tresql_structure(obj: Map[String, Any]): Map[String, Any] = {
      def merge(lm: Seq[Map[String, Any]]): Map[String, Any] =
        lm.tail.foldLeft(tresql_structure(lm.head)) {(l, m) =>
          val x = tresql_structure(m)
          l map (t => (t._1, (t._2, x.getOrElse(t._1, null)))) map {
            case (k, (v1: Map[String @unchecked, _], v2: Map[String @unchecked, _])) if v1.nonEmpty && v2.nonEmpty =>
              (k, merge(List(v1, v2)))
            case (k, (v1: Map[String @unchecked, _], _)) if v1.nonEmpty => (k, v1)
            case (k, (_, v2: Map[String @unchecked, _])) if v2.nonEmpty => (k, v2)
            case (k, (v1, _)) => (k, v1 match {
              case _: Map[_, _] | _: Seq[_] => v1
              case _ if k endsWith "->" => v1
              case _ => "***"
            })
          }
        }
      var resolvableProps = Set[String]()
      val struct: Map[String, Any] = obj.map { case (key, value) =>
        (key, value match {
          case Seq() | Array() => Map()
          case l: Seq[Map[String, _] @unchecked] => merge(l)
          case l: Array[Map[String, _] @unchecked] => merge(l)
          case m: Map[String @unchecked, Any @unchecked] => tresql_structure(m)
          case x =>
            //filter out properties which are duplicated because of resolver
            if (key endsWith "->") {
              resolvableProps += key.substring(0, key.length - "->".length)
              value
            } else "***"
        })
      }
      if (resolvableProps.isEmpty) struct
      else struct.flatMap { case (k, _) if resolvableProps(k) => Nil case x => List(x) }
    }
    def parseTables(name: String) = {
      def multiSaveProp(names: Seq[String])(implicit resources: Resources) = {
        /* Returns zero or one imported key from table for each relation. In the case of multiple
         * imported keys pointing to the same relation the one specified after : symbol is chosen
         * or exception is thrown.
         * This is used to find relation columns for insert/update multiple methods.
         * Additionaly primary key of table is returned if it consist only of one column */
        def importedKeysAndPks(tableName: String, relations: List[String]) = {
          val x = tableName split ":"
          val table = resources.metadata.table(x.head)
          relations.foldLeft(x.tail.toSet) { (keys, rel) =>
            val relation = rel.split(":").head
            val refs = table.refs(relation).filter(_.cols.size == 1)
            (if (refs.size == 1) keys + refs.head.cols.head
            else if (refs.isEmpty || refs.exists(r => keys.contains(r.cols.head))) keys
            else error(s"Ambiguous refs: $refs from table ${table.name} to table $relation")) ++
              (table.key.cols match {case List(k) => Set(k) case _ => Set()})
          }
        }
        names.tail.foldLeft(List(names.head)) { (ts, t) => (t.split(":")
          .head + importedKeysAndPks(t, ts).mkString(":", ":", "")) :: ts
        }.reverse.mkString("#")
      }
      def setLinks(name: String) =
        if (name.indexOf("#") == -1) name else {
          val tablesEndIdx = {
            val oi = name.indexOf("[")
            if (oi != -1) oi else name.indexOf("|")
          }
          if (tablesEndIdx != -1) multiSaveProp(name.substring(0, tablesEndIdx)
            .split("#")) + name.substring(tablesEndIdx) else multiSaveProp(name.split("#"))
        }
      val PropPattern(tables, options, alias, filterStr) = setLinks(name)
      //insert update delete option
      val (i, u, d) = Option(options).map (_ =>
        (options contains "+", options contains "=", options contains "-")
      ).getOrElse {(true, false, true)}
      import parsing.{Arr, Null}
      val filters =
        Option(filterStr).flatMap(new QueryParser(resources, resources.cache).parseExp(_) match {
          case Arr(List(insert, delete, update)) => Some(Filters(
            insert = Some(insert).filter(_ != Null).map(_.tresql),
            delete = Some(delete).filter(_ != Null).map(_.tresql),
            update = Some(update).filter(_ != Null).map(_.tresql)
          ))
          case _ => error(s"""Unrecognized filter declaration '$filterStr'.
                             |Must consist of 3 comma separated tresql expressions: insertFilter, deleteFilter, updateFilter.
                             |In the case expression is not needed it must be set to 'null'.""".stripMargin)
        })
      View((tables split "#").map{ t =>
        val x = t split ":"
        SaveTo(x.head, x.tail.toSet, Nil)
      }.toList, SaveOptions(i, u, d), filters, alias, Nil)
    }
    def resolver_tresql(property: String, resolverExp: String) = {
      import parsing._
      val ResolverPropPattern(prop) = property
      val ResolverExpPattern(col, exp) = resolverExp
      val parser = new QueryParser(resources, resources.cache)
      OrtMetadata.Property(col, TresqlValue(
        parser.transformer {
          case Ident(List("_")) => Variable(prop, Nil, opt = false)
        } (parser.parseExp(if (exp startsWith "(" ) exp else s"($exp)")).tresql
      ))
    }
    val struct = tresql_structure(saveableMap)
    val props =
      struct.map {
        case (prop, value) => value match {
          case m: Map[String, Any] => OrtMetadata.Property(prop, ViewValue(ort_metadata(prop, m)))
          case v: String if prop.indexOf("->") != -1 => resolver_tresql(prop, v)
          case _ => OrtMetadata.Property(prop, TresqlValue(s":$prop"))
        }
      }.toList
    parseTables(tables).copy(properties = props)
  }

  private def save_tresql(name: String,
                          view: View,
                          parents: List[ParentRef],
                          save_tresql_func: SaveContext => String)(implicit
                                                                   resources: Resources): String = {
    val parent = parents.headOption.map(_.table).orNull
    val md = resources.metadata
    //find first imported key to parent in list of table links passed as a param.
    def importedKeyOption(tables: List[SaveTo]): Option[(metadata.Table, String)] = {
      def refInSet(refs: Set[String], child: metadata.Table) = refs.find(r =>
        child.refs(parent).filter(_.cols.size == 1).exists(_.cols.head == r))
      def imported_key_option(childTable: metadata.Table) =
        Option(childTable.refs(parent).filter(_.cols.size == 1)).flatMap {
          case Nil => None
          case List(ref) => ref.cols.headOption
          case x => error(
            s"""Ambiguous references from table '${childTable.name}' to table '$parent'.
             |Reference must be one and must consist of one column. Found: $x"""
              .stripMargin)
        }
      def processLinkedTables(linkedTables: List[SaveTo]): Option[(metadata.Table, String)] =
        linkedTables match {
          case table :: tail => md.tableOption(table.table)
            .flatMap(t =>
              imported_key_option(t)
                .filterNot(table.refs.contains) //linked table has ref to parent
                .map((t, _))) orElse processLinkedTables(tail)
          case Nil => None //no ref to parent
        }
      md.tableOption(tables.head.table) //no parent no ref to parent
        .filter(_ => parent == null)
        .map((_, null))
        .orElse {
          if (tables.head.refs.nonEmpty) //ref to parent in first table found must be resolved!
            md.tableOption(tables.head.table).flatMap(table => refInSet(tables.head.refs, table).map(table -> _))
          else
            processLinkedTables(tables) //search for ref to parent
        }
    }
    (for {
      (table, ref) <- importedKeyOption(view.saveTo)
      pk <- Some(table.key.cols).filter(_.size == 1).map(_.head) orElse Some(null)
    } yield
        save_tresql_func(SaveContext(name, view, parents, table, ref, pk))
    ).orNull
  }

  private def table_insert_tresql(
    tableName: String,
    alias: String,
    cols_vals: List[(String, String)],
    refsAndPk: Set[(String, String)],
    upsert: Boolean,
    maybeFilters: Option[Filters]
  ) = {
    cols_vals ++ refsAndPk match {
      case cols_vals =>
        val (cols, vals) = cols_vals.unzip
        cols.mkString(s"+$tableName {", ", ", "}") +
          (for {
            filters <- maybeFilters
            filter <- filters.insert
          } yield {
            val toa = if (alias == null) tableName else alias
            val cv = cols_vals filter (_._2 != null)
            val sel = s"(null{${cv.map(c => c._2 + " " + c._1).mkString(", ")}} @(1)) $toa"
            cv.map(c => s"$toa.${c._1}").mkString(s" $sel [$filter] {", ", ", "}")
          }).getOrElse(vals.filter(_ != null).mkString(" [", ", ", "]"))
    }
  }

  private def insert_tresql(ctx: SaveContext)
    (implicit resources: Resources): String = {
    save_tresql_internal(ctx, table_insert_tresql(_, _, _, _, _, ctx.view.filters),
      save_tresql(_, _, _, insert_tresql))
  }

  private def update_tresql(ctx: SaveContext)
    (implicit resources: Resources): String = {

    def stripTrailingAlias(tresql: String, alias: String) =
      if (tresql != null && alias != null && tresql.endsWith(alias))
        tresql.dropRight(alias.length) else tresql


    def table_save_tresql(tableName: String, alias: String,
      cols_vals: List[(String, String)],
      refsAndPk: Set[(String, String)],
      upsert: Boolean) = {
      val upd_tresql = cols_vals.unzip match {
        case (cols: List[String], vals: List[String]) if cols.nonEmpty =>
          val filter = ctx.view.filters.flatMap(_.update).map(f => s" & ($f)").getOrElse("")
          val tn = tableName + (if (alias == null) "" else " " + alias)
          val updateFilter = refsAndPk.map(t=> s"${t._1} = ${t._2}").mkString("[", " & ", s"$filter]")
          cols.mkString(s"=$tn $updateFilter {", ", ", "}") +
            vals.filter(_ != null).mkString("[", ", ", "]")
        case _ => null
      }

      Option(upd_tresql)
        .map { ut =>
          if (upsert) {
            val stripped_ut = stripTrailingAlias(ut, alias)
            val ins_tresql =
              stripTrailingAlias(
                table_insert_tresql(tableName, alias, cols_vals, refsAndPk, false, ctx.view.filters),
                alias)
            s"""|_upsert($stripped_ut, $ins_tresql)"""
          } else ut
        }
        .orNull
    }

    import ctx._
    val parent = parents.headOption.map(_.table).orNull
    val tableName = table.name
    def upd: String = save_tresql_internal(ctx, table_save_tresql, save_tresql(_, _, _, update_tresql))
    val delFilter = ctx.view.filters.flatMap(_.delete).map(f => s" & ($f)").getOrElse("")
    def delAllChildren = s"-$tableName[$refToParent = :#$parent$delFilter]"
    def delMissingChildren =
      s"""_delete_children('$name', '$tableName', -${table
        .name}[$refToParent = :#$parent & _not_delete_ids($pk !in :ids)$delFilter])"""

    val insFilter = {
      val insf = ctx.view.filters.flatMap(_.insert)
      (for {
        pkstr <- Option(pk)
        _ <- ctx.view.filters.flatMap(_.update)
      } yield {
        //if update filter is present set additional insert filter which checks if row with current sequence id does not exist
        val pkcheck = s"!exists($tableName[$pkstr = #$tableName]{1})"
        insf.map (f => s"($f) & $pkcheck").getOrElse(pkcheck)
      })
      .getOrElse(insf orNull)
    }
    def ins = save_tresql(name,
      ctx.view.copy(filters = Option(insFilter).map(f => Filters(insert = Some(f)))), parents, insert_tresql)
    def insOrUpd = {
      val strippedIns = stripTrailingAlias(ins, s" '$name'")
      val strippedUpd = stripTrailingAlias(upd, s" '$name'")
      s"""|_insert_or_update('$tableName', $strippedIns, $strippedUpd) '$name'"""
    }

    import view.options._
    if (parent == null) if (pk == null) null else upd
    else if (refToParent == pk) upd else
      if (pk == null) {
        (Option(doDelete).filter(_ == true).map(_ => delAllChildren) ++
        Option(doInsert).filter(_ == true)
          .flatMap(_ => Option(ins))).mkString(", ")
      } else {
        (Option(doDelete).filter(_ == true).map(_ =>
          if(!doUpdate) delAllChildren else delMissingChildren) ++
        ((doInsert, doUpdate) match {
          case (true, true) => Option(insOrUpd)
          case (true, false) => Option(ins)
          case (false, true) => Option(upd)
          case (false, false) => None
        })).mkString(", ")
      }
  }

  private def save_tresql_internal(
    ctx: SaveContext,
    table_save_tresql: (
      String, //table name
      String, //alias
      List[(String, String)], //cols vals
      Set[(String, String)], //refsPk & values
      Boolean //upsert flag
    ) => String,
    children_save_tresql: (
      String, //table property
      View,
      List[ParentRef] //parent chain
    ) => String)
    (implicit resources: Resources) = {

    def tresql_string(
      table: metadata.Table,
      alias: String,
      refsAndPk: Set[(String, String)],
      children: List[String],
      tresqlColAlias: String,
      upsert: Boolean
    ) = {
      def lookup_tresql(
                         view: View,
                         refColName: String,
                         name: String)(implicit resources: Resources) =
        resources.metadata.tableOption(name).filter(_.key.cols.size == 1).map {
          table =>
            val pk = table.key.cols.headOption.filter(pk => view.properties
              .exists { case OrtMetadata.Property(col, _) => pk == col case _ => false }).orNull
            val insert = save_tresql(name, view, Nil, insert_tresql)
            val update = save_tresql(name, view, Nil, update_tresql)
            List(
              s":$refColName = |_lookup_edit('$refColName', ${
                if (pk == null) "null" else s"'$pk'"}, $insert, $update)",
              refColName -> s":$refColName")
        }.orNull

      ctx.view.properties.flatMap {
        case OrtMetadata.Property(col, _) if refsAndPk.exists(_._1 == col) => Nil
        case OrtMetadata.Property(col, TresqlValue(tresql)) =>
          List(table.colOption(col).map(_.name).orNull -> tresql)
        case OrtMetadata.Property(prop, ViewValue(v)) =>
          table.refTable.get(List(prop)).map(lookupTable => // FIXME perhaps take lookup table from metadata since 'prop' may not match fk name
            lookup_tresql(v.copy(saveTo = List(SaveTo(lookupTable, Set(), Nil))), prop, lookupTable)).getOrElse {
            List(children_save_tresql(prop, v, ParentRef(table.name, ctx.refToParent) :: ctx.parents) -> null)
          }
      }.groupBy { case _: String => "l" case _ => "b" } match {
        case m: Map[String @unchecked, List[_] @unchecked] =>
          val tableName = table.name
          //lookup edit tresql
          val lookupTresql = m.get("l").map(_.asInstanceOf[List[String]].map(_ + ", ").mkString).orNull
          //base table tresql
          val tresql =
            m.getOrElse("b", Nil).asInstanceOf[List[(String, String)]]
              .filter(_._1 != null /*check if prop->col mapping found*/) ++
              children.map(_ -> null) /*add same level one to one children*/
            match {
              case x if x.isEmpty && refsAndPk.isEmpty => null //no columns & refs found
              case cols_vals => table_save_tresql(tableName, alias, cols_vals, refsAndPk, upsert)
            }

          (for {
            base <- Option(tresql)
            tresql <- Option(lookupTresql).map(lookup =>
              s"([$lookup$base])") //put lookup in braces and array,
              //so potentially not to conflict with insert expr with multiple values arrays
              .orElse(Some(base))
          } yield tresql + tresqlColAlias).orNull
      }
    }

    val md = resources.metadata
    val headTable = ctx.view.saveTo.head
    val parent = ctx.parents.headOption.map(_.table).orNull
    def idRefId(idRef: String, id: String) = s"_id_ref_id($idRef, $id)"
    def refsAndPk(tbl: metadata.Table, refs: Set[String]): Set[(String, String)] =
      //ref table (set fk and pk)
      (if (tbl.name == ctx.table.name && ctx.refToParent != null) if (ctx.refToParent == ctx.pk)
        Set(ctx.pk -> idRefId(parent, tbl.name)) else Set(ctx.refToParent -> s":#$parent") ++
          (if (ctx.pk == null || refs.contains(ctx.pk)) Set() else Set(ctx.pk -> s"#${tbl.name}"))
      //not ref table (set pk)
      else Option(tbl.key.cols)
        .filter(k=> k.size == 1 && !refs.contains(k.head)).map(_.head -> s"#${tbl.name}").toSet) ++
      //set refs
      (if(tbl.name == headTable.table) Set() else refs
          //filter out pk of the linked table in case it matches refToParent
          .filterNot(tbl.name == ctx.table.name && _ == ctx.refToParent)
          .map(_ -> idRefId(headTable.table, tbl.name)))

    md.tableOption(headTable.table).map { tableDef =>
      val linkedTresqls =
        for {
          linkedTable <- ctx.view.saveTo.tail
          tableDef <- md.tableOption(linkedTable.table)
        } yield
          tresql_string(tableDef, ctx.view.alias,
            refsAndPk(tableDef, linkedTable.refs), Nil, "", true) //no children, set upsert flag for linked table

      tresql_string(tableDef, ctx.view.alias, refsAndPk(tableDef, Set()),
        linkedTresqls.filter(_ != null), Option(parent)
          .map(_ => s" '${ctx.name}'").getOrElse(""), false)
    }.orNull
  }
}

object OrtMetadata {
  sealed trait OrtValue

  /** Column value
   * @param tresql    tresql statement
   * */
  case class TresqlValue(tresql: String) extends OrtValue

  /** Column value
   * @param view      child view
   * */
  case class ViewValue(view: View) extends OrtValue

  /** Saveable column.
   * @param col       Goes to dml column clause or child tresql alias
   * @param value     Goes to dml values clause or column clause as child tresql
   * */
  case class Property(col: String, value: OrtValue)

  /** Saveable view.
   * @param saveTo        destination tables. If first table has {{{refs}}} parameter
   *                      it indicates reference field to parent.
   * @param options       saveable options [+-=]. Relevant for child views
   * @param filters       horizontal authentication filters
   * @param alias         table alias in DML statement
   * @param properties    saveable fields
   *
   * */
  case class View(saveTo: List[SaveTo],
                  options: SaveOptions,
                  filters: Option[Filters],
                  alias: String,
                  properties: List[Property])

  /** Table to save data to.
   * @param table         table name
   * @param refs          imported keys of table from parent or linked tables to be set
   * @param key           column names uniquely identifying table record
   * */
  case class SaveTo(table: String, refs: Set[String], key: Seq[String])

  /** Save options
   *  @param doInsert     insert new children data
   *  @param doUpdate     update existing children data
   *  @param doDelete     delete absent children data
   * */
  case class SaveOptions(doInsert: Boolean, doUpdate: Boolean, doDelete: Boolean)

  /** Horizontal authentication filters
   *  @param insert   insert statement where clause
   *  @param delete   delete statement where clause
   *  @param update   update statement where clause
   * */
  case class Filters(insert: Option[String] = None, delete: Option[String] = None, update: Option[String] = None)
}

object ORT extends ORT
