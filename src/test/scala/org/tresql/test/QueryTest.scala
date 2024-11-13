package org.tresql.test

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import java.sql.{Connection, DriverManager}

import org.tresql._
import org.tresql.metadata.JDBCMetadata
import sys._

/** To run from console {{{org.scalatest.run(new test.QueryTest)}}} */
class QueryTest extends AnyFunSuite with BeforeAndAfterAll {

  val compilerMacroDependantTests = new CompilerMacroDependantTests()

  val hsqlDialect: CoreTypes.Dialect = dialects.HSQLDialect orElse dialects.VariableNameDialect

  var tresqlResources: Resources = null

  override def beforeAll() = {
    //initialize environment
    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    val connection = DriverManager.getConnection("jdbc:hsqldb:mem:db")
    val md = new JDBCMetadata {
      override def conn: Connection = connection
      override def to_sql_type(vendor: String, typeName: String): String = {
        if (vendor == "postgresql") {
          Map(
            "string"    -> "text",
            "short"     -> "smallint",
            "int"       -> "integer",
            "long"      -> "bigint",
            "integer"   -> "numeric",
            "float"     -> "float",
            "double"    -> "double precision",
            "decimal"   -> "numeric",
            "boolean"   -> "bool",
            "date"      -> "date",
            "time"      -> "time",
            "dateTime"  -> "timestamp",
            "bytes"     -> "bytea",
          ).getOrElse(typeName, typeName)
        } else super.to_sql_type(vendor, typeName)
      }
    }
    val res = Resources()
      .withMetadata(md)
      .withConn(connection)
      .withDialect(hsqlDialect)
      .withIdExpr(_ => "nextval('seq')")
      .withMacros(org.tresql.test.Macros)
      .withCache(new SimpleCache(-1))
      .withLogger((msg, _, topic) => if (topic != LogTopic.sql_with_params) println (msg))

    val conn1 = DriverManager.getConnection("jdbc:hsqldb:mem:db1")
    val md1 = new JDBCMetadata {
      override def conn = conn1
      override def macroSignaturesResource: String = "/tresql-macros-db1.txt"
    }
    val macro1 = new MacroResourcesImpl(Macros1, md1) {
      // TODO currently not supported in runtime since query parser uses only one macro resources obj not dependant on db
      override def macroResource: String = "/tresql-macros-db1.txt"
    }
    val res1 = Resources()
      .withMetadata(md1)
      .withConn(conn1)
      .withDialect(hsqlDialect orElse {
        case f: QueryBuilder#FunExpr if f.name == "current_time" && f.params.isEmpty => "current_time"
      })
      .withIdExpr(_ => "nextval('seq1')")
      .withMacros(macro1)
      .withLogger((msg, _, topic) => if (topic != LogTopic.sql_with_params) println (msg))

    tresqlResources = res.withExtraResources(Map("emp_db" -> res, "contact_db" -> res1, "" -> res))

    List(("/db.sql", connection), ("/db1.sql", conn1)) foreach { case (db, c) =>
      //create test db script
      tresqlResources.log(s"Creating database from file ($db)")
      new scala.io.BufferedSource(getClass.getResourceAsStream(db)).mkString.split("//").foreach {
        sql => val st = c.createStatement; tresqlResources.log(sql); st.execute(sql); st.close
      }
    }
    //set resources for console
    ConsoleResources.resources = tresqlResources
  }

  test("tresql statements") {
    def parsePars(pars: String, conn: Connection, sep:String = ";"): Map[String, Any] = {
      val DF = new java.text.SimpleDateFormat("yyyy-MM-dd")
      val TF = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      val D = """(\d{4}-\d{1,2}-\d{1,2})""".r
      val T = """(\d{4}-\d{1,2}-\d{1,2} \d{1,2}:\d{1,2}:\d{1,2})""".r
      val N = """(-?\d+(\.\d*)?|\d*\.\d+)""".r
      val A = """\[(.*)\](::([^;]+))?""".r
      val VAR = """(\w+)\s*=\s*(.+)""".r
      var map = false
      def par(p: String): Any = p.trim match {
        case VAR(v, x) => {map = true; (v, par(x))}
        case s if s.startsWith("'") => s.substring(1, s.length)
        case "null" => null
        case "false" => false
        case "true" => true
        case D(d) => DF.parse(d)
        case T(t) => TF.parse(t)
        case N(n,_) => BigDecimal(n)
        case A(ac, _, typ) =>
          if (typ == null) {
            if (ac.isEmpty) List()
            else ac.split(",").map(par).toList
          } else {
            val values = if (ac.isEmpty) new Array(0) else ac.split(",").map(par)
            val arr = java.lang.reflect.Array.newInstance(Class.forName(typ), values.length)
            values.zipWithIndex.foreach { case (v, i) => java.lang.reflect.Array.set(arr, i, v) }
            arr
          }
        case x => error(s"unparseable parameter: '$x'")
      }
      val pl = pars.split(sep).map(par).toList
      if (map) {
        var i = 0
        pl map {
          case (k, v) => (k toString, v)
          case x => i += 1; (i toString, x)
        } toMap
      } else pl.zipWithIndex.map(t => (t._2 + 1).toString -> t._1).toMap
    }
    println("\n---------------- Test TreSQL statements ----------------------\n")
    implicit val testResources = tresqlResources
    import io.bullet.borer.Json
    import io.bullet.borer.Dom._
    import TresqlResultBorerElementTranscoder._
    testTresqls("/test.txt", (st, params, patternRes, nr) => {
      println(s"Executing test #$nr:")
      val pattern = jsonDomToAny(Json.decode(patternRes.getBytes("UTF8")).to[Element].value)
      assertResult(pattern, st) {
        jsonDomToAny(
          resultToJsonDom(
            if (params == null) Query(st) else Query(st, parsePars(params, implicitly[Resources].conn))
          )
        )
      }
    })
  }

  test("API") {
    compilerMacroDependantTests.api(tresqlResources)
  }

  test("ORT") {
    compilerMacroDependantTests.ort(tresqlResources)
  }

  test("tresql methods") {
    implicit val testRes = tresqlResources
    println("\n---- TEST tresql methods of QueryParser.Exp ------\n")
    val parser = new QueryParser(testRes, testRes.cache)
    testTresqls("/test.txt", (tresql, _, _, nr) => {
      println(s"$nr. Testing tresql method of:\n$tresql")
      parser.parseExp(tresql) match {
        case e: ast.Exp @unchecked => assert(e === parser.parseExp(e.tresql))
      }
    })
  }

  test("compiler") {
    val testRes = tresqlResources.withMetadata(
      new metadata.JDBCMetadata {
        override def conn: Connection = tresqlResources.conn
        override def macrosClass: Class[_] = classOf[org.tresql.test.Macros]
      }
    )
    val child_metadata = new JDBCMetadata {
      override def conn: Connection = testRes.extraResources("contact_db").conn
      override def macrosClass: Class[_] = classOf[org.tresql.test.Macros1]
      override def functionSignaturesResource: String = "/tresql-function-signatures-db1.txt"
    }
    println("\n-------------- TEST compiler ----------------\n")
    val compiler = new QueryCompiler(testRes.metadata,
      Map("contact_db" -> child_metadata, "emp_db" -> testRes.metadata, "" -> testRes.metadata), testRes)
    //set console compiler so it can be used from scala console
    ConsoleResources.compiler = compiler
    import ast.CompilerException
    testTresqls("/test.txt", (tresql, _, _, nr) => {
      println(s"$nr. Compiling tresql:\n$tresql")
      try compile(tresql)
      catch {
        case e: Exception => throw new RuntimeException(s"Error compiling statement #$nr:\n$tresql", e)
      }
    })

    def compile(exp: String) =
      try compiler.compile(exp) catch {
        case e: Exception => throw new RuntimeException(s"Error compiling statement $exp", e)
      }

    //with recursive compile with braces select def
    compile("""t(*) {(emp[ename ~~ 'kin%']{empno}) +
      (t[t.empno = e.mgr]emp e{e.empno})} t#(1)""")

    //with recursive in column clause compile
    compile("""dummy { (kd(nr, name) {
      emp[ename ~~ 'kin%']{empno, ename} + emp[mgr = nr]kd{empno, ename}
    } kd {name}) val}""")

    //with expression with asterisk resolving
    compile("i(# *){emp e[empno = '']{*}} i{*}")
    compile("i(# *){emp e[empno = '']{e.*}} i{*}")
    compile("i(*){emp e[empno = '']{e.*} + i[false]emp e{e.*}} i{*}")
    compile("e(# *){emp{empno, ename}}, t(# *){i(*){e[empno = '']{e.*} + i[false]e{e.*}}i{*}}t{*}")
    compile("dept{(i(){emp e{ename} + i[false]emp e{i.ename}} i{ename}) x}")
    compile("dept{(i(*){emp e{ename} + i[false]emp e{i.ename}} i{ename}) x}")

    //with expression with dml statement
    compile("d(# dname) {dept{dname}} +dept{deptno, dname} d{#dept, dname || '[reorganized]'}")
    compile("d(# dname) {dept{dname}} =dept[dept.dname = 'x']{deptno, dname} d{#dept, dname || '[reorganized]'}")
    compile("d(# dname) {dept{dname}} =dept[dept.dname = d.dname]d[d.dname = 'x'] {deptno = #dept, dept.dname = d.dname}")
    compile("d(# dname) {dept[deptno = 1]{dname}} dept - [deptno in d{deptno}]")

    //values from select compilation
    compile("=dept_addr da [da.addr_nr = a.nr] address a {da.addr = a.addr}")
    compile("=dept_addr da [da.addr_nr = a.nr] address a {addr = a.addr}")
    compile("=dept_addr da [da.addr_nr = a.nr] address a {addr = da.addr}")
    compile("=dept_addr da [da.addr_nr = address.nr] address {da.addr = address.addr}")
    compile("=dept_addr da [da.addr_nr = a.nr] (address a {a.nr, a.addr}) a {da.addr = a.addr}")
    compile("=dept_addr da [da.addr_nr = nr] (address a {a.nr, a.addr}) a {da.addr = a.addr}")
    compile("=dept_addr da [da.addr_nr = nr] (address a {a.nr, a.addr}) a {da.addr = 'ADDR'}")
    compile("=dept_addr da / address a / dept_addr da1 {da.addr = a.nr}")

    //postgresql style cast compilation
    compile("dummy[dummy::int = 1 & dummy::'double precision' & (dummy + dummy)::long] {dummy::int}")

    //returing compilation
    compile("i(#) { +dummy{dummy} [:v] {*} }, u(#) {=dummy [dummy = :v] {dummy = :v + 1} {*}}, d (#) {dummy - [dummy = :v] {*}} i ++ u ++ d")
    compile("d1(# *) { dummy a[]dummy b { b.dummy col }}, d2(# *) { d1[]dummy? {dummy.dummy d, d1.col c} }, i(# *) { +dummy {dummy} d1[col = 1]{col} {dummy} }, u(# *) { =dummy[dummy = d2.c]d2 {dummy = d2.c} {d2.c u } } u")
    compile("+contact_db:contact{name}[:n] {name}")

    //values from select compilation errors
    intercept[CompilerException](compiler.compile("=dept_addr da [da.addr_nr = a.nr] (address a {a.addr}) a {da.addr = a.addr}"))
    intercept[CompilerException](compiler.compile("=dept_addr da [da.addr_nr = nrz] (address a {a.nr, a.addr}) a {da.addr = a.addr}"))
    intercept[CompilerException](compiler.compile("=dept_addr da [da.addr_nr = a.nr] address a {addr = addr}"))
    intercept[CompilerException](compiler.compile("=dept_addr da [da.addr_nr = a.nr] address a {1}"))

    intercept[CompilerException](compiler.compile("work/dept{*}"))
    intercept[CompilerException](compiler.compile("works"))
    intercept[CompilerException](compiler.compile("emp{aa}"))
    intercept[CompilerException](compiler.compile("(dummy() ++ dummy()){dummy}"))
    intercept[CompilerException](compiler.compile("(dummy ++ dummiz())"))
    intercept[CompilerException](compiler.compile("(dummiz() ++ dummy){dummy}"))
    intercept[CompilerException](compiler.compile("dept/emp[l = 'x']{loc l}(l)^(count(loc) > 1)#(l || 'x')"))
    intercept[CompilerException](compiler.compile("dept/emp{loc l}(l)^(count(l) > 1)#(l || 'x')"))
    intercept[CompilerException](compiler.compile("dept d{deptno dn, dname || ' ' || loc}#(~(dept[dname = dn]{deptno}))"))
    intercept[CompilerException](compiler.compile("dept d[d.dname in (d[1]{dname})]"))
    intercept[CompilerException](compiler.compile("(dummy{dummy} + dummy{dummy d}) d{d}"))
    intercept[CompilerException](compiler.compile("dept{group_concat(dname)#(dnamez)}"))
    intercept[CompilerException](compiler.compile("dept{group_concat(dname)#(dname)[dept{deptnox} < 30]}"))
    intercept[CompilerException](compiler.compile("dept{group_concat(dname)#(dname)[deptno{deptno} < 30]}"))
    intercept[CompilerException](compiler.compile("{dept[10]{dnamez}}"))
    intercept[CompilerException](compiler.compile("b(# y) {a{x}}, a(# x) {dummy{dummy}} b{y}"))
    intercept[CompilerException](compiler.compile("i(# ename){emp e[empno = '']{*}} i{*}"))
    intercept[CompilerException](compiler.compile("d(# id) { dummy[dummy =0] }, u(# id) {dummy[dummy =2]}, upd(#) {dummy[dummy in (u.id)]{dummy} = [u.id + 1] }, remove_from(# ) { dummy - [ dummy in (d{id}) ] } remove_from{dummy}"))
    intercept[CompilerException](compiler.compile("d(# id) { dummy[dummy =0] }, u(# id) {dummy[dummy =2]}, upd(#) {dummy[dummy in (u.id)]{dummy} = [u.id + 1] }, remove_from(# ) { dummy - [ dummy in (d{id}) ] } upd{dummy}"))
    intercept[CompilerException](compiler.compile("d1(# *) { dummy a[]dummy b { b.dummy col }}, d2(# *) { d1[]dummy? {dummy.dummy d, d1.col c} }, i(# *) { +dummy {dummy} d1[col = 1]{col} {dummy} }, u(# *) { =dummy[dummy = d2.c]d2 {dummy = d2.c} {d2.c u, d1.col } } u"))
    intercept[CompilerException](compiler.compile("d(# dname) {dept{dname}} =dept[d.dname = 'x']{deptno, dname} d{#dept, dname || '[reorganized]'}"))
    intercept[CompilerException](compiler.compile("[]dummy_table() d(d){d.x}"))
    intercept[CompilerException](compiler.compile("macro_interpolator_test4(dname, dept)"))
    intercept[CompilerException](compiler.compile("macro_interpolator_test4(dept, name)"))
    intercept[CompilerException](compiler.compile("dept [dname = 'RESEARCH'] {dname, if_defined(:x, macro_interpolator_test(:x, 2))}"))
    intercept[CompilerException](compiler.compile("e(*){emp{ename}}, d(*) {dept{dname}}"))

    //parser errors on macro functions with distinct or agreggate capabilities
    intercept[CompilerException](compiler.compile("macro_interpolator_test1(# 1, 2)"))
    intercept[CompilerException](compiler.compile("macro_interpolator_test1(1, 2)#(1)"))
    intercept[CompilerException](compiler.compile("macro_interpolator_test1(1, 2)[true]"))
    intercept[CompilerException](compiler.compile("macro_interpolator_test1(# 1, 2)#(1)[true]"))

    //insert with asterisk column
    intercept[CompilerException](compiler.compile("+dummy{*} dummy{dummy kiza}"))

    //hierarchical recursive query
    intercept[CompilerException](compiler.compile("emp[mgr = null]{empno, ename, |[:1(empno) = kiza]}"))

    //two aliases
    intercept[CompilerException](compiler.compile("dummy {dummy a b}"))

    //child database error
    intercept[CompilerException](compiler.compile("emp[ename = 'BLAKE']{ ename, |contact_db:[ids = emp.empno]contact{email} email}"))
    intercept[CompilerException](compiler.compile("emp[ename = 'BLAKE']{ ename, |contact_db:[id = emp.empno]contacts{email} email}"))
    intercept[CompilerException](compiler.compile("emp[ename = 'BLAKE']{ ename, |contact_db:[id = emp.empno]contact{eml} email}"))
    intercept[CompilerException](compiler.compile("emp[ename = 'BLAKE']{ ename, |contact:[id = emp.empno]contact{eml} email}"))
    intercept[CompilerException](compiler.compile("+contact_db:contact{name}[:n] {namez}"))

    //function only found in child metadata
    intercept[CompilerException](compiler.compile("emp { instr (ename, job) }"))
    intercept[CompilerException](compiler.compile("emp { format_tuple_b (ename, job) }"))
  }

  test("compiler macro") {
    compilerMacroDependantTests.compilerMacro(tresqlResources)
  }

  test("dialects") {
    println("\n------------------ Test dialects -----------------\n")
    implicit val testRes = tresqlResources.withDialect(dialects.PostgresqlDialect)
    assertResult(Query.build("a::'b'").sql)("select * from a::b")
    assertResult(Query.build("a {1::int + 2::'double precision'}").sql)("select 1::integer + 2::double precision from a")
    assertResult(Query.build("=dept_addr da [da.addr_nr = a.nr] (addr a {a.addr}) a {da.addr = a.addr}").sql)(
      "update dept_addr da set da.addr = a.addr from (select a.addr from addr a) a where da.addr_nr = a.nr")
    assertResult(Query.build("=dept_addr da [da.addr_nr = a.nr] addr a {da.addr = a.addr}").sql)(
      "update dept_addr da set da.addr = a.addr from addr a where da.addr_nr = a.nr")
    assertResult(Query.build("=dept_addr da [da.addr_nr = a.nr] (addr a {a.addr}) a [da.addr_nr = 1 & a.addr = 'a'] {da.addr = a.addr}").sql)(
      "update dept_addr da set da.addr = a.addr from (select a.addr from addr a) a where (da.addr_nr = a.nr) and (da.addr_nr = 1 and a.addr = 'a')")
    assertResult(Query.build("=dept_addr da [da.addr_nr = a.nr] addr a [da.addr_nr = 1 & a.addr = 'a'] {da.addr = a.addr}").sql)(
      "update dept_addr da set da.addr = a.addr from addr a where (da.addr_nr = a.nr) and (da.addr_nr = 1 and a.addr = 'a')")
    assertResult(Query.build("=dept_addr da / address a / dept_addr da1 {da.addr = 'ADDR'}").sql)(
      "update dept_addr da set da.addr = 'ADDR' from address a left join dept_addr da1 on a.nr = da1.addr_nr where da.addr_nr = a.nr"
    )
    assertResult(Query.build("d(# dname) {dept{dname}} +dept{deptno, dname} d{#dept, dname || '[reorganized]'}").sql)(
      "with d(dname) as (select dname from dept) insert into dept (deptno, dname) select ?, dname || '[reorganized]' from d")
    assertResult(Query.build("d(# dname) {dept{dname}} =dept[dept.dname = d.dname]d[d.dname = 'x'] {deptno = #dept, dname = d.dname}").sql)(
      "with d(dname) as (select dname from dept) update dept set deptno = ?, dname = d.dname from d where (dept.dname = d.dname) and (d.dname = 'x')"
    )
    assertResult(Query.build("d(# dname) {dept[deptno = 1]{dname}} dept - [deptno in d{deptno}]").sql)(
      "with d(dname) as (select dname from dept where deptno = 1) delete from dept where deptno in (select deptno from d)"
    )
  }

  test("ast serialization") {
    import io.bullet.borer._
    import io.bullet.borer.derivation.MapBasedCodecs._
    import io.bullet.borer.Codec
    import org.tresql.ast._
    import org.tresql.metadata.{Par, Procedure, ReturnType}
    import CompilerAst._
    implicit      val tableColDefCodec:   Codec[TableColDef]    = deriveCodec    [TableColDef]
    implicit lazy val exprTypeCodec:      Codec[ExprType]       = deriveCodec    [ExprType]
    implicit lazy val parCodec:           Codec[Par]            = deriveCodec    [Par]
    implicit lazy val returnTypeCodec:    Codec[ReturnType]     = deriveAllCodecs[ReturnType]
    implicit lazy val procedureCodec:     Codec[Procedure]      = deriveCodec    [Procedure]
    // deriveAllCodecs fixed in borer 1.10.3, cleanup for scala 3 possible:
    implicit lazy val joinCodec:          Codec[Join]           = deriveCodec    [Join]          // TODO
    implicit lazy val objCodec:           Codec[Obj]            = deriveCodec    [Obj]           // TODO
    implicit lazy val tableDefCodec:      Codec[TableDef]       = deriveCodec    [TableDef]      // TODO
    implicit lazy val identCodec:         Codec[Ident]          = deriveCodec    [Ident]         // TODO
    implicit lazy val arrCodec:           Codec[Arr]            = deriveCodec    [Arr]           // TODO
    implicit lazy val colCodec:           Codec[Col]            = deriveCodec    [Col]           // TODO
    implicit lazy val distinctCodec:      Codec[Distinct]       = deriveCodec    [Distinct]      // TODO
    implicit lazy val colsCodec:          Codec[Cols]           = deriveCodec    [Cols]          // TODO
    implicit lazy val deleteCodec:        Codec[Delete]         = deriveCodec    [Delete]        // TODO
    implicit lazy val colDefCodec:        Codec[ColDef]         = deriveCodec    [ColDef]        // TODO
    implicit lazy val insertCodec:        Codec[Insert]         = deriveCodec    [Insert]        // TODO
    implicit lazy val insertCfl:          Codec[InsertConflict] = deriveCodec    [InsertConflict]// TODO
    implicit lazy val insertCflAct:       Codec[InsertConflictAction] = deriveCodec    [InsertConflictAction]// TODO
    implicit lazy val insertCflTrg:       Codec[InsertConflictTarget] = deriveCodec    [InsertConflictTarget]// TODO
    implicit lazy val updateCodec:        Codec[Update]         = deriveCodec    [Update]        // TODO
    implicit lazy val withTableDefCodec:  Codec[WithTableDef]   = deriveCodec    [WithTableDef]  // TODO
    implicit lazy val dmlDefBaseCodec:    Codec[DMLDefBase]     = deriveAllCodecs[DMLDefBase]    // TODO
    implicit lazy val ordColCodec:        Codec[OrdCol]         = deriveCodec    [OrdCol]        // TODO
    implicit lazy val ordCodec:           Codec[Ord]            = deriveCodec    [Ord]           // TODO
    implicit lazy val funCodec:           Codec[Fun]            = deriveCodec    [Fun]           // TODO
    implicit lazy val funDefCodec:        Codec[FunDef]         = deriveCodec    [FunDef]        // TODO
    implicit lazy val filtersCodec:       Codec[Filters]        = deriveCodec    [Filters]       // TODO
    implicit lazy val grpCodec:           Codec[Grp]            = deriveCodec    [Grp]           // TODO
    implicit lazy val queryCodec:         Codec[Query]          = deriveCodec    [Query]         // TODO
    implicit lazy val binOpCodec:         Codec[BinOp]          = deriveCodec    [BinOp]         // TODO
    implicit lazy val selectDefBaseCodec: Codec[SelectDefBase]  = deriveAllCodecs[SelectDefBase] // TODO
    implicit lazy val sqlDefBaseCodec:    Codec[SQLDefBase]     = deriveAllCodecs[SQLDefBase]    // TODO
    // define explicitly empty transformer exp codec since it cannot be derived
    implicit lazy val transformerExpCodec: Codec[TransformerExp] = Codec(
      new Encoder[TransformerExp] {
        override def write(w: Writer, value: TransformerExp): Writer = ???
      },
      new Decoder[TransformerExp] {
        override def read(r: Reader): TransformerExp = ???
      }
    )
    implicit lazy val expCodec:           Codec[Exp]            = deriveAllCodecs[Exp]
    val testRes = tresqlResources.withMetadata(
      new metadata.JDBCMetadata {
        override def conn: Connection = tresqlResources.conn
        override def macrosClass: Class[_] = classOf[org.tresql.test.Macros]
      }
    )
    val child_metadata = new JDBCMetadata {
      override def conn: Connection = testRes.extraResources("contact_db").conn
      override def macrosClass: Class[_] = classOf[org.tresql.test.Macros1]
      override def functionSignaturesResource: String = "/tresql-function-signatures-db1.txt"
    }
    val compiler = new QueryCompiler(testRes.metadata,
      Map("contact_db" -> child_metadata, "emp_db" -> testRes.metadata, "" -> testRes.metadata), testRes)
    testTresqls("/test.txt", (st, _, _, nr) => {
      def check(e: Exp) = {
        val ev = try Cbor.encode(e).toByteArray catch {
          case ex: Exception => throw new RuntimeException(s"Error encoding statement nr. $nr:\n$st\n$e", ex)
        }
        assertResult(e, st) { Cbor.decode(ev).to[Exp].value }
      }
      val pe = compiler.parseExp(st)
      check(pe)
      check(compiler.compile(pe))
    })
  }

  test("cache") {
    Option(tresqlResources.cache) foreach(c => println(s"\nCache size: ${c.size}\n"))
  }

  test("Test Java API") {
    val res = new java_api.ThreadLocalResources {
      override def cache: Cache = tresqlResources.cache
    }
    res.conn = tresqlResources.conn
    res.dialect = tresqlResources.dialect
    res.idExpr = tresqlResources.idExpr
    res.metadata = tresqlResources.metadata
    Class.forName("org.tresql.test.TresqlJavaApiTest").getDeclaredConstructor().newInstance()
      .asInstanceOf[org.tresql.test.TresqlJavaApiTest].run(res)
  }

  def testTresqls(resource: String, testFunction: (String, String, String, Int) => Unit) = {
    var nr = 0
    new scala.io.BufferedSource(getClass.getResourceAsStream(resource))("UTF-8")
      .getLines().foreach {
        case l if l.trim.startsWith("//") =>
        case l if l.trim.nonEmpty =>
          val (st, params, patternRes) = l.split("-->") match {
            case scala.Array(s, r) => (s, null, r)
            case scala.Array(s, p, r) => (s, p, r)
          }
          nr += 1
          testFunction(st, params, patternRes, nr)
        case _ =>
      }
  }
}

object TresqlResultBorerElementTranscoder {
  import io.bullet.borer.Dom._
  def resultToJsonDom(r: Result[_]): Element = {
    def anyToElement(v: Any): Element = v match {
      case i: Iterable[_] => ArrayElem.Unsized((i map anyToElement).toVector)
      case a: Array[_] => ArrayElem.Unsized((a map anyToElement).toVector)
      case b: Boolean => BooleanElem(b)
      case n: Byte => IntElem(n)
      case n: Short => IntElem(n)
      case n: Int => IntElem(n)
      case n: Long => LongElem(n)
      case n: Float => FloatElem(n)
      case n: Double => DoubleElem(n)
      case n: scala.math.BigInt => NumberStringElem(n.toString)
      case n: scala.math.BigDecimal => NumberStringElem(n.toString)
      case n: java.lang.Number => NumberStringElem(n.toString)
      case b: java.lang.Boolean => BooleanElem(b)
      case t: java.sql.Timestamp => StringElem(t.toString.substring(0, 19))
      case d: java.sql.Date => StringElem(d.toString)
      case null => NullElem
      case a: java.sql.Array => anyToElement(a.getArray)
      case x => StringElem(x.toString)
    }
    anyToElement(r.toListOfVectors)
  }
  def jsonDomToAny(e: Element): Any = e match {
    case ArrayElem.Unsized(value) => value map jsonDomToAny
    case BooleanElem(value) => value
    case IntElem(value) => value
    case LongElem(value) => value
    case FloatElem(value) => value
    case DoubleElem(value) => value
    case NumberStringElem(value) => BigDecimal(value)
    case StringElem(value) => value
    case NullElem => null
    case x => x
  }
}

object ConsoleResources {
  //used in console
  implicit var resources: Resources = _
  var compiler: QueryCompiler = _
}
