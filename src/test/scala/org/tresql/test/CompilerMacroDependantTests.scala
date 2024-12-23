package org.tresql.test

import org.scalatest.funsuite.AnyFunSuite
import org.tresql.OrtMetadata.SaveTo
import org.tresql._

import java.sql.{Date, SQLException, Time}
import java.time.LocalTime

class CompilerMacroDependantTests extends AnyFunSuite with CompilerMacroDependantTestsApi  {

  //typed objects tests
  trait Poha
  case class Car(nr: Int, brand: String) extends Poha
  case class Tyre(carNr: Int, brand: String) extends Poha

  override def api(implicit resources: Resources) = {
    println("\n---------------- Test API ----------------------\n")
    assertResult(10)(Query.head[Int]("dept{deptno}#(deptno)"))
    assertResult(10)(Query.unique[Int]("dept[10]{deptno}#(deptno)"))
    assertResult(Some(10))(Query.headOption[Int]("dept{deptno}#(deptno)"))
    intercept[Exception](Query.unique[Int]("dept{deptno}#(deptno)"))
    intercept[Exception](Query.unique[Int]("dept[100]{deptno}#(deptno)"))
    intercept[Exception](Query.head[Int]("dept[100]{deptno}#(deptno)"))
    assertResult(None)(Query.headOption[Int]("dept[100]{deptno}#(deptno)"))
    assertResult("ACCOUNTING")(Query.unique[String]("dept[10]{dname}#(deptno)"))
    assertResult((10, "ACCOUNTING", "NEW YORK"))(
      Query("dept{deptno, dname, loc}#(1)").toList.map(r => (r.l.deptno, r.s.dname, r.s.loc)).head)
    //option binding
    assertResult("ACCOUNTING")(Query.unique[String]("dept[?]{dname}#(deptno)", Some(10)))
    assertResult("1981-11-17")(Query.unique[java.sql.Date]("emp[sal = 5000]{hiredate}").toString)
    assertResult(java.time.LocalDate.of(1981, 11, 17))(Query.unique[java.time.LocalDate]("emp[sal = 5000]{hiredate}"))
    assertResult("KING" -> java.time.LocalDate.of(1981, 11, 17))(Query.unique[String, java.time.LocalDate]("emp[sal = 5000]{ename, hiredate}"))
    assertResult(java.time.LocalDateTime.of(2009, 2, 22, 0, 0, 0))(
      Query("""date_add ( sql("date '2008-11-22'"), sql("interval 3 month"))""").head[java.time.LocalDateTime])
    assertResult("ABC" -> java.time.LocalDateTime.of(2009, 2, 22, 0, 0, 0))(
      Query("""{ 'ABC', date_add ( sql("date '2008-11-22'"), sql("interval 3 month")) }""").head[String, java.time.LocalDateTime])
    assertResult(BigDecimal(10))(Query.unique[BigDecimal]("dept[10]{deptno}#(deptno)"))
    assertResult(5)(Query.unique[Int]("inc_val_5(?)", 0))
    assertResult(20)(Query.unique[Int]("inc_val_5(inc_val_5(?))", 10))
    assertResult(15)(Query.unique[Long]("inc_val_5(inc_val_5(?))", 5))
    intercept[Exception](Query.head[Int]("emp[?]{empno}", 'z'))
    assertResult(Some(Map("dname" -> "SALES", "loc" -> "CHICAGO"))) {
      val r = Query("dept[dname = 'SALES'] {dname, loc}").uniqueOption
      val m = r.map(_.toMap)
      r.foreach(_.close)
      m
    }
    assertResult(Map("dname" -> "SALES", "loc" -> "CHICAGO")) {
      val r = Query("dept[dname = 'SALES'] {dname, loc}").unique
      val m = r.toMap
      // close result set explicitly since unique method does not close it.
      r.close
      m
    }
    assertResult(Some(Map("loc" -> "DALLAS",
      "emps" -> List(
        Map("ename" -> "ADAMS"),
        Map("ename" -> "FORD"),
        Map("ename" -> "JONES"),
        Map("ename" -> "MĀRTIŅŠ ŽVŪKŠĶIS"),
        Map("ename" -> "SCOTT"),
        Map("ename" -> "SMITH"))))
    ) {
      Query("dept[dname = 'RESEARCH'] { loc, |emp{ename}#(1) emps }").uniqueOption.map { r =>
        0 until r.columnCount map { i =>
          val n = r.column(i).name
          n -> (r(n) match {
            case r: Result[_] => r.toListOfMaps
            case x => x
          })
        } toMap
      }
    }
    assertResult(List(
      ("KING", 5000, null, null),
      ("MARTIN", 1250, java.math.BigInteger.valueOf(1400), 1400),
      ("SCOTT", 4000, null, null))
    ) {
      Query("emp[sal > ? | comm > ?]{ename, sal, comm, comm}#(1)", java.math.BigInteger.valueOf(4000), BigInt(1000))
        .list[String, BigInt, java.math.BigInteger, BigInt]
    }

    //result closing
    intercept[SQLException] {
      val res = Query("emp")
      res.toList
      res(0)
    }
    //expression closing
    intercept[TresqlException] {
      val ex = Query.build("emp")
      ex().asInstanceOf[Result[_]].toList
      ex.close
      ex().asInstanceOf[Result[_]].toList
    }
    //bind variables absence error message
    assert(intercept[MissingBindVariableException](Query("emp[?]")).name === "1")
    assert(intercept[MissingBindVariableException](Query("emp[:nr]")).name === "nr")

    {
      val (op, iop) = OutPar() -> InOutPar(5)
      assertResult(List(Vector(10, "x")))(Query("in_out(?, ?, ?)", iop, op, "x")
        .toListOfVectors)
      assertResult("x")(op.value)
      assertResult(())(tresql"in_out($iop, $op, 'x')")
    }
    assertResult(10)(Query.unique[Long]("dept[(deptno = ? | dname ~ ?)]{deptno} @(0 1)", 10, "ACC%"))
    assertResult(10)(Query.unique[Long]("dept[(deptno = ? | dname ~ ?)]{deptno} @(0 1)",
        Map("1" -> 10, "2" -> "ACC%")))
    assertResult(None)(Query.headOption[Int]("dept[?]", -1))
    //dynamic tests
    assertResult(1900)(Query("salgrade[1] {hisal, losal}").foldLeft(0)((x, r) => x +
        r.i.hisal + r.i.losal))
    assertResult(1900)(Query("salgrade[1] {hisal, losal}").foldLeft(0L)((x, r) => x +
        r.l.hisal + r.l.losal))
    assertResult(1900.00)(Query("salgrade[1] {hisal, losal}").foldLeft(0D)((x, r) => x +
        r.dbl.hisal + r.dbl.losal))
    assertResult(1900)(Query("salgrade[1] {hisal, losal}").foldLeft(BigDecimal(0))((x, r) => x +
        r.bd.hisal + r.bd.losal))
    assertResult("KING PRESIDENT")(Query("emp[7839] {ename, job}").foldLeft("")((x, r) =>
        r.s.ename + " " + r.s.job))
    assertResult("1982-12-09")(Query("emp[ename ~~ 'scott'] {hiredate}").foldLeft("")((x, r) =>
        r.d.hiredate.toString))
    assertResult("1982-12-09 00:00:00.0")(Query("emp[ename ~~ 'scott'] {hiredate}").foldLeft("")((x, r) =>
        r.t.hiredate.toString))
    assertResult("KING PRESIDENT")(Query("emp[7839] {ename, job}").foldLeft("")((_, r) =>
        r.s.ename + " " + r.s.job))
    //typed tests
    assertResult(("MILLER", BigDecimal(2300.35)))(Query.head[(String, BigDecimal)]("emp[hiredate = '1982-01-23']{ename, sal}"))
    assertResult(List(("CLARK", "ACCOUNTING", 2450.00), ("KING", "ACCOUNTING", 5000.00),
      ("MILLER", "ACCOUNTING", 2300.35)))(Query.list[(String, String, Double)]("emp/dept[?]{ename, dname, sal}#(1)", 10))
    assertResult(List(("CLARK", "ACCOUNTING", 2450.00, "NEW YORK"), ("KING", "ACCOUNTING", 5000.00, "NEW YORK"),
      ("MILLER", "ACCOUNTING", 2300.35, "NEW YORK"))) {
      Query.list[String, String, Double, String]("emp/dept[?]{ename, dname, sal, loc}#(1)", 10)
    }
    assertResult(List("ACCOUNTING", "OPERATIONS", "RESEARCH", "SALES"))(Query.list[String]("dept{dname}#(1)"))
    assertResult(List((10,"ACCOUNTING",List((7782,"CLARK",List()), (7839,"KING",List((Date.valueOf("2012-06-06"),3),
        (Date.valueOf("2012-06-07"),4))), (7934, "MILLER", List())),List("PORCHE"))))(
            Query.list[Int, String, List[(Int, String, List[(Date, Int)])], List[String]] {
      "dept[10]{deptno, dname, |emp[deptno = :1(deptno)]{empno, ename, |[empno]work{wdate, hours}#(1,2) work}#(1) emps," +
      " |car[deptnr = :1(deptno)]{name}#(1) cars}"})
    assertResult(List((10, "ACCOUNTING"), (20, "RESEARCH")))(
        Query.list[Int, String]("dept[deptno = ? | deptno = ?]#(1)", 10, 20))
    assertResult((10, 10)) {
      val r = tresql"dept[deptno = 10]{deptno}"
      r.hasNext
      r.next()
      val (id1, id2) = (r.typed[Int]("deptno"), r.typed("deptno")(scala.reflect.ManifestFactory.Int))
      r.close
      (id1, id2)
    }
    assertResult((List("10:15:00", "12:00:30"), List("10:15", "12:00:30"))) {
      val contact_id =
        Query("+contact_db:contact {id = #contact, name = 'Alla', sex = 'F', birth_date = '1980-05-31', email = 'alla@zz.lv'}")
          .asInstanceOf[DMLResult].id.get
      def q(tn: String) =
        s"+contact_db:visit {id = #visit, contact_id = :contact, visit_date = '2022-06-01', visit_time = :$tn}"
      Query(
        q("time1") + ", " + q("time2")
      )(implicitly[Resources].withParams(
        Map(
          "contact" -> contact_id,
          "time1" -> Time.valueOf("10:15:00"),
          "time2" -> LocalTime.of(12, 0, 30)
        )
      ))
      val st = "|contact_db:visit[contact_id = (contact[name = 'Alla']{id}) & visit_date = '2022-06-01']{visit_time}#(1)"
      (Query(st).list[Time].map(_.toString), Query(st).list[LocalTime].map(_.toString))
    }

    implicit def convertRowLiketoPoha[T <: Poha](r: RowLike, m: Manifest[T]): T = m.toString match {
      case s if s.contains("Car") => Car(r.i("nr"), r.s("name")).asInstanceOf[T]
      case s if s.contains("Tyre") => Tyre(r.i("nr"), r.s("brand")).asInstanceOf[T]
      case x => sys.error("Unable to convert to object of type: " + x)
    }
    assertResult(List(Car(1111, "PORCHE"), Car(2222, "BMW"), Car(3333, "MERCEDES"),
        Car(4444, "VOLKSWAGEN")))(Query.list[Car]("car {nr, name} #(1)"))
    assertResult(List(Tyre(3333, "MICHELIN"), Tyre(3333, "NOKIAN")))(
        Query.list[Tyre]("tyres {carnr nr, brand} #(1, 2)"))
    //column alias test
    assertResult(List(("ACCOUNTING,CLARK", -2450.00), ("ACCOUNTING,KING", -5000.00), ("ACCOUNTING,MILLER", -2300.35))) {
      Query("emp/dept[10] {dname || ',' || ename name, -sal salary}#(1)") map (r=> (r.name, r.dbl.salary)) toList
    }
    assertResult(List(0.00, 0.00, 0.00)) {
      Query("emp/dept[10] {sal + -sal salary}#(1)") map (_.salary) toList
    }
    assertResult(List(0.00, 0.00, 0.00)) {
      Query("emp/dept[10] {(sal + -sal) salary}#(1)") map (_.salary) toList
    }

    assertResult(List(Map("dname" -> "ACCOUNTING", "emps" -> List(Map("ename" -> "CLARK"),
        Map("ename" -> "KING"), Map("ename" -> "MILLER"))))) {
      Query.toListOfMaps("dept[10]{dname, |emp{ename}#(1) emps}")
    }

    //bind variables test
    assertResult(List(10, 20, 30, 40)){
      val ex = Query.build("dept[deptno in ?]{deptno}")
      val res = List(10, 20, 30, 40) flatMap {p=> ex(List(p)).asInstanceOf[DynamicResult].map(_.deptno)}
      ex.close
      res
    }
    assertResult(List(Vector("ACCOUNTING"), Vector("RESEARCH"))) {
      Query("(dept[deptno = :1.deptno]{dname} + dept[deptno = :2.deptno]{dname})#(1)",
        List(Map("deptno" -> 10), Map("deptno" -> 20)): _*).toListOfVectors
    }
    assertResult(List(Vector("Kiki"), Vector("Mimi"))) {
      Query("names(# name) {{:1.name name} + {:2.name}} names{name}#(1)",
        List(Map("name" -> "Mimi"), Map("name" -> "Kiki")): _*).toListOfVectors
    }
    //array, stream, reader, blob, clob test
    assertResult(List(Vector(2)))(Query("car_image{carnr, image} + [?, ?], [?, ?]", 1111,
        new java.io.ByteArrayInputStream(scala.Array[Byte](1, 4, 127, -128, 57)), 2222,
        scala.Array[Byte](0, 32, 100, 99)).toListOfVectors)
    assertResult(List(1, 4, 127, -128, 57))(
        Query("car_image[carnr = ?] {image}", 1111).flatMap(_.b.image).toList)
    assertResult(List[Byte](0, 32, 100, 99)) {
      val res = Query("car_image[carnr = ?] {image}", 2222).map(_.bs(0)).toList(0)
      val bytes = new scala.Array[Byte](4)
      res.read(bytes)
      bytes.toList
    }

    assertResult(List[Byte](0, 32, 100, 99)){
      val b = Query.head[java.sql.Blob]("car_image[carnr = ?] {image}", 2222).getBinaryStream
      Stream.continually(b.read).takeWhile(-1 !=).map(_.toByte).toArray.toList
    }
    assertResult(4){
      Query.head[java.sql.Blob]("car_image[carnr = ?] {image}", 2222).length
    }
    assertResult(List("ACCOUNTING", "OPERATIONS", "RESEARCH", "SALES")) {
      Query.list[java.io.Reader]("dept{dname}#(1)").map(r =>
        new String(Stream.continually(r.read).takeWhile(-1 !=).map(_.toChar).toArray)).toList
    }
    assertResult(List("ACCOUNTING", "OPERATIONS", "RESEARCH", "SALES")) {
      Query.list[java.sql.Clob]("dept{dname}#(1)").map(c => {
        val r = c.getCharacterStream
        new String(Stream.continually(r.read).takeWhile(-1 !=).map(_.toChar).toArray)
      }).toList
    }
    assertResult(List(Vector(1))) {
      Query("+dept_addr", 10, new java.io.StringReader("Strelnieku str."),
          new java.io.StringReader("LV-1010"), null).toListOfVectors
    }
    assertResult(List(Vector(1)))(Query("car_image[carnr = ?]{image} = [?]", 2222,
        new java.io.ByteArrayInputStream(scala.Array[Byte](1, 2, 3, 4, 5, 6, 7))).toListOfVectors)
    assertResult(List(1, 2, 3, 4, 5, 6, 7))(
        Query("car_image[carnr = ?] {image}", 2222).flatMap(_.b("image")).toList)
    //array binding
    assertResult(List(10, 20, 30))(Query.list[Int]("dept[deptno in ?]{deptno}#(1)", List(30, 20, 10)))
    assertResult(List(10, 20, 30))(Query.list[Int]("dept[deptno in ?]{deptno}#(1)", scala.Array(30, 20, 10)))
    //hierarchical inserts, updates test
    assertResult(new InsertResult(Some(1), children = List(("emps", List(new InsertResult(Some(1)), new InsertResult(Some(1))))), id = Some(50))) {
      Query("""dept{deptno, dname, loc, +emp {empno, ename, deptno}[:empno, :ename, :#dept] emps} +
        [#dept:dept_id, :dname, :loc]""",
      Map("dept_id" -> 50, "dname" -> "LAW", "loc" -> "DALLAS",
        "emps" -> List(Map("empno" -> 1111, "ename" -> "SMITH"),
          Map("empno" -> 2222, "ename" -> "LEWIS"))))
    }
    assertResult(new UpdateResult(Some(1), children = List((null, new DeleteResult(Some(2))),
      ("emps", List(new InsertResult(Some(1)), new InsertResult(Some(1)))))))(Query(
      """dept[deptno = #dept:dept_id]{ dname, loc,
               -emp[deptno = :#dept],
               +emp {empno, ename, deptno} [:empno, :ename, :#dept] emps} =
        [:dname, :loc]""",
      Map("dept_id" -> 50, "dname" -> "LAW", "loc" -> "FLORIDA",
        "emps" -> List(Map("empno" -> 1111, "ename" -> "BROWN"),
          Map("empno" -> 2222, "ename" -> "CHRIS")))))
    assertResult(List(new DeleteResult(count = Some(2)), new DeleteResult(count = Some(1))))(
      Query("emp - [deptno = 50], dept - [50]").head.values)
    assertResult(new InsertResult(Some(1), children =
      List(("emps", List(new InsertResult(Some(1), id = Some(10002)),
        new InsertResult(Some(1), id = Some(10003))))), id = Some(10001)))(Query(
      """dept{deptno, dname, loc, +emp {empno, ename, deptno} [#emp, :ename, :#dept] emps} +
        [#dept, :dname, :loc]""",
      Map("dname" -> "LAW", "loc" -> "DALLAS", "emps" -> scala.Array(
        Map("ename" -> "SMITH"), Map("ename" -> "LEWIS")))))

    //tresql string interpolation tests
    assertResult("CLARK, KING, MILLER")(tresql"dept[10] {dname, |emp {ename}#(1) emps}"
        .head.emps.map(_.ename).mkString(", "))
    assertResult((List(Vector(0), Vector(10)),List(Vector(0)))){
      val (a, b) = ("acc%", -1)
      val r = tresql"/(dept[dname ~~ $a]{deptno} + dummy) a#(1), salgrade[$b] {grade} + dummy"
      (r._1.toListOfVectors, r._2.toListOfVectors)
    }

    var name = "S%"
    assertResult(List(Vector("SCOTT"), Vector("SMITH"), Vector("SMITH"))) {
      tresql"emp [ename ~ $name] {ename}#(1)".toListOfVectors
    }
    val salary = 1000
    assertResult(List(Vector("SCOTT")))(tresql"emp [ename ~ $name & sal > $salary] {ename}#(1)".toListOfVectors)
    name = null
    assertResult(List(Vector("JAMES"), Vector("SMITH"))) {
      tresql"emp [ename ~ $name? & sal < $salary?] {ename}#(1)".toListOfVectors
    }
    assertResult(List(Vector(0)))(tresql"dummy" toListOfVectors)

    assertResult((10, "ACCOUNTING",(7782, "CLARK", "1981-06-09")))(
      tresql"dept{deptno, dname, |emp{empno, ename, hiredate}#(1) emps}#(1)"
        .head match {case d => (d.deptno, d.dname, d.emps
          .head match {case e => (e.empno, e.ename, String.valueOf(e.hiredate))})})
    assertResult(Map("deptno" -> 10, "dname" -> "ACCOUNTING", "loc" -> "NEW YORK"))(
      tresql"dept".head.toMap)
    assertResult((10, "ACCOUNTING"))(tresql"dept{deptno, dname}#(1)".head[Int, String])
    assertResult(List((10, "ACCOUNTING"), (20, "RESEARCH")))(
      tresql"dept{deptno, dname}#(1)@(2)".list[Int, String])

    //Resources methods test
    val qt = resources.queryTimeout
    val e1 = resources.withMaxResultSize(555)
    val e2 = resources.withQueryTimeout(333)
    assertResult((555, qt)) {(e1.maxResultSize, e1.queryTimeout)}
    assertResult((0, 333)) {(e2.maxResultSize, e2.queryTimeout)}
    val e3 = e2.withQueryTimeout(222)
    assertResult((555, 222)) {(e1.maxResultSize, e3.queryTimeout)}
    assertResult((0, 333)) {(e2.maxResultSize, e2.queryTimeout)}
    assertResult((110, 111)) {
      //thread local resources test
      val res = new ThreadLocalResources {
        override def initResourcesTemplate = super.initResourcesTemplate.copy(queryTimeout = 110)
      }.withFetchSize(111)
      res.queryTimeout -> res.fetchSize
    }

    //exists method
    assertResult(true)(tresql"exists(emp {*})")
    assertResult(false)(tresql"exists(emp[ename = null])")

    //from clause consist of functions - only tresql macro cannot be used.
    assertResult(
      List(
        Vector("ALLEN", "SALES"),
        Vector("BLAKE", "SALES"),
        Vector("JAMES", "SALES"),
        Vector("MARTIN", "SALES"),
        Vector("TURNER", "SALES"),
        Vector("WARD", "SALES"))) {
      Query("[]sql('emp')[emp.deptno = dept.deptno]sql('dept')[dname = 'SALES']{ename, dname}#(1,2)").toListOfVectors
    }
    assertResult(List(Vector("ACCOUNTING"))) {
      Query("[]dynamic_table(:table)[deptno = 10]{dname}", Map("table" -> "dept")).toListOfVectors
    }
    assertResult(List(Vector("ACCOUNTING"))) {
      Query("c(#) { []dynamic_table(:table)[deptno = 10]{dname} } c", Map("table" -> "dept")).toListOfVectors
    }

    //path bind variable access (dotted syntax)
    assertResult(List(Vector("SALES")))(
      Query("dept[dname = :dept.name] {dname}", Map("dept" -> Map("name" -> "SALES"))).toListOfVectors)
    assertResult(Nil)(
      Query("dept[dname = :dept.name] {dname}", Map("dept" -> Map("name" -> null))).toListOfVectors)
    assertResult(Nil)(
      Query("dept[dname = :dept.name] {dname}", Map("dept" -> null)).toListOfVectors)
    intercept[MissingBindVariableException](
      Query("dept[dname = :dept.name] {dname}", Map("dept" -> "x")).toListOfVectors)
    assertResult(List(Vector("SALES")))(
      Query("dept[dname = :dept.2.name] {dname}", Map("dept" -> (1 -> Map("name" -> "SALES")))).toListOfVectors)
    assertResult(Nil)(
      Query("dept[dname = :dept.2.name] {dname}", Map("dept" -> (1 -> null))).toListOfVectors)
    intercept[MissingBindVariableException](
      Query("dept[dname = :dept.2.name] {dname}", Map("dept" -> (1 -> "a"))).toListOfVectors)
    //path bind variables optional binding
    assertResult(List(Vector("RESEARCH"), Vector("SALES")))(
      Query("dept[dname = 'SALES' | dname = :dept.name?] {dname}#(1)", Map("dept" -> Map("name" -> "RESEARCH"))).toListOfVectors)
    assertResult( List(Vector(null)))(
      Query("(dept[dname = 'SALES']{dname} + {null})[case(:dept.name? = null, dname = null, dname = :dept.name?)]{dname}#(null 1)", Map("dept" -> Map("name" -> null))).toListOfVectors)
    assertResult(List(Vector(null), Vector("SALES")) )(
      Query("(dept[dname = 'SALES']{dname} + {null})[case(:dept.name? = null, dname = null, dname = :dept.name?)]{dname}#(null 1)", Map("dept" -> null)).toListOfVectors)
    assertResult(List(Vector(null), Vector("SALES")))(
      Query("(dept[dname = 'SALES']{dname} + {null})[case(:dept.name? = null, dname = null, dname = :dept.name?)]{dname}#(null 1)", Map("dept" -> "x")).toListOfVectors)
    assertResult(List(Vector("RESEARCH")))(
      Query("(dept{dname} + {null})[case(:dept.2.name? = null, dname = null, dname = :dept.2.name?)]{dname}#(null 1)", Map("dept" -> (1 -> Map("name" -> "RESEARCH")))).toListOfVectors)
    assertResult(List(Vector(null), Vector("ACCOUNTING"), Vector("LAW"), Vector("OPERATIONS"), Vector("RESEARCH"), Vector("SALES")))(
      Query("(dept{dname} + {null})[case(:dept.1.name? = null, dname = null, dname = :dept.1.name?)]{dname}#(null 1)", Map("dept" -> (1 -> Map("name" -> "RESEARCH")))).toListOfVectors)
    assertResult(List(Vector(null), Vector("ACCOUNTING"), Vector("LAW"), Vector("OPERATIONS"), Vector("RESEARCH"), Vector("SALES")))(
      Query("(dept{dname} + {null})[case(:dept.2.name? = null, dname = null, dname = :dept.2.name?)]{dname}#(null 1)", Map("dept" -> (1 -> null))).toListOfVectors)
    assertResult(List(Vector(null), Vector("ACCOUNTING"), Vector("LAW"), Vector("OPERATIONS"), Vector("RESEARCH"), Vector("SALES")))(
      Query("(dept{dname} + {null})[case(:dept.2.name? = null, dname = null, dname = :dept.2.name?)]{dname}#(null 1)", Map("dept" -> (1 -> "a"))).toListOfVectors)
    assertResult(List(Map("name" -> "Sales Riga")))(
      Query("{:dept.0.name || ' ' || :dept.0.loc name}",
        Map("dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))).toListOfMaps)
    assertResult(List(Map("name" -> "Sales, Riga")))(
      Query("dept(# name, loc) { {:dept.0.name, :dept.0.loc} } dept { name || ', ' || loc name}#(1)",
        Map("dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))).toListOfMaps)

    //test string escape syntax
    assertResult("x\u202Fy")(Query("{'x' || '\u202F' || 'y'}").head[String])
    assertResult("x\u202Fy")(tresql"{'x' || '\u202F' || 'y'}".head[String])

    //macro API test
    assertResult(List("ACCOUNTING", "LAW", "OPERATIONS", "RESEARCH", "SALES"))(
      tresql"macro_interpolator_test4(dept, dname)"(using resources
        .withMacros(Some(org.tresql.test.Macros))).map(_.dname).toList)
    assertResult(List(0))(tresql"dummy{dummy}@(1)"(using resources
        .withMacros(None)).map(_.dummy).toList)
    assertResult(List(0))(tresql"dummy{dummy}@(1)"(using resources
      .withMacros(null)).map(_.dummy).toList)
    intercept[Exception](tresql"dummy{dummy}@(1)"(using resources
      .withMacros(List(1))).map(_.dummy).toList)
    intercept[Exception](tresql"dummy{dummy}@(1)"(using resources
      .withMacros(1)).map(_.dummy).toList)
    assertResult(true)(tresql"macro_interpolator_test4(dept, dname)"(using resources
      .withMacros(null).withCache(null)).map(_.dname).toList.nonEmpty)
    //macro string escape syntax
    assertResult("\u202F\u202F")(tresql"macro_interpolator_str_test('\u202F', '\u202F')")
    assertResult("no_args")(tresql"macro_interpolator_noargs_test()")
    assertResult(("SALES", "CHICAGO")) {
      tresql"""concat_exps('dept[dname = "SALES"]{', ',', '}', dname, loc)""".unique[String, String]
    }
    assertResult(("x a y", "x b y")) { tresql"map_exps('x ' || ['a', 'b'] || ' y')" }
    assertResult(("x a y", "x b y")) {
      tresql"concat_exps('{', ',', '}', map_exps('x ' || ['a', 'b'] || ' y'))".unique[String, String]
    }
    assertResult( ("x ADAMS y", "x ACCOUNTING y")) {
      tresql"concat_exps('{', ',', '}', map_exps('x ' || [(emp[ename != null]{ename}#(1)@(1)), (dept{dname}#(1)@(1))] || ' y'))"
        .unique[String, String]
    }
    assertResult( ("x ADAMS y", "x ACCOUNTING y")) {
      tresql"concat_exps('{', ',', '}', map_exps('x ' || [(emp[ename != null][ename != null]{ename}#(1)@(1)), (dept{dname}#(1)@(1))] || ' y'))"
        .unique[String, String]
    }
    assertResult("x a y") {
      Query("concat_exps_test('a')").unique[String] // scala compiler started to crash if treql interpolator is used
    }
    assertResult(("x a y", "x b y")) {
      Query("concat_exps_test('a', 'b')").unique[String, String]
    }
    assertResult("x a y") {
      Query("concat_exps_test1('{', ',', '}', 'a')").unique[String]
    }
    assertResult(("x a y", "x b y")) {
      Query("concat_exps_test1('{', ',', '}', 'a', 'b')").unique[String, String]
    }

    //alias test
    assertResult(List(1))(tresql"{(pi()) / pi() pi}".map(_.pi).toList)

    //tresql exception test
    try {
      val n = "SCOTT"
      Query("emp [name = ?]", n)
    } catch {
      case ex: TresqlException =>
        assertResult(("select * from emp where name = ?/*1*/", List("1" -> "SCOTT"))) {
          ex.sql -> ex.bindVars
        }
      case x: Throwable => throw x
    }

    //if_defined macro test for nested bind vars structure
    assertResult("n")(Query("if_defined_or_else(:a.x, 'y', 'n')", Map("a" -> null)).head(0))
    assertResult("n")(Query("if_defined_or_else(:a.x, 'y', 'n')", Map("a" -> 1)).head(0))
    assertResult("y")(Query("if_defined_or_else(:a.x, 'y', 'n')", Map("a" -> Map("x" -> 2))).head(0))

    //test column sequence
    val rowColOrderTest = (0 to 10 map (_ + 'a'.toInt) map (_.toChar.toString)).reverse.zipWithIndex
    assertResult(List(rowColOrderTest)) {
      val q = rowColOrderTest.map { case (c, v) => s"$v $c" }.mkString("{ ", ", ", " }")
      Query(q).toListOfMaps.map(_.toSeq)
    }

    //test null column names
    assertResult(List(Map("a" -> 1, "_1" -> 2, "b" -> 3, "_2" -> 4, "_3" -> List(
      Map("_1" -> 1, "a" -> 2, "_2" -> 3, "b" -> 4))))) {
      Query("{ 1 a, 2, 3 b, 4, |null{ 1, 2 a, 3, 4 b } }").toListOfMaps
    }

    //sql array test
    assertResult((List(List(4)), List("D"))) {
      (tresql"results{scores}".map(_.scores.getArray.asInstanceOf[Array[_]].toList).toList,
        Query("results{names}").head[java.sql.Array].getArray.asInstanceOf[Array[_]].toList)
    }

    //stack overflow test for long bin exps
    assertResult(1) {
      val s = List.fill(1024 * 4)("{1}").mkString("+")
      Query(s)(implicitly[Resources].withLogger(
        (msg, _, _) =>
          println(if (msg.length > 256) msg.take(128) + " ... " + msg.substring(msg.length - 127) else msg)
      )).head[Int]
    }

    //implicit conversion from java.sql.ResultSet to org.tresql.Result
    assertResult(List(Vector("ACCOUNTING"), Vector("LAW"), Vector("OPERATIONS"), Vector("RESEARCH"), Vector("SALES"))) {
      import org.tresql.given
      val s = implicitly[Resources].conn.prepareStatement("select dname from dept order by 1")
      s.execute
      val r: Result[RowLike] = s.getResultSet
      r.toListOfVectors
    }

    //string escape syntax tests
    assertResult("""start'"'end""") {tresql""""start'""'end""""}
    assertResult("""start"'"end""") {tresql"""'start"''"end'"""}
    val p: QueryParser = new QueryParser(null, null)
    assertResult("""start'"'end"""){
      Query(p.parseExp(""""start'""'end"""").tresql).unique[String]
    }
    assertResult("""start"'"end"""){
      Query(p.parseExp("""'start"''"end'""").tresql).unique[String]
    }
  }

  override def ort(implicit resources: Resources) = {
    println("\n----------- ORT tests ------------\n")
    println("\n--- INSERT ---\n")

    var obj: Map[String, Any] = Map("deptno" -> null, "dname" -> "LAW2", "loc" -> "DALLAS",
      "calculated_field"->333, "another_calculated_field"->"A",
      "emp" -> scala.Array(Map("empno" -> null, "ename" -> "SMITH", "deptno" -> null,
          "deptno_name" -> List(Map("name" -> "20, RESEARCH (DALLAS)")),
          "work:empno"->List(Map("wdate"-> java.time.LocalDate.of(2012, 7, 9), "empno"->null, "hours"->8, "empno_mgr"->null),
              Map("wdate"->java.time.LocalDate.of(2012, 7, 10), "empno"->null, "hours"->8, "empno_mgr"->null))),
        Map("empno" -> null, "ename" -> "LEWIS", "deptno" -> null,
            "deptno_name" -> List(Map("name" -> "20, RESEARCH (DALLAS)")),
            "work:empno"->List(Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "empno"->null, "hours"->8, "empno_mgr"->null)))))
    assertResult(new InsertResult(Some(1), children = List(("emp",
      List(new InsertResult(count = Some(1), children = List(("work:empno",
        List(new InsertResult(count = Some(1)), new InsertResult(count = Some(1))))), id = Some(10005)),
        new InsertResult(count = Some(1), children =
          List(("work:empno", List(new InsertResult(count = Some(1))))), id = Some(10006))))), id = Some(10004)))(
        ORT.insert("dept", obj))
    intercept[Exception](ORT.insert("no_table", obj))

    //insert with set parent id and do not insert existing tables with no link to parent
    //(work under dept)
    obj = Map("deptno" -> 50, "dname" -> "LAW3", "loc" -> "FLORIDA",
        "emp" -> List(Map("empno" -> null, "ename" -> "BROWN", "deptno" -> null),
          Map("empno" -> null, "ename" -> "CHRIS", "deptno" -> null)),
        "work"->List(Map("wdate"->"2012-7-9", "empno"->null, "hours"->8, "empno_mgr"->null)))
    assertResult(new InsertResult(count = Some(1), children =
      List(("emp", List(new InsertResult(count = Some(1), id = Some(10007)),
        new InsertResult(count = Some(1), id = Some(10008))))), id = Some(50)))(ORT.insert("dept", obj))

    obj = Map("dname" -> "FOOTBALL", "loc" -> "MIAMI",
        "emp" -> List(Map("ename" -> "BROWN"), Map("ename" -> "CHRIS")))
    assertResult( new InsertResult(count = Some(1), children =
      List(("emp", List(new InsertResult(count = Some(1), id = Some(10010)),
        new InsertResult(count = Some(1), id = Some(10011))))), id = Some(10009)))(ORT.insert("dept", obj))

    obj = Map("ename" -> "KIKI", "deptno" -> 50, "car"-> List(Map("name"-> "GAZ")))
    assertResult(new InsertResult(count = Some(1), id = Some(10012)))(ORT.insert("emp", obj))

    //Ambiguous references to table: emp. Refs: List(Ref(List(empno)), Ref(List(empno_mgr)))
    obj = Map("emp" -> Map("empno" -> null, "ename" -> "BROWN", "deptno" -> null,
            "work"->List(Map("wdate"->"2012-7-9", "empno"->null, "hours"->8, "empno_mgr"->null))))
    intercept[Exception](ORT.insert("emp", obj))

    //child foreign key is also its primary key
    obj = Map("deptno" -> 60, "dname" -> "POLAR", "loc" -> "ALASKA",
              "dept_addr" -> List(Map("addr" -> "Halibut")))
    assertResult(new InsertResult(count = Some(1), children =
      List(("dept_addr", List(new InsertResult(count = Some(1), id = Some(60))))), id = Some(60)))(
      ORT.insert("dept", obj))
    //child foreign key is also its primary key
    obj = Map("dname" -> "BEACH", "loc" -> "HAWAII",
              "dept_addr" -> List(Map("deptnr" -> 1, "addr" -> "Honolulu", "zip_code" -> "1010")))
    assertResult(new InsertResult(count = Some(1), children =
      List(("dept_addr",
        List(new InsertResult(count = Some(1), id = Some(10013))))), id = Some(10013)))(ORT.insert("dept", obj))

    obj = Map("deptno" -> null, "dname" -> "DRUGS",
              "car" -> List(Map("nr" -> "UUU", "name" -> "BEATLE")))
    assertResult(new InsertResult(count = Some(1), children =
      List(("car", List(new InsertResult(count = Some(1), id = Some("UUU"))))),
      id = Some(10014)))(ORT.insert("dept", obj))

    //multiple column primary key
    obj = Map("empno"->7788, "car_nr" -> "1111")
    assertResult(new InsertResult(count = Some(1)))(ORT.insert("car_usage", obj))

    //value clause test
    obj = Map("car_nr" -> 2222, "empno" -> 7788, "date_from" -> java.sql.Date.valueOf("2013-11-06"))
    assertResult(new InsertResult(count = Some(1)))(ORT.insert("car_usage", obj))

    println("\n--- UPDATE ---\n")

    obj = Map("dname"->"DEVELOPMENT", "loc"->"DETROIT", "calculated_field"-> 222,
        "emp"->List(
            Map("empno"->null, "ename"->"ANNA", "mgr"->7788, "mgr_name"->null,
              "work:empno"->List(Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "hours"->8, "empno_mgr"->7839),
                                 Map("wdate"->java.sql.Date.valueOf("2012-7-10"), "hours"->10, "empno_mgr"->7839))),
            Map("empno"->null, "ename"->"MARY", "mgr"->7566, "mgr_name"->null,
              "work:empno" -> List())),
        "calculated_children"->List(Map("x"->5)), "deptno"->40,
        //this will not be inserted since work has no relation to dept
        "work"->List(
            Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "empno"->7788, "hours"->8, "empno_mgr"->7839),
            Map("wdate"->java.sql.Date.valueOf("2012-7-10"), "empno"->7788, "hours"->8, "empno_mgr"->7839)),
        "car" -> List(Map("nr" -> "EEE", "name" -> "BEATLE"), Map("nr" -> "III", "name" -> "FIAT")))
    assertResult(new UpdateResult(count = Some(1),
      children = List((null, new DeleteResult(count = Some(0))),
        ("emp", List(new InsertResult(count = Some(1), children =
          List(("work:empno", List(new InsertResult(count = Some(1)),
            new InsertResult(count = Some(1))))), id = Some(10015)),
          new InsertResult(count = Some(1), children = List(("work:empno",List())), id = Some(10016)))),
        (null, new DeleteResult(count = Some(0))), ("car", List(new InsertResult(count = Some(1), id = Some("EEE")),
          new InsertResult(count = Some(1), id = Some("III")))))))(ORT.update("dept", obj))

    obj = Map("empno"->7788, "ename"->"SCOTT", "mgr"-> 7839,
        "work:empno"->List(
          Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "empno"->7788, "hours"->8, "empno_mgr"->7839),
          Map("wdate"->java.sql.Date.valueOf("2012-7-10"), "empno"->7788, "hours"->8, "empno_mgr"->7839)),
        "calculated_children"->List(Map("x"->5)), "deptno"->40)
    assertResult(new UpdateResult(count = Some(1), children =
      List((null, new DeleteResult(count = Some(2))),
        ("work:empno", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1)))))))(ORT.update("emp", obj))

    //no child record is updated since no relation is found with car
    obj = Map("empno"->7788, "ename"->"SCOTT", "mgr"-> 7839,
        "calculated_children"->List(Map("x"->5)), "deptno"->40,
        "car"-> List(Map("nr" -> "AAA", "name"-> "GAZ", "deptno" -> 15)))
    assertResult(new UpdateResult(count = Some(1)))(ORT.update("emp", obj))

    //ambiguous relation is found with work
    obj = Map("empno"->7788, "ename"->"SCOTT", "mgr"-> 7839,
        "work"->List(Map("wdate"->"2012-7-9", "empno"->7788, "hours"->8, "empno_mgr"->7839),
              Map("wdate"->"2012-7-10", "empno"->7788, "hours"->8, "empno_mgr"->7839)),
        "calculated_children"->List(Map("x"->5)), "deptno"->40)
    intercept[Exception](ORT.update("emp", obj))

    //child foreign key is also its primary key (one to one relation)
    obj = Map("deptno" -> 60, "dname" -> "POLAR BEAR", "loc" -> "ALASKA",
              "dept_addr" -> List(Map("addr" -> "Halibut", "zip_code" -> "1010")))
    assertResult(new UpdateResult(count = Some(1), children =
      List(("dept_addr", List(new UpdateResult(count = Some(1)))))))(ORT.update("dept", obj))

    //value clause test
    obj = Map("nr" -> "4444", "deptnr" -> 10)
    assertResult(new UpdateResult(count = Some(1)))(ORT.update("car", obj))
    obj = Map("nr" -> "4444", "deptnr" -> -1)
    intercept[TresqlException](ORT.update("car", obj))

    //update only children (no first level table column updates)
    obj = Map("nr" -> "4444", "tyres" -> List(Map("brand" -> "GOOD YEAR", "season" -> "S"),
        Map("brand" -> "PIRELLI", "season" -> "W")))
    assertResult(new UpdateResult(None, children =
      List((null, new DeleteResult(count = Some(0))),
        ("tyres", List(new InsertResult(count = Some(1), id = Some(10017)),
          new InsertResult(count = Some(1), id = Some(10018)))))))(ORT.update("car", obj))

    //delete children
    obj = Map("nr" -> "4444", "name" -> "LAMBORGHINI", "tyres" -> List())
    assertResult(new UpdateResult(count = Some(1), children =
      List((null, new DeleteResult(count = Some(2))), ("tyres", List()))))(ORT.update("car", obj))

    //update three level, for the first second level object it's third level is empty
    obj = Map("deptno" -> 10013, "emp" -> List(
        Map("ename" -> "ELKHADY",
            "work:empno" -> List()),
        Map("ename" -> "GUNTER",
            "work:empno" -> List(
            Map("wdate" -> java.sql.Date.valueOf("2014-08-27"), "hours" -> 8),
            Map("wdate" -> java.sql.Date.valueOf("2014-08-28"), "hours" -> 8)))))
    assertResult(new UpdateResult(None, children =
      List((null, new DeleteResult(count = Some(0))),
        ("emp", List(new InsertResult(count = Some(1), children =
          List(("work:empno",List())), id = Some(10019)),
          new InsertResult(count = Some(1), children = List(("work:empno", List(new InsertResult(count = Some(1)),
            new InsertResult(count = Some(1))))), id = Some(10020)))))))(ORT.update("dept", obj))
    //delete third level children
    obj = Map("deptno" -> 10013, "emp" -> List(
        Map("ename" -> "ELKHADY",
            "work:empno" -> List()),
        Map("ename" -> "GUNTER",
            "work:empno" -> List())))
    assertResult(new UpdateResult(None, children = List((null, new DeleteResult(count = Some(2))),
      ("emp", List(new InsertResult(count = Some(1), children =
        List(("work:empno", List())), id = Some(10021)),
        new InsertResult(count = Some(1), children =
          List(("work:empno", List())), id = Some(10022)))))))(ORT.update("dept", obj))


    println("\n--- DELETE ---\n")

    assertResult(new DeleteResult(count = Some(1)))(ORT.delete("emp", 7934))

    println("\n--- SAVE - merge children ---\n")

    obj = Map("dname" -> "SALES", "loc" -> "WASHINGTON", "calculated_field" -> 222,
      "emp[+-=]" -> List(
        Map("empno" -> 7499, "ename" -> "ALLEN SMITH", "job" -> "SALESMAN", "mgr" -> 7698,
            "mgr_name" -> null, "deptno" -> 30),
        Map("empno" -> 7654, "ename" -> "MARTIN BLAKE", "job" -> "SALESMAN", "mgr" -> 7698,
            "mgr_name" -> null, "deptno" -> 30),
        Map("empno" -> null, "ename" -> "DEISE ROSE", "job" -> "SALESGIRL", "mgr" -> 7698,
            "mgr_name" -> null, "deptno" -> 30),
        Map("empno" -> 7698, "ename" -> "BLAKE", "job" -> "SALESMAN", "mgr" -> 7839,
            "mgr_name" -> null, "deptno" -> 30)),
      "calculated_children" -> List(Map("x" -> 5)), "deptno" -> 30)
      assertResult(new UpdateResult(count = Some(1), children =
        List((null, new DeleteResult(count = Some(3))), ("emp[+-=]", List(new UpdateResult(count = Some(1)),
          new UpdateResult(count = Some(1)), new InsertResult(count = Some(1), id = Some(10023)),
          new UpdateResult(count = Some(1)))))))(ORT.update("dept", obj))

    obj = Map("empno" -> 7788, "ename"->"SCOTT", "mgr"-> 7839,
      "work:empno[+-=]" -> List(
        Map("wdate"->java.sql.Date.valueOf("2012-7-12"), "empno"->7788, "hours"->10, "empno_mgr"->7839),
        Map("wdate"->java.sql.Date.valueOf("2012-7-13"), "empno"->7788, "hours"->3, "empno_mgr"->7839)),
      "calculated_children"->List(Map("x"->5)), "deptno"->20)
    assertResult( new UpdateResult(count = Some(1), children =
      List((null, new DeleteResult(count = Some(2))),
        ("work:empno[+-=]", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1)))))))(ORT.update("emp", obj))

    obj = Map("dname"->"DEVELOPMENT", "loc"->"DETROIT", "calculated_field"-> 222,
        "emp[+-=]"->List(
          Map("empno"->null, "ename"->"AMY", "mgr"->7788, "job"-> "SUPERVIS", "mgr_name"->null, "deptno"->40,
            "work:empno[+-=]"->List(
              Map("wdate"->java.sql.Date.valueOf("2012-7-12"), "empno"->null, "hours"->5, "empno_mgr"->7839),
              Map("wdate"->java.sql.Date.valueOf("2012-7-13"), "empno"->null, "hours"->2, "empno_mgr"->7839))),
          Map("empno"->null, "ename"->"LENE", "mgr"->7566, "job"-> "SUPERVIS", "mgr_name"->null, "deptno"->40,
            "work:empno[+-=]"->List(
              Map("wdate"->java.sql.Date.valueOf("2012-7-14"), "empno"->null, "hours"->5, "empno_mgr"->7839),
              Map("wdate"->java.sql.Date.valueOf("2012-7-15"), "empno"->null, "hours"->2, "empno_mgr"->7839)))),
        "calculated_children"->List(Map("x"->5)), "deptno"->40)
    assertResult(new UpdateResult(count = Some(1), children =
      List((null, new DeleteResult(count = Some(2))),
        ("emp[+-=]", List(new InsertResult(count = Some(1), children =
          List(("work:empno[+-=]", List(new InsertResult(count = Some(1)),
            new InsertResult(count = Some(1))))), id = Some(10024)),
          new InsertResult(count = Some(1), children =
            List(("work:empno[+-=]", List(new InsertResult(count = Some(1)),
              new InsertResult(count = Some(1))))), id = Some(10025)))))))(
        ORT.update("dept", obj))

    obj = Map("empno"->7788, "ename"->"SCOTT", "mgr"-> 7839,
        "work:empno[+-=]"->List(),
        "calculated_children"->List(Map("x"->5)), "deptno"->20)
    assertResult(new UpdateResult(count = Some(1), children =
      List((null,new DeleteResult(count = Some(2))), ("work:empno[+-=]", List()))))(ORT.update("emp", obj))

    println("\n---- Multiple table INSERT, UPDATE ------\n")

    obj = Map("dname" -> "SPORTS", "addr" -> "Brisbane", "zip_code" -> "4000")
    assertResult(new InsertResult(count = Some(1), children =
      List((null, new InsertResult(count = Some(1), id = Some(10026)))),
      id = Some(10026)))(ORT.insertMultiple(obj, "dept", "dept_addr")())

    obj = Map("deptno" -> 10026, "loc" -> "Brisbane", "addr" -> "Roma st. 150")
    assertResult(new UpdateResult(count = Some(1), children =
      List((null, new UpdateResult(count = Some(1))))))(ORT.updateMultiple(obj, "dept", "dept_addr")())

    assertResult(List(Map(
        "dname" -> "SPORTS", "loc" -> "Brisbane",
        "addr" -> List(Map("addr" -> "Roma st. 150", "zip_code" -> "4000"))))) {
      tresql"dept[dname = 'SPORTS'] {dname, loc, |dept_addr {addr, zip_code} addr}".toListOfMaps
    }

    //update only first table in one to one relationship
    obj = Map("deptno" -> 60, "dname" -> "POLAR BEAR", "loc" -> "ALASKA")
    assertResult(new UpdateResult(Some(1),List((null, new UpdateResult(Some(1))))))(
      ORT.updateMultiple(obj, "dept", "dept_addr")())

    println("\n-------- EXTENDED CASES --------\n")

    //insert, update one to one relationship (pk of the extended table is fk for the base table) with children for the extended table
    obj = Map("dname" -> "PANDA BREEDING",
        "dept_addr" -> List(Map("addr" -> "Chengdu", "zip_code" -> "2000",
            "dept_sub_addr" -> List(Map("addr" -> "Jinli str. 10", "zip_code" -> "CN-1234"),
                Map("addr" -> "Jinjiang District", "zip_code" -> "CN-1234")))))
    assertResult(new InsertResult(count = Some(1), children =
      List(("dept_addr", List(new InsertResult(count = Some(1), children =
        List(("dept_sub_addr", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1))))), id = Some(10027))))), id = Some(10027))) {ORT.insert("dept", obj)}

    obj = Map("deptno" -> 10027, "dname" -> "PANDA BREEDING",
      "dept_addr" -> List(Map("addr" -> "Chengdu", "zip_code" -> "CN-1234",
        "dept_sub_addr" -> List(Map("addr" -> "Jinli str. 10", "zip_code" -> "CN-1234"),
          Map("addr" -> "Jinjiang District", "zip_code" -> "CN-1234")))))
    assertResult(new UpdateResult(count = Some(1), children =
      List(("dept_addr", List(new UpdateResult(count = Some(1), children =
        List((null, new DeleteResult(count = Some(2))),
          ("dept_sub_addr", List(new InsertResult(count = Some(1)),
            new InsertResult(count = Some(1))))))))))) {ORT.update("dept", obj)}

    println("\n-------- LOOKUP object editing --------\n")
    //edit lookup object
    obj = Map("brand" -> "DUNLOP", "season" -> "W", "carnr" -> Map("name" -> "VW"))
    assertResult(List(("DUNLOP", "W"))) {
      ORT.insert("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[brand = 'DUNLOP' & season = 'W' & carnr = (car[name = 'VW']{nr})]{brand, season}"
        .map(r => r.brand -> r.season).toList
    }
    obj = Map("brand" -> "CONTINENTAL", "season" -> "W", "carnr" -> Map("nr" -> "UUU", "name" -> "VW"))
    assertResult(List(("CONTINENTAL", "W"))) {
      ORT.insert("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[brand = 'CONTINENTAL' & season = 'W' & carnr = (car[nr = 'UUU']{nr})]{brand, season}"
        .map(r => r.brand -> r.season).toList
    }
    obj = Map("nr" -> 10029, "season" -> "S", "carnr" -> Map("name" -> "SKODA"))
    assertResult(List(("DUNLOP", "S"))) {
      ORT.update("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[season = 'S' & carnr = (car[name = 'SKODA']{nr})]{brand, season}"
        .map(r => r.brand -> r.season).toList
    }
    obj = Map("nr" -> 10029, "brand" -> "DUNLOP", "carnr" -> Map("nr" -> "UUU", "name" -> "VOLKSWAGEN"))
    assertResult(List(("DUNLOP", "S", "VOLKSWAGEN"))) {
      ORT.update("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[brand = 'DUNLOP' & carnr = (car[nr = 'UUU']{nr})]{brand, season, (car[nr = tyres.carnr]{name}) car}"
        .map(r => (r.brand, r.season, r.car)).toList
    }
    //one to one relationship with lookup for extended table
    obj = Map("dname" -> "MARKETING", "addr" -> "Valkas str. 1",
        "zip_code" -> "LV-1010", "addr_nr" -> Map("addr" -> "Riga"))
    assertResult(new InsertResult(count = Some(1), children =
      List((null, List(10033,
        new InsertResult(count = Some(1), id = Some(10032))))),
      id = Some(10032))) { ORT.insertMultiple(obj, "dept", "dept_addr")() }
    obj = Map("deptno" -> 10032, "dname" -> "MARKET", "addr" -> "Valkas str. 1a",
      "zip_code" -> "LV-1010", "addr_nr" -> Map("nr" -> 10033, "addr" -> "Riga, LV"))
    assertResult(new UpdateResult(count = Some(1), children =
      List((null, List(10033, new UpdateResult(count = Some(1))))))) {
      ORT.updateMultiple(obj, "dept", "dept_addr")()
    }
    obj = Map("deptno" -> 10032, "dname" -> "MARKETING", "addr" -> "Liela str.",
      "zip_code" -> "LV-1010", "addr_nr" -> Map("addr" -> "Saldus"))
    assertResult(new UpdateResult(count = Some(1), children =
      List((null, List(10034, new UpdateResult(count = Some(1))))))) {
      ORT.updateMultiple(obj, "dept", "dept_addr")()
    }
    //insert of lookup object where it's pk is present but null
    obj = Map("nr" -> 10029, "brand" -> "DUNLOP", "carnr" -> Map("nr" -> null, "name" -> "AUDI"))
    assertResult( List(("DUNLOP", "S", "AUDI"))) {
      ORT.update("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[brand = 'DUNLOP' & carnr = (car[name = 'AUDI']{nr})]{brand, season, (car[nr = tyres.carnr]{name}) car}"
        .map(r => (r.brand, r.season, r.car)).toList
    }

    println("\n----------------- Multiple table INSERT UPDATE extended cases ----------------------\n")

    obj = Map("dname" -> "AD", "ename" -> "Lee", "wdate" -> java.sql.Date.valueOf("2000-01-01"), "hours" -> 3)
    assertResult( new InsertResult(count = Some(1), children =
      List((null, new InsertResult(count = Some(1), id = Some(10036))),
        (null, new InsertResult(count = Some(1), id = Some(10036)))),
      id = Some(10036))) { ORT.insertMultiple(obj, "dept", "emp", "work:empno")() }

    obj = Map("deptno" -> 10036, "wdate" -> java.sql.Date.valueOf("2015-10-01"), "hours" -> 4)
    assertResult(new UpdateResult(None, List((null, new UpdateResult(Some(1))), (null, new UpdateResult(Some(1)))))) {
      ORT.updateMultiple(obj, "dept", "emp", "work:empno")()
    }

    obj = Map("deptno" -> 10036, "work:empno" -> List(Map("wdate" -> java.sql.Date.valueOf("2015-10-10"), "hours" -> 5)))
    assertResult(new UpdateResult(None, children =
      List((null,new UpdateResult(None, children = List(
        (null,new DeleteResult(count = Some(1))),
        ("work:empno", List(new InsertResult(count = Some(1)))))))))) { ORT.updateMultiple(obj, "dept", "emp")() }

    obj = Map("deptno" -> 10036, "dname" -> "ADVERTISING", "ename" -> "Andy")
    assertResult(new UpdateResult(count = Some(1), children = List((null,new UpdateResult(count = Some(1)))))) {
      ORT.updateMultiple(obj, "dept", "emp")()
    }

    obj = Map("dname" -> "DIG", "emp" -> List(Map("ename" -> "O'Jay",
        "work:empno" -> List(Map("wdate" -> java.sql.Date.valueOf("2010-05-01"), "hours" -> 7)))), "addr" -> "Tvaika 1")
    assertResult( new InsertResult(count = Some(1), children =
      List(
        ("emp", List(new InsertResult(count = Some(1), children =
          List(("work:empno", List(new InsertResult(count = Some(1))))), id = Some(10038)))),
        (null, new InsertResult(count = Some(1), id = Some(10037)))), id = Some(10037))) {
      ORT.insertMultiple(obj, "dept", "dept_addr")()
    }

    obj = Map("dname" -> "GARAGE", "name" -> "Nissan", "brand" -> "Dunlop", "season" -> "S")
    assertResult(new InsertResult(count = Some(1), children = List(
      (null,new InsertResult(count = Some(1), id = Some(10039))),
      (null,new InsertResult(count = Some(1), id = Some(10039)))),
      id = Some(10039))) {
      ORT.insertMultiple(obj, "dept", "car", "tyres")()
    }

    obj = Map("deptno" -> 10039, "dname" -> "STOCK", "name" -> "Nissan", "brand" -> "PIRELLI", "season" -> "W")
    //obj = Map("dname" -> "STOCK", "name" -> "Nissan", "brand" -> "PIRELLI", "season" -> "W")
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null,new UpdateResult(count = Some(1))),
      (null,new UpdateResult(count = Some(1)))))) { ORT.updateMultiple(obj, "dept", "car", "tyres")() }

    obj = Map("deptno" -> 10039, "dname" -> "STOCK", "name" -> "Nissan Patrol",
        "tyres" -> List(Map("brand" -> "CONTINENTAL", "season" -> "W")))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null,new UpdateResult(count = Some(1), children = List(
        (null,new DeleteResult(count = Some(1))),
        ("tyres", List(new InsertResult(count = Some(1), id = Some(10040)))))))))) {
      ORT.updateMultiple(obj, "dept", "car")()
    }

    obj = Map("dname" -> "NEW STOCK", "name" -> "Audi Q7",
        "tyres" -> List(Map("brand" -> "NOKIAN", "season" -> "S")))
    assertResult(new InsertResult(count = Some(1), children = List(
      (null,new InsertResult(count = Some(1), children = List(
        ("tyres", List(new InsertResult(count = Some(1), id = Some(10042))))),
        id = Some(10041)))), id = Some(10041))) { ORT.insertMultiple (obj, "dept", "car")() }

    println("\n-------- LOOKUP extended cases - chaining, children --------\n")

    obj = Map("brand" -> "Nokian", "season" -> "W", "carnr" ->
      Map("name" -> "Mercedes", "deptnr" -> Map("dname" -> "Logistics")))
    assertResult(List(("Nokian", "W"))) {
      ORT.insert("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[brand = 'Nokian' & season = 'W' & carnr = (car[name = 'Mercedes' & deptnr = (dept[dname = 'Logistics']{deptno})]{nr})]{brand, season}"
        .map(r => r.brand -> r.season).toList
    }

    obj = Map("nr" -> 10045, "brand" -> "Nokian", "season" -> "S", "carnr" ->
      Map("nr" -> 10044, "name" -> "Mercedes Benz", "deptnr" -> Map("deptno" -> 10043, "dname" -> "Logistics dept")))
    assertResult(List(("Nokian", "S"))) {
      ORT.update("tyres", obj)
      println(s"\nResult check:")
      tresql"tyres[brand = 'Nokian' & season = 'S' & carnr = (car[name = 'Mercedes Benz' & deptnr = (dept[dname = 'Logistics dept']{deptno})]{nr})]{brand, season}"
        .map(r => r.brand -> r.season).toList
    }

    obj = Map("dname" -> "MILITARY", "loc" -> "Alabama", "emp" -> List(
      Map("ename" -> "Selina", "mgr" -> null),
      Map("ename" -> "Vano", "mgr" -> Map("ename" -> "Carlo", "deptno" -> Map("dname" -> "Head")))))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emp", List(List(null, new InsertResult(count = Some(1), id = Some(10047))),
        List(10049, new InsertResult(count = Some(1), id = Some(10050)))))), id = Some(10046))) {
      ORT.insert("dept", obj)
    }

    obj = Map("deptno" -> 10046, "dname" -> "METEO", "loc" -> "Texas", "emp[+-=]" -> List(
      Map("ename" -> "Selina", "mgr" -> null),
      Map("ename" -> "Vano", "mgr" -> Map("ename" -> "Pedro", "deptno" -> Map("deptno" -> 10048, "dname" -> "Head")))))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null,new DeleteResult(count = Some(2))),
      ("emp[+-=]", List(List(null, new InsertResult(count = Some(1), id = Some(10051))),
        List(10052, new InsertResult(count = Some(1), id = Some(10053)))))))) {
      ORT.update("dept", obj)
    }

    obj = Map("name" -> "Dodge", "car_usage" -> List(
      Map("empno" -> Map("empno" -> null, "ename" -> "Nicky", "job" -> "MGR", "deptno" -> 10)),
      Map("empno" -> Map("empno" -> 10052, "ename" -> "Lara", "job" -> "MGR", "deptno" -> 10))))
    assertResult(new InsertResult(count = Some(1),
      children = List(("car_usage", List(
        List(10055, new InsertResult(count = Some(1))),
        List(10052, new InsertResult(count = Some(1)))
      ))), id = Some(10054))
    ) { ORT.insert("car", obj) }

    println("\n-------- INSERT, UPDATE, DELETE with additional filter --------\n")
    //insert, update with additional filter
    assertResult(new InsertResult(count = Some(0))){ORT.insert("dummy", Map("dummy" -> 2), "dummy = -1")}
    assertResult(new InsertResult(count = Some(1))){ORT.insert("dummy d", Map("dummy" -> 2), "d.dummy = 2")}
    assertResult(new UpdateResult(count = Some(0))){ORT.update("address a", Map("nr" -> 10033, "addr" -> "gugu"), "a.addr ~ 'Ri'")}
    assertResult(new DeleteResult(count = Some(0))) { ORT.delete("emp e", 10053, "ename ~~ :ename", Map("ename" -> "ivans%")) }
    assertResult(new DeleteResult(count = Some(1))) { ORT.delete("emp e", 10053, "ename ~~ :ename", Map("ename" -> "van%")) }

    println("\n---- Object INSERT, UPDATE ------\n")

    implicit def pohatoMap[T <: Poha](o: T): (String, Map[String, _]) = o match {
      case Car(nr, name) => "car" -> Map("nr" -> nr, "name" -> name)
    }
    assertResult(new InsertResult(count = Some(1), id = Some(8888)))(ORT.insertObj(Car(8888, "OPEL")))
    assertResult(new UpdateResult(count = Some(1)))(ORT.updateObj(Car(8888, "SAAB")))

    println("\n-------- SAVE - extended cases --------\n")

    obj = Map("dname" -> "TRUCK DEPT",
      "car[+=]" -> List(
        Map("name" -> "VOLVO",
          "tyres[+=]" -> List(
            Map("brand" -> "BRIDGESTONE", "season" -> "S",
              "tyres_usage[+=]" -> List(
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-04-25"),
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-05-01"))),
            Map("brand" -> "COPARTNER", "season" -> "W",
              "tyres_usage[+=]" -> List(
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-09-25"),
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-10-01"))))
       ),
       Map("name" -> "TATA",
          "tyres[+=]" -> List(
           Map("brand" -> "METRO TYRE", "season" -> "S",
             "tyres_usage[+=]" -> List(
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-04-25"),
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-05-01"))),
           Map("brand" -> "GRL", "season" -> "W",
             "tyres_usage[+=]" -> List(
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-09-25"),
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-10-01")))))))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("car[+=]", List(new InsertResult(count = Some(1), children = List(
        ("tyres[+=]", List(new InsertResult(count = Some(1), children = List(
          ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
            new InsertResult(count = Some(1))))), id = Some(10058)),
          new InsertResult(count = Some(1), children = List(
            ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
              new InsertResult(count = Some(1))))), id = Some(10059))))), id = Some(10057)),
        new InsertResult(count = Some(1), children = List(
          ("tyres[+=]", List(new InsertResult(count = Some(1), children = List(
            ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
              new InsertResult(count = Some(1))))), id = Some(10061)),
            new InsertResult(count = Some(1), children = List(
              ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
                new InsertResult(count = Some(1))))), id = Some(10062))))), id = Some(10060))))),
      id = Some(10056)))(ORT.insert("dept", obj))

    obj = Map("deptno" -> 10056, "dname" -> "TRUCK DEPT",
      "car[+=]" -> List(
        Map("nr" -> 10057, "name" -> "VOLVO",
          "tyres[+=]" -> List(
            Map("nr" -> 10058, "brand" -> "BRIDGESTONE", "season" -> "S",
              "tyres_usage[+=]" -> List(
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-04-01"))),
            Map("nr" -> null, "brand" -> "ADDO", "season" -> "W",
              "tyres_usage[+=]" -> List(
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-09-25"),
                Map("carnr->" -> "carnr=:#car", "date_from" -> "2016-10-01"))))),
        Map("nr" -> 10060, "name" -> "TATA MOTORS",
          "tyres[+=]" -> List(
           Map("nr" -> 10061, "brand" -> "METRO TYRE", "season" -> "S",
             "tyres_usage[+=]" -> List(
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-04-25"),
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-05-01"))),
           Map("nr" -> 10062, "brand" -> "GRL", "season" -> "W",
             "tyres_usage[+=]" -> List(
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-09-25"),
               Map("carnr->" -> "carnr=:#car", "date_from" -> "2015-10-01")))))))
      assertResult(new UpdateResult(count = Some(1), children = List(
        ("car[+=]", List(new UpdateResult(count = Some(1), children = List(
          ("tyres[+=]", List(new UpdateResult(count = Some(1), children = List(
            ("tyres_usage[+=]", List(new InsertResult(count = Some(1)))))),
            new InsertResult(count = Some(1), children = List(
              ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
                new InsertResult(count = Some(1))))), id = Some(10063)))))),
          new UpdateResult(count = Some(1), children = List(
            ("tyres[+=]", List(new UpdateResult(count = Some(1), children = List(
              ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
                new InsertResult(count = Some(1)))))), new UpdateResult(count = Some(1), children = List(
              ("tyres_usage[+=]", List(new InsertResult(count = Some(1)),
                new InsertResult(count = Some(1)))))))))))))))(ORT.update("dept", obj))

    obj = Map("deptno" -> 10056, "car[+-=]" -> List(
      Map("nr" -> 10060, "tyres[+-=]" -> List(
        Map("brand" -> "BRIDGESTONE", "season" -> "W")
      )),
      Map("nr" -> 10057, "tyres[+-=]" -> List(
        Map("nr" -> 10063, "brand" -> "HANKOOK", "season" -> "S"),
        Map("nr" -> 10059, "brand" -> "FIRESTONE", "season" -> "W"),
        Map("brand" -> "YOKOHAMA", "season" -> "S")
      ))
    ))
    assertResult(new UpdateResult(None, children = List((null, new DeleteResult(count = Some(0))), (
      "car[+-=]", List(new UpdateResult(None, children = List(
      (null, new DeleteResult(count = Some(2))),
      ("tyres[+-=]", List(new InsertResult(count = Some(1), id = Some(10064)))))),
      new UpdateResult(None, children = List(
        (null, new DeleteResult(count = Some(1))),
        ("tyres[+-=]", List(new UpdateResult(count = Some(1)),
          new UpdateResult(count = Some(1)),
          new InsertResult(count = Some(1), id = Some(10065))))))))))
    )(ORT.update("dept", obj))

    obj = Map("deptno" -> 10056, "car[+-=]" -> List(
      Map("nr" -> 10060, "tyres[+-=]" -> List(
        Map("nr" -> 10064, "brand" -> "BRIDGESTONE", "season" -> "S")
      )),
      Map("nr" -> 10057, "tyres[+-=]" -> List(
        Map("nr" -> 10063, "brand" -> "HANKOOK", "season" -> "W"),
        Map("nr" -> 10059, "brand" -> "FIRESTONE", "season" -> "S"),
        Map("nr" -> 10065, "brand" -> "YOKOHAMA", "season" -> "W")
      ))
    ))
    assertResult(new UpdateResult(None, children = List(
      (null, new DeleteResult(count = Some(0))),
      ("car[+-=]", List(new UpdateResult(None, children = List(
        (null, new DeleteResult(count = Some(0))),
        ("tyres[+-=]", List(new UpdateResult(count = Some(1)))))),
        new UpdateResult(None, children = List(
          (null, new DeleteResult(count = Some(0))),
          ("tyres[+-=]", List(new UpdateResult(count = Some(1)),
            new UpdateResult(count = Some(1)),
            new UpdateResult(count = Some(1))))))))))
    )(ORT.update("dept", obj))

    obj = Map("deptno" -> 10056, "car[+-=]" -> List(
      Map("nr" -> 10057, "tyres[+-=]" -> List(
        Map("nr" -> 10063, "brand" -> "HANKOOK", "season" -> "S"),
        Map("nr" -> 10059, "brand" -> "FIRESTONE", "season" -> "W"),
        Map("nr" -> 10065, "brand" -> "YOKOHAMA", "season" -> "S")
      )),
      Map("nr" -> 10060, "tyres[+-=]" -> List(
        Map("brand" -> "BRIDGESTONE", "season" -> "S"),
        Map("brand" -> "BRIDGESTONE", "season" -> "W")
      ))
    ))
    assertResult(new UpdateResult(None, List((null, new DeleteResult(Some(0))), "car[+-=]" -> List(
      new UpdateResult(None, List((null, new DeleteResult(Some(0))), "tyres[+-=]" -> List(
        new UpdateResult(Some(1)), new UpdateResult(Some(1)), new UpdateResult(Some(1))))),
      new UpdateResult(None, List((null, new DeleteResult(Some(1))), "tyres[+-=]" -> List(
        new InsertResult(Some(1), Nil, Some(10066)),
        new InsertResult(Some(1), Nil, Some(10067)))))))))(
    ORT.update("dept", obj))

    println("\n-------- SAVE - extended cases - multiple children --------\n")

    obj = Map("dname" -> "Service", "emp#work:empno" ->
      Map("ename" -> "Sophia", "wdate" -> java.sql.Date.valueOf("2015-10-30"), "hours" -> 2))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emp#work:empno", new InsertResult(count = Some(1), children = List(
        (null, new InsertResult(count = Some(1), id = Some(10069)))),
        id = Some(10069)))), id = Some(10068)))(ORT.insert("dept", obj))

    obj = Map("deptno" -> 10068, "dname" -> "Services", "emp#work:empno[+-=]" ->
      Map("empno" -> 10069, "wdate" -> java.sql.Date.valueOf("2015-10-30"), "hours" -> 8))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new DeleteResult(count = Some(0))),
      ("emp#work:empno[+-=]", new UpdateResult(None, children = List(
        (null, new UpdateResult(count = Some(1)))))))))(ORT.update("dept", obj))

    obj = Map("deptno" -> 10068, "emp#work:empno[=]" ->
      Map("empno" -> 10069, "empno_mgr" -> Map("ename" -> "Jean", "deptno" -> 10068)))
    assertResult(new UpdateResult(None, children = List(
      ("emp#work:empno[=]", new UpdateResult(None, children = List(
        (null, List(10070, new UpdateResult(count = Some(1))))))))))(ORT.update("dept", obj))

    obj = Map("nr" -> 10057, "is_active" -> true, "emp#car_usage" ->
      Map("ename" -> "Peter", "date_from" -> "2015-11-02",
        "deptno" -> Map("dname" -> "Supervision")))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null,new DeleteResult(count = Some(0))),
      ("emp#car_usage", List(10071, new InsertResult(count = Some(1), children = List(
        (null,new InsertResult(count = Some(1)))), id = Some(10072)))))))(ORT.update("car", obj))

    println("\n--- LOOKUP extended case - separate lookup expression from previous insert expr values ---\n")
    obj = Map("dname" -> "Design", "name" -> "Tesla", "date_from" -> "2015-11-20",
      "empno" -> Map("ename" -> "Inna", "deptno" -> 10068))
    assertResult(new InsertResult(count = Some(1), children = List(
      (null,new InsertResult(count = Some(1), id = Some(10073))),
      (null, List(10074, new InsertResult(count = Some(1))))), id = Some(10073)))(
      ORT.insertMultiple(obj, "dept", "car", "car_usage")())

    println("\n--- Delete all children with save options specified ---\n")
    obj = Map("nr" -> 10035, "tyres[+-=]" -> Nil)
    assertResult(new UpdateResult(None, children = List(
      (null, new DeleteResult(count = Some(1))),
      ("tyres[+-=]",List()))))(ORT.update("car", obj))

    println("\n--- Name resolving ---\n")
    obj = Map("wdate" -> java.sql.Date.valueOf("2017-03-10"), "hours" -> 8, "emp" -> "SCOTT", "emp_mgr" -> "KING",
      "emp" -> "SCOTT",
      "emp->" -> "empno=emp[ename = _] {empno}",
      "emp_mgr" -> "KING",
      "emp_mgr->" -> "empno_mgr=emp[ename = :emp_mgr] {empno}")
    assertResult(new InsertResult(Some(1), Nil, None))(ORT.insert("work", obj))

    obj = Map("deptno" -> 10, "emp[=]" ->
      List(
        Map("empno" -> 7782, "ename" -> "CLARK", "mgr" -> "KING", "mgr->" -> "mgr=emp[ename = _]{empno}"),
        Map("empno" -> 7839, "ename" -> "KING", "mgr" -> null, "mgr->" -> "mgr=emp[ename = _]{empno}")
      )
    )
    assertResult(new UpdateResult(None, List("emp[=]" -> List(new UpdateResult(Some(1)), new UpdateResult(Some(1))))))(
      ORT.update("dept", obj))

    obj = Map("empno" -> 7369, "sal" -> 850, "dept-name" -> "SALES",
      "dept-name" -> "SALES", "dept-name->" -> "deptno=dept[dname = _]{deptno}")
    assertResult(new UpdateResult(Some(1)))(ORT.update("emp", obj))

    obj = Map("deptno" -> 10037, "loc" -> "Latvia", "zip_code" -> "LV-1005", "addr" -> "Tvaika iela 48",
      "address-city" -> "Riga, LV", "address-city->" -> "addr_nr=address[addr = _]{nr}")
    assertResult(new UpdateResult(Some(1), List((null, new UpdateResult(Some(1))))))(
      ORT.updateMultiple(obj, "dept", "dept_addr")())

    println("\n-------- SAVE with additional filter for children --------\n")

    obj = Map("dname" -> "Temp", "addr" -> "Field", "zip_code" -> "none", "dept_sub_addr" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      )
    )
    assertResult(new InsertResult(Some(1),
      List((null, new InsertResult(
        Some(1),
        List("dept_sub_addr" -> List(new InsertResult(Some(1)), new InsertResult(Some(1)))),
        Some(10075))
      )), Some(10075)))(ORT.insertMultiple(obj, "dept", "dept_addr")())

    obj = Map("dname" -> "Temp", "addr" -> "Field", "zip_code" -> "none", "dept_sub_addr" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      ), "filter_condition" -> false
    )
    assertResult(new InsertResult(Some(0), Nil, Some(10076)))(
      ORT.insertMultiple(obj, "dept", "dept_addr")(":filter_condition = true"))

    obj = Map("dname" -> "Temp1", "addr" -> "Field1", "zip_code" -> "none",
      "dept_sub_addr dsa|dsa.addr = null,:filter_condition = true,dsa.addr = null" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      ), "filter_condition" -> false
    )
    assertResult(new InsertResult(Some(1),
      List((null, new InsertResult(
        Some(1),
        List("dept_sub_addr dsa|dsa.addr = null,:filter_condition = true,dsa.addr = null" ->
          List(new InsertResult(Some(0)), new InsertResult(Some(0)))),
        Some(10077))
      )), Some(10077)))(ORT.insertMultiple(obj, "dept", "dept_addr")())

    obj = Map("deptno" -> 10077, "dname" -> "Temp2", "addr" -> "Field2", "zip_code" -> "----",
      "dept_sub_addr[+-=]|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      ),
      "emp[+-=] e|e.ename = null,e.ename = null,e.ename = null" ->
      List(Map("ename" -> "X"), Map("empno" -> 7369, "ename" -> "Y")),
      "filter_condition" -> false
    )
    assertResult(
      new UpdateResult(
        Some(1),
        List(
          (null, new DeleteResult(Some(0))),
          "emp[+-=] e|e.ename = null,e.ename = null,e.ename = null" ->
            List(new InsertResult(Some(0), Nil, Some(10079)), new InsertResult(Some(0), id = Some(7369))),
          (null, new UpdateResult(
            Some(1),
            List(
              (null, new DeleteResult(Some(0))),
              "dept_sub_addr[+-=]|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
                List(new InsertResult(Some(0)), new InsertResult(Some(0)))
            )))
        )
      )
    )(ORT.updateMultiple(obj, "dept", "dept_addr")())

    //should not delete dept_sub_addr children
    obj = Map("deptno" -> 10075, "addr" -> "Field alone",
      "dept_sub_addr[+-=] dsa|dsa.addr = null,dsa.addr = null,dsa.addr = null" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      )
    )
    assertResult(new UpdateResult(
      None,
      List((null,
        new UpdateResult(
          Some(1),
          List((
            null, new DeleteResult(Some(0))),
            "dept_sub_addr[+-=] dsa|dsa.addr = null,dsa.addr = null,dsa.addr = null" ->
              List(new InsertResult(Some(0)), new InsertResult(Some(0)))
          )
        )
      ))
    ))(ORT.updateMultiple(obj, "dept", "dept_addr")())

    //should delete dept_sub_addr children
    obj = Map("deptno" -> 10075, "addr" -> "Field alone",
      "dept_sub_addr[+-=] dsa|dsa.addr = null, null, dsa.addr = null" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      )
    )
    assertResult(new UpdateResult(
      None,
      List((null,
        new UpdateResult(
          Some(1),
          List(
            (null, new DeleteResult(Some(2))),
            "dept_sub_addr[+-=] dsa|dsa.addr = null, null, dsa.addr = null" ->
              List(new InsertResult(Some(0)), new InsertResult(Some(0)))
          )
        )
      ))
    ))(ORT.updateMultiple(obj, "dept", "dept_addr")())

    println("----- SAVE with resolver for self column -----")
    //resolving the same column
    obj = Map("deptno" -> 10004,
      "dname" -> "legal",
      "dname->" -> "dname=upper(_)")
    assertResult(new UpdateResult(Some(1)))(
      ORT.updateMultiple(obj, "dept")())

    obj = Map("dname" -> "attorney", "dname->" -> "dname=upper(_)")
    assertResult(new InsertResult(Some(1), Nil, Some(10080)))(ORT.insert("dept", obj))

    obj = Map("deptno" -> 10080, "dname" -> "devop", "dname->" -> "dname=upper(_)")
    assertResult(new UpdateResult(Some(1)))(ORT.update("dept", obj))

    assertResult(List("DEVOP"))(tresql"dept[dname = 'DEVOP']{dname}".map(_.dname).toList)

    println("----- SAVE to multiple tables with children having references to both parent tables -----")

    obj = Map("dname" -> "Radio", "ename" -> "John",
      "emp:mgr" -> List(Map("ename" -> "Agnes", "deptno" -> 10004)))
    assertResult(new InsertResult(
      Some(1),
      List((null, new InsertResult(
        Some(1),
        List("emp:mgr" -> List(
          new InsertResult(Some(1), id = Some(10082)))),
        id = Some(10081)
      ))),
      id = Some(10081)
    )) (ORT.insertMultiple(obj, "dept", "emp")())


    println("----- SAVE record together with subrecord like manager together with emp -----")

    obj = Map("ename" -> "Nico", "dname" -> "Services", "dname->" -> "deptno=dept[dname = _]{deptno}",
      "emp[+-=]" -> List(
        Map("ename" -> "Martin", "dname" -> "Services", "dname->" -> "deptno=dept[dname = _]{deptno}")
      )
    )
    assertResult(new InsertResult(
      Some(1), List("emp[+-=]" -> List(
        new InsertResult(Some(1), id = Some(10084))
      )), id = Some(10083)
    ))(ORT.insert("emp", obj))

    obj = Map("ename" -> "Vytas", "empno" -> tresql"emp[ename = 'Nico']{empno}".unique[Int],
      "emp[+-=]" -> List(
        Map("ename" -> "Martins", "empno" -> tresql"emp[ename = 'Martin']{empno}".unique[Int])
      )
    )
    assertResult(new UpdateResult(
      Some(1),
      List((null, new DeleteResult(Some(0))),
        "emp[+-=]" -> List(new UpdateResult(Some(1)))
      )
    ))(ORT.update("emp", obj))

    obj = Map("ename" -> "Vytas", "empno" -> tresql"emp[ename = 'Vytas']{empno}".unique[Int],
      "emp[+-=]" -> List(
        Map("ename" -> "Martins",
          "dname" -> "Services", "dname->" -> "deptno=dept[dname = _]{deptno}",
          "empno" -> tresql"emp[ename = 'Martins']{empno}".unique[Int]),
        Map("ename" -> "Sergey",
          "dname" -> "Services", "dname->" -> "deptno=dept[dname = _]{deptno}")
      )
    )
    assertResult(new UpdateResult(
      Some(1),
      List((null, new DeleteResult(Some(0))), "emp[+-=]" -> List(new UpdateResult(Some(1)), new InsertResult(Some(1), id = Some(10085))))
    ))(ORT.update("emp", obj))

    obj = Map("ename" -> "Vytas", "empno" -> tresql"emp[ename = 'Vytas']{empno}".unique[Int],
      "emp[+-=]" -> List(
        Map("ename" -> "Martino", "empno" -> tresql"emp[ename = 'Martins']{empno}".unique[Int]),
        Map("ename" -> "Sergio", "empno" -> tresql"emp[ename = 'Sergey']{empno}".unique[Int])
      )
    )
    assertResult(new UpdateResult(
      Some(1),
      List((null, new DeleteResult(Some(0))),
        "emp[+-=]" -> List(new UpdateResult(Some(1)), new UpdateResult(Some(1)))
      )
    ))(ORT.update("emp", obj))

    assertResult(List(("Vytas", null), ("Martino", "Vytas"), ("Sergio", "Vytas")))(
      tresql"emp[ename in ('Vytas', 'Martino', 'Sergio')]{ename, (emp e[e.empno = emp.mgr] {ename}) mgr}#(empno)"
        .map(r => (r.ename, r.mgr)).toList)


    println("----- UPSERT test -----")

    tresql"-dept_addr[10077]"

    obj = Map("deptno" -> 10077, "loc" -> "Asia", "addr" -> "Singapore")
    assertResult(new UpdateResult(
      Some(1), List((null, new InsertResult(Some(1), id = Some(10077))))
    ))(ORT.updateMultiple(obj, "dept", "dept_addr")())

    assertResult(List((10077, "Temp2", "Singapore", null)))(
      tresql"dept/dept_addr![deptno = 10077]{deptno, dname, loc, addr, zip_code}#(1)"
        .map(r => (r.deptno, r.dname, r.addr, r.zip_code)).toList)

    obj = Map("deptno" -> 10077, "loc" -> "Asia", "zip_code" -> "560252")
    assertResult(new UpdateResult(Some(1), List((null, new UpdateResult(Some(1))))))(
      ORT.updateMultiple(obj, "dept", "dept_addr")())

    assertResult(List((10077, "Temp2", "Singapore", "560252")))(
      tresql"dept/dept_addr![deptno = 10077]{deptno, dname, loc, addr, zip_code}#(1)"
        .map(r => (r.deptno, r.dname, r.addr, r.zip_code)).toList)

    obj = Map("deptno" -> tresql"dept[dname = 'Temp']{deptno}".unique[Int],
      "emp[+-=]" -> List(Map("empno" -> 987654, "ename" -> "Betty")))

    assertResult(new UpdateResult(
      None, List((null, new DeleteResult(Some(0))), "emp[+-=]" -> List(new InsertResult(Some(1), id = Some(987654)))))
    )(ORT.update("dept", obj))

    assertResult("Betty")(tresql"emp[ename = 'Betty']{ename}".unique[String])

    println("----- Exception handling test -----")
    assertResult("emp[+-=]") {
      try {
        obj = Map("deptno" -> tresql"dept[dname = 'Temp']{deptno}".unique[Int],
          "emp[+-=]" -> List(Map("empno" -> 987654, "ename" -> "Betty",
            "work:empno" -> List(Map("hours" -> 4)))))
        ORT.update("dept", obj)
      } catch {
        case e: ChildSaveException => e.name
      }
    }

    //primary key component not specified error must be thrown
    obj = Map("car_nr" -> "1111")
    intercept[TresqlException](ORT.insert("car_usage", obj))
    obj = Map("date_from" -> "2013-10-24")
    intercept[TresqlException](ORT.insert("car_usage", obj))
    obj = Map("empno" -> 7839)
    intercept[TresqlException](ORT.insert("car_usage", obj))

    ortKeyTests
    ortOnExtraDatabase
    ortPkName_ne_BindVarName
    ortSaveToMultipleTablesWithRepeatingColName
    ortForInsertForUpdateOptionalFlags
    ortLookupByKey
  }

  private def ortKeyTests(implicit resources: Resources) = {
    println("------ UPDATE by key test ------")

    var obj: Map[String, Any] = Map("dname" -> "METEO", "loc" -> "Florida", "emp[ename][+-=]" -> List(
      Map("ename" -> "Selina", "job" -> "Observer" , "mgr" -> "KING", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
      Map("ename" -> "Paul", "job" -> "Observer" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
      Map("ename" -> "Ziko", "job" -> "Apprent" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
    ))
    assertResult(List(("Florida",
      List(("Paul", "Observer", "Selina"), ("Selina", "Observer", "KING"), ("Ziko", "Apprent", "Selina"))))) {
      ORT.update("dept[dname]", obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'METEO']{loc, |emp e{ename, job, (emp m[m.empno = e.mgr]{m.ename}) mgr}#(1) emps}"
        .map(d => d.loc -> d.emps.map(e => (e.ename, e.job, e.mgr)).toList).toList
    }

    obj = Map("dname" -> "METEO", "loc" -> "Florida", "emp[ename][+-=]" -> List(
      Map("ename" -> "Selina", "job" -> "Observer" , "mgr" -> "KING", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
      Map("ename" -> "Paul", "job" -> "Gardener" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
      Map("ename" -> "Mia", "job" -> "Gardener" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
    ))
    assertResult(List(("Florida",
      List(("Selina", "Observer", "KING"), ("Paul", "Gardener", "Selina"), ("Mia", "Gardener", "Selina"))))) {
      ORT.update("dept[dname]", obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'METEO']{loc, |emp e{ename, job, (emp m[m.empno = e.mgr]{m.ename}) mgr}#(~1) emps}"
        .map(d => d.loc -> d.emps.map(e => (e.ename, e.job, e.mgr)).toList).toList
    }

    obj = Map("dname" -> "METEO", "loc" -> "Arizona", "emp[ename, deptno, mgr][+-=]" -> List(
      Map("ename" -> "Selina", "job" -> "Driver" , "mgr" -> "KING", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
      Map("ename" -> "Paul", "job" -> "Teacher" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}"),
    ))
    assertResult(List(("Arizona",
      List(("Paul", "Teacher", "Selina"), ("Selina", "Driver", "KING"))))) {
      ORT.update("dept[dname]", obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'METEO']{loc, |emp e{ename, job, (emp m[m.empno = e.mgr]{m.ename}) mgr}#(1) emps}"
        .map(d => d.loc -> d.emps.map(e => (e.ename, e.job, e.mgr)).toList).toList
    }

    obj = Map("dname" -> "METEO", "loc" -> "Nevada", "emp[ename, deptno][+-=]" ->
      List(
        Map("ename" -> "Selina", "job" -> "Driver" , "mgr" -> "Vytas", "mgr->" -> "mgr=emp[ename = :mgr]{empno}",
          "car_usage[empno, car_nr][+-=]" -> Nil),
        Map("ename" -> "Paul", "job" -> "Pilot" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}",
          "car_usage[empno, car_nr][+-=]" ->
            List(
              Map("car" -> "VOLKSWAGEN", "car->" -> "car_nr=car[name = :car]{nr}", "date_from" -> "2021-11-10"),
              Map("car" -> "FIAT", "car->" -> "car_nr=car[name = :car]{nr}", "date_from" -> "2021-11-11"),
            ))
      ),
    )
    assertResult(List(("Nevada",
      List(
        ("Paul", "Pilot", "Selina", List(("FIAT","2021-11-11"), ("VOLKSWAGEN","2021-11-10"))),
        ("Selina", "Driver", "Vytas", Nil)
      )
    ))) {
      ORT.update("dept[dname]", obj)
      println(s"\nResult check:")
      tresql"""dept[dname = 'METEO']{loc, |emp e{ename, job, (emp m[m.empno = e.mgr]{m.ename}) mgr,
              |car_usage{(car[nr = car_nr]{name}) car, date_from}#(1) cars }#(1) emps}"""
        .map(d => d.loc -> d.emps.map(e => (e.ename, e.job, e.mgr,
          e.cars.map(c => c.car -> c.date_from.toString).toList)).toList).toList
    }

    obj = Map("dname" -> "METEO", "loc" -> "Montana", "emp[ename][+-=]" ->
      List(
        Map("ename" -> "Selina", "job" -> "Driver" , "mgr" -> "Martino", "mgr->" -> "mgr=emp[ename = :mgr]{empno}",
          "car_usage[empno, car_nr][+-=]" ->
            List(
              Map("car" -> "Tesla", "car->" -> "car_nr=car[name = :car]{nr}", "date_from" -> "2021-11-15"),
            )),
        Map("ename" -> "Paul", "job" -> "Mechanic" , "mgr" -> "Selina", "mgr->" -> "mgr=emp[ename = :mgr]{empno}",
          "car_usage[empno, car_nr][+-=]" ->
            List(
              Map("car" -> "Mercedes Benz", "car->" -> "car_nr=car[name = :car]{nr}", "date_from" -> "2021-11-10"),
              Map("car" -> "FIAT", "car->" -> "car_nr=car[name = :car]{nr}", "date_from" -> "2021-12-11"),
            ))
      ),
    )
    assertResult(List(("Montana",
      List(
        ("Paul", "Mechanic", "Selina", List(("FIAT", "2021-12-11"), ("Mercedes Benz", "2021-11-10"))),
        ("Selina", "Driver", "Martino", List(("Tesla", "2021-11-15"))))))) {
      ORT.update("dept[dname]", obj)
      println(s"\nResult check:")
      tresql"""dept[dname = 'METEO']{loc, |emp e{ename, job, (emp m[m.empno = e.mgr]{m.ename}) mgr,
              |car_usage{(car[nr = car_nr]{name}) car, date_from}#(1) cars }#(1) emps}"""
        .map(d => d.loc -> d.emps.map(e => (e.ename, e.job, e.mgr,
          e.cars.map(c => c.car -> c.date_from.toString).toList)).toList).toList
    }

    obj = Map("car" -> "Tesla", "car->" -> "car_nr=car[name = :car]{nr}",
      "emp" -> "Selina", "emp->" -> "empno=emp[ename = :emp]{empno}", "date_from" -> "2022-01-01")
    assertResult(List("2022-01-01")) {
      ORT.update("car_usage[car_nr, empno]", obj)
      println(s"\nResult check:")
      tresql"car_usage[empno = (emp e[ename = 'Selina']{e.empno}) & car_nr = (car[name = 'Tesla']{nr})]{date_from}"
        .map(_.date_from.toString).toList
    }

    obj = Map("ename" -> "Selina", "hiredate" -> "2021-05-05",
      "car_usage[empno, car_nr][+-=]|null, null, exists(car[name = :car & deptnr = (emp e[empno = :#emp] {e.deptno})]{1})" -> List(
        Map("car" -> "Tesla", "car->" -> "car_nr=car[name = :car]{nr}", "date_from" -> "2000-10-05")
      ))
    assertResult(List(("2021-05-05", List(("Tesla", "2022-01-01"))))) {
      ORT.update("emp[ename]", obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Selina'] {hiredate, |car_usage{(car[nr = car_nr]{name}) car, date_from}#(1) cars}"
        .map(e => e.hiredate.toString -> e.cars.map(c => c.car -> c.date_from.toString).toList).toList
    }

    obj = Map("number" -> "000", "balance" -> 1000)
    assertResult(List(("000",1000.00, null))) {
      import OrtMetadata._
      ORT.save(
        View(
          List(SaveTo("accounts.account", Set(), List("number"))), None, null,
          List(
            Property("number", TresqlValue(":number"), false, true, true),
            Property("balance", TresqlValue(":balance"), false, true, true)
          ),
          null
        ), obj)
      println(s"\nResult check:")
      tresql"accounts.account{number, balance, empno}#(number)".map(a => (a.number, a.balance, a.empno)).toList
    }

    obj = Map("number" -> "000", "balance" -> 2000)
    assertResult(List(("000", 3000.00, null))) {
      import OrtMetadata._
      ORT.save(View(List(SaveTo("accounts.account", Set(),
        List("number"))), None, null,
        List(
          Property("number", TresqlValue(":number"), false, true, true),
          Property("balance", TresqlValue("accounts.account[number = :number]{ balance + :balance}"), false, true, true)
        ), null),
        obj)
      println(s"\nResult check:")
      tresql"accounts.account{number, balance, empno}#(number)".map(a => (a.number, a.balance, a.empno)).toList
    }

    obj = Map("ename" -> "Betty", "accounts.account[empno, number][+-=]" -> List(
      Map("number" -> "ABC123", "balance" -> 5, "accounts.transaction:beneficiary_id[+]" -> Nil),
      Map("number" -> "ABC456", "balance" -> 15, "accounts.transaction:beneficiary_id[+]" ->
        List(
          Map("originator" -> "000", "originator->" -> "originator_id=accounts.account[number = :originator]{id}",
            "amount" -> 10, "tr_date" -> "2021-11-11"),
          Map("originator" -> "ABC123", "originator->" -> "originator_id=accounts.account[number = :originator]{id}",
            "amount" -> 5, "tr_date" -> "2021-11-11"),
        )),
    ))
    assertResult( List(("Betty", List(("ABC123", 5.00, Nil),
      ("ABC456", 15.00, List((5.00, "2021-11-11", "ABC123"), (10.00, "2021-11-11", "000"))))))) {
      ORT.update("emp[ename]", obj)
      println(s"\nResult check:")
      tresql"""emp[ename = 'Betty'] {ename, |accounts.account a{number, balance,
              |[t.beneficiary_id]accounts.transaction t
                { amount, tr_date, (accounts.account a[originator_id = a.id] {number}) from_acc }#(1,2,3) tr}#(1) acc}"""
        .map(e => e.ename -> e.acc
          .map(a => (a.number, a.balance, a.tr
            .map(t => (t.amount, t.tr_date.toString, t.from_acc)).toList)).toList).toList
    }

    obj = Map("dname" -> "ADVERTISING", "emp[empno][+=]" -> List(
      Map("empno" -> null, "ename" -> "Ibo", "job" -> "clerk"),
      Map("empno" -> 10036, "ename" -> "Andy", "job" -> "clerk"),
    ))
    assertResult(List(List(("Andy", "clerk"), ("Ibo", "clerk")))) {
      import OrtMetadata._
      ORT.update(View(List(SaveTo("dept",Set(),List("dname"))),None,null,
        List(
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("emp[empno][+=]", ViewValue(View(List(SaveTo("emp", Set(),List("empno"))),None,null,
            List(
              Property("empno", KeyValue(":empno", TresqlValue("#emp"), Some(TresqlValue(":empno"))), false,true,true),
              Property("ename",TresqlValue(":ename"), false,true,true),
              Property("job",TresqlValue(":job"),false,true,true)),null
          ),
        SaveOptions(true,true,false)), false,true,true)), null), obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'ADVERTISING'] { |emp {ename, job}#(ename) e}".map(_.e.map(e => e.ename -> e.job).toList).toList
    }

    obj = Map("dname" -> "ADVERTISING", "emp[deptno, empno][+-=]" -> List(
      Map("empno" -> null, "ename" -> "Zizo", "job" -> "arborist"),
      Map("empno" -> 10036, "ename" -> "Andy", "job" -> "arborist"),
    ))
    assertResult(List(List(("Andy", "arborist"), ("Zizo", "arborist")))) {
      import OrtMetadata._
      ORT.update(View(List(SaveTo("dept", Set(), List("dname"))), None, null,
        List(
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("emp[deptno, empno][+-=]",
            ViewValue(View(List(SaveTo("emp", Set(), List("deptno", "empno"))), None, null,
              List(
                Property("empno", KeyValue(":empno", AutoValue(":empno"), Some(AutoValue(":empno"))), false, true, true),
                Property("ename", TresqlValue(":ename"), false, true, true),
                Property("job", TresqlValue(":job"), false, true, true)), null
            ),
              SaveOptions(true, true, true)), false, true, true)), null), obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'ADVERTISING'] { |emp {ename, job}#(ename) e}".map(_.e.map(e => e.ename -> e.job).toList).toList
    }

    obj = Map("dname" -> "ADVERTISING", "emp[deptno, empno][+-=]" -> List(
      Map("empno" -> null, "ename" -> "Solomon", "job" -> "operator"),
      Map("empno" -> 10036, "ename" -> "Andy", "job" -> "operator"),
    ))
    assertResult(List(List(("Andy", "operator"), ("Solomon", "operator")))) {
      import OrtMetadata._
      ORT.update(View(List(SaveTo("dept", Set(), List("dname"))), None, null,
        List(
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("emp[deptno, empno][+-=]",
            ViewValue(View(List(SaveTo("emp", Set(), List("deptno", "empno"))), None, null,
              List(
                Property("empno", KeyValue(":empno", TresqlValue("#emp"), Some(TresqlValue(":empno"))), false, true, true),
                Property("ename", TresqlValue(":ename"), false, true, true),
                Property("job", TresqlValue(":job"), false, true, true)), null
            ),
            SaveOptions(true, true, true)), false, true, true)), null), obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'ADVERTISING'] { |emp {ename, job}#(ename) e}".map(_.e.map(e => e.ename -> e.job).toList).toList
    }

    obj = Map("dname" -> "Security", "loc" -> "Warsaw", "emp[deptno, ename]" -> List(
      Map("ename" -> "Carol", "job" -> "Analyst"),
      Map("ename" -> "Marta", "job" -> "Tester"),
    ))
    assertResult(List(("Security", List(("Carol", "Analyst"), ("Marta", "Tester"))))) {
      val md = OrtMetadata.ortMetadata("dept[dname]", obj)._1
      resources log s"$md"
      ORT.save(md, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Security'] { dname, |emp {ename, job}#(ename) e}"
        .map(d => d.dname -> d.e.map(e => e.ename -> e.job).toList).toList
    }

    obj = Map("dname" -> "Security", "loc" -> "Warsaw", "emp[ename]" -> List(
      Map("ename" -> "Pawel", "job" -> "Analyst"),
      Map("ename" -> "Marta", "job" -> "Tester"),
    ))
    assertResult(List(("Security", List(("Marta", "Tester"), ("Pawel", "Analyst"))))) {
      val md = OrtMetadata.ortMetadata("dept[dname]", obj)._1
      resources log s"$md"
      ORT.save(md, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Security'] { dname, |emp {ename, job}#(ename) e}"
        .map(d => d.dname -> d.e.map(e => e.ename -> e.job).toList).toList
    }

    obj = Map("id" -> 0, "name" -> "Zero")
    assertResult(0) {
      import OrtMetadata._
      ORT.insert(View(
        List(SaveTo("dept",Set(),List())), None, null, List(
          Property("deptno", KeyValue(":id", TresqlValue(":id"), None), false,true,false),
      Property("dname", TresqlValue(":name"), false,true,true)
      ),null), obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Zero']{deptno}".head[Long]
    }

    println("------ KEY UPDATE test ------")

    obj = Map("number" -> "000", "new_number" -> "000000")
    assertResult(List("000000")) {
      import OrtMetadata._
      ORT.save(View(List(SaveTo("accounts.account", Set(),
        List("number"))), None, null,
        List(Property("number", KeyValue(":number", TresqlValue(":new_number")), false, true, true)), null),
        obj)
      println(s"\nResult check:")
      tresql"accounts.account[number = '000000']{number}".map(_.number).toList
    }

    obj = Map("number" -> "111", "new_number" -> "111111", "balance" -> 100)
    assertResult(List("111111")) {
      import OrtMetadata._
      ORT.save(
        View(
          List(SaveTo("accounts.account", Set(), List("number"))), None, null,
          List(
            Property("number", KeyValue(":number", TresqlValue(":new_number")), false, true, true),
            Property("balance", TresqlValue(":balance"), false, true, true)
          ), null),
        obj
      )
      println(s"\nResult check:")
      tresql"accounts.account[number = '111111']{number, balance}".map(_.number).toList
    }

    obj = Map("number" -> "111111", "new_number" -> "222222", "balance" -> 200)
    assertResult(List(("111111", 200))) {
      import OrtMetadata._
      ORT.save(
        View(
          List(SaveTo("accounts.account", Set(), List("number"))), None, null,
          List(
            Property("number", KeyValue(":number", TresqlValue(":new_number")), false, true, false),
            Property("balance", TresqlValue(":balance"), false, true, true)
          ), null),
        obj
      )
      println(s"\nResult check:")
      tresql"accounts.account[number = '111111']{number, balance}".map(r => r.number -> r.balance).toList
    }

    obj = Map("ename" -> "Betty", "new_ename" -> "Binny", "dept" -> "Temp", "new_dept" -> "Radio")
    assertResult( List(("Binny", "Radio"))) {
      import OrtMetadata._
      ORT.save(
        View(
          List(SaveTo("emp", Set(), List("ename", "deptno"))), None, null,
          List(
            Property("ename", KeyValue(":ename", TresqlValue(":new_ename")), false, true, true),
            Property("deptno", KeyValue("(dept[dname = :dept]{deptno})",
              TresqlValue("(dept[dname = :new_dept]{deptno})")), false, true, true)
          ), null),
        obj
      )
      println(s"\nResult check:")
      tresql"emp/dept[ename = 'Binny']{ename, dname}".map(e => e.ename -> e.dname).toList
    }
    // key is composite primary key
    obj = Map("car_nr" -> tresql"car[name = 'Mercedes Benz']{nr}".head[String],
      "empno" -> tresql"emp[ename = 'Lara']{empno}".head[Long])
    assertResult(List(("Lara", "Dodge"), ("Lara", "Mercedes Benz"))) {
      import OrtMetadata._
      ORT.insert(
        View(
          List(SaveTo("car_usage", Set(), List("car_nr", "empno"))), None, null,
          List(
            Property("car_nr", KeyValue("", TresqlValue(":car_nr")), false, true, true),
            Property("empno", KeyValue("", TresqlValue(":empno")), false, true, true)
          ),
          null
        ), obj)
      println(s"\nResult check:")
      tresql"car_usage/car;car_usage/emp[ename = 'Lara']{ename, name}#(1, 2)".map(r => (r.ename, r.name)).toList
    }

    obj = Map("old_car_nr" -> tresql"car[name = 'FIAT']{nr}".head[String],
      "car_nr" -> tresql"car[name = 'Tesla']{nr}".head[String],
      "empno" -> tresql"emp[ename = 'Paul']{empno}".head[Long])
    assertResult(List(("Paul", "Mercedes Benz"), ("Paul", "Tesla"))) {
      import OrtMetadata._
      ORT.save(
        View(
          List(SaveTo("car_usage", Set(), List("car_nr", "empno"))), None, null,
          List(
            Property("car_nr", KeyValue(":old_car_nr", TresqlValue(":car_nr")), false, true, true),
            Property("empno", KeyValue(":empno", TresqlValue(":empno")), false, true, true)
          ),
          null
        ), obj)
      println(s"\nResult check:")
      tresql"car_usage/car;car_usage/emp[ename = 'Paul']{ename, name}#(1, 2)".map(r => (r.ename, r.name)).toList
    }

    obj = Map("nr"-> "C1", "car_name" -> "Citroen", "usage" -> List(
        Map("empno" -> tresql"emp[ename = 'Lara']{empno}".head[Long]),
        Map("empno" -> tresql"emp[ename = 'Paul']{empno}".head[Long])
      )
    )
    def car_with_usage_view = {
      import OrtMetadata._
      View(
        List(SaveTo("car", Set(), List("nr"))), None, null,
        List(
          Property("name", TresqlValue(":car_name"), false, true, true),
          Property("usage", ViewValue(
            View(
              List(SaveTo("car_usage", Set(), List("car_nr", "empno"))), None, null,
              List(
                Property("empno",
                  KeyValue(":old_empno", TresqlValue(":empno")),
                  false, true, true
                )
              ),
              null
            ),
            SaveOptions(doInsert = true, doDelete = true, doUpdate = true)),
            false, true, true
          )
        ),
        null
      )
    }
    assertResult(List(("Lara", "Citroen"), ("Paul", "Citroen"))) {
      ORT.insert(car_with_usage_view, obj)
      println(s"\nResult check:")
      tresql"car_usage/car;car_usage/emp[nr = 'C1']{ename, name}#(1, 2)".map(r => (r.ename, r.name)).toList
    }

    obj = Map("nr" -> "C1", "car_name" -> "Citroen", "usage" -> List(
      Map(
        "empno" -> tresql"emp[ename = 'Nicky']{empno}".head[Long],
        "old_empno" -> tresql"emp[ename = 'Lara']{empno}".head[Long]
      ),
      Map(
        "empno" -> tresql"emp[ename = 'SCOTT']{empno}".head[Long],
        "old_empno" -> tresql"emp[ename = 'SCOTT']{empno}".head[Long]

      )
    ))
    assertResult(List(("Nicky", "Citroen"), ("SCOTT", "Citroen"))) {
      ORT.save(car_with_usage_view, obj)
      println(s"\nResult check:")
      tresql"car_usage/car;car_usage/emp[nr = 'C1']{ename, name}#(1, 2)".map(r => (r.ename, r.name)).toList
    }

    // children composite primary key
    obj = {
      def nr(name: String) = tresql"car[name = $name]{nr}".head[String]
      Map("empno" -> tresql"emp[ename = 'SCOTT']{empno}".head[Long], "car_usage[+-=]" ->
        List(
          Map("car_nr" -> nr("BMW")),
          Map("car_nr" -> nr("Citroen")),
          Map("car_nr" -> nr("Tesla")),
        )
      )
    }
    assertResult(List("BMW", "Citroen", "Tesla")) {
      ORT.update("emp", obj)
      println(s"\nResult check:")
      tresql"car_usage/car;car_usage/emp[ename = 'SCOTT']{name}#(1)".map(_.name).toList
    }

    obj = {
      def nr(name: String) = tresql"car[name = $name]{nr}".head[String]
      Map("empno" -> tresql"emp[ename = 'SCOTT']{empno}".head[Long], "car_usage[car_nr][+-=]" ->
        List(
          Map("car_nr" -> nr("VW")),
          Map("car_nr" -> nr("Citroen")),
          Map("car_nr" -> nr("Tesla")),
        )
      )
    }
    assertResult(List("Citroen", "Tesla", "VW")) {
      ORT.update("emp", obj)
      println(s"\nResult check:")
      tresql"car_usage/car;car_usage/emp[ename = 'SCOTT']{name}#(1)".map(_.name).toList
    }
  }

  private def ortOnExtraDatabase(implicit resources: Resources) = {
    println("------ ORT on extra database --------")

    var obj = Map("name" -> "Dzidzis", "sex" -> "M", "birth_date" -> "1999-04-06", "email" -> "dzidzis@albatros.io",
      "@contact_db:notes[note_date, note][+-=]" ->
        List(
          Map("note_date" -> new java.sql.Timestamp(System.currentTimeMillis), "note" -> "Mene, tekel, ufarsin")
        )
    )
    assertResult(List(("M", "dzidzis@albatros.io", List("Mene, tekel, ufarsin")))) {
      ORT.insert("@contact_db:contact[name]", obj)
      println(s"\nResult check:")
      tresql"|contact_db:contact[name = 'Dzidzis']{sex, email, |contact_db:notes{note}#(1) notes}"
        .map(c => (c.sex, c.email, c.notes.map(_.note).toList)).toList
    }

    obj = Map("name" -> "Dzidzis", "sex" -> "M", "birth_date" -> "2000-04-06", "email" -> "dzidzis@albatros.io",
      "@contact_db:notes[note][+-=]" ->
        List(
          Map("note_date" -> new java.sql.Timestamp(System.currentTimeMillis), "note" -> "Mene, tekel, ufarsin"),
          Map("note_date" -> new java.sql.Timestamp(System.currentTimeMillis), "note" -> "Cicerons")
        )
    )
    assertResult(List(("M", "2000-04-06", List("Cicerons", "Mene, tekel, ufarsin")))) {
      ORT.update("@contact_db:contact[name]", obj)
      println(s"\nResult check:")
      tresql"|contact_db:contact[name = 'Dzidzis']{sex, birth_date, |contact_db:notes{note}#(1) notes}"
        .map(c => (c.sex, c.birth_date.toString, c.notes.map(_.note).toList)).toList
    }

    obj = Map("name" -> "Dzidzis", "@contact_db:notes[note]" ->
      List(
        Map("note_date" -> new java.sql.Timestamp(System.currentTimeMillis), "note" -> "Cicerons")
      )
    )
    assertResult(List(("M", "2000-04-06", List("Cicerons")))) {
      ORT.update("@contact_db:contact[name]", obj)
      println(s"\nResult check:")
      tresql"|contact_db:public.contact c[c.name = 'Dzidzis']{c.sex, c.birth_date, |contact_db:public.notes n{n.note}#(1) notes}"
        .map(c => (c.sex, c.birth_date.toString, c.notes.map(_.note).toList)).toList
    }

    assertResult(Nil) {
      ORT.delete("@contact_db:notes", List("contact_id", "note"),
        Map("note" -> "Cicerons", "contact_id" -> Query("|contact_db:contact[name = 'Dzidzis']{id}").unique[Long]),
        null
      )
      println(s"\nResult check:")
      tresql"|contact_db:notes[contact_id = (contact[name = 'Dzidzis']{id})]".toList
    }

    assertResult(Nil) {
      ORT.delete("@contact_db:contact", Query("|contact_db:contact[name = 'Dzidzis']{id}").unique[Long])
      println(s"\nResult check:")
      tresql"|contact_db:contact[name = 'Dzidzis']".toList
    }
  }

  private def ortForInsertForUpdateOptionalFlags(implicit resources: Resources) = {
    println("------ ORT forInsert, forUpdate flags --------")

    var view = {
      import OrtMetadata._
      View(
        List(SaveTo("dept", Set(), Nil)), None, null,
        List(
          Property("dname", TresqlValue(":dname"), false, true, false),
          Property("loc", TresqlValue(":loc"), false, false, true)
        ), null)
    }
    var obj: Map[String, Any] = Map("dname" -> "Cafe", "loc" -> "Purvciems")
    assertResult(List(("Cafe", null))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Cafe']{dname, loc}".map(d => d.dname -> d.loc).toList
    }
    obj = Map("deptno" -> tresql"dept[dname = 'Cafe']{deptno}".unique[Long], "dname" -> "Cafez", "loc" -> "Purvciems")
    assertResult(List(("Cafe", "Purvciems"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Cafe']{dname, loc}".map(d => d.dname -> d.loc).toList
    }
    view = view.copy(saveTo = List(SaveTo("dept", Set(), key = List("dname"))))
    obj = Map("dname" -> "Pizza", "loc" -> "Imanta")
    assertResult(List(("Pizza", null))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Pizza']{dname, loc}".map(d => d.dname -> d.loc).toList
    }
    assertResult(List(("Pizza", "Imanta"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Pizza']{dname, loc}".map(d => d.dname -> d.loc).toList
    }

    println("-------- ORT view forInsert, forUpdate test --------")
    view = {
      import OrtMetadata._
      View(
        List(SaveTo("dept", Set(), Nil)), None, null,
        List(
          Property("deptno", TresqlValue(":deptno"), false, true, true),
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("loc", TresqlValue(":loc"), false, true, true)
        ), null)
    }
    obj = Map("dname" -> "Space", "loc" -> "Mars")
    assertResult(List(("Space", "Mars"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Space']{dname, loc}".map(d => (d.dname, d.loc)).toList
    }
    var childView = {
      import OrtMetadata._
      View(
        List(SaveTo("emp", Set(), Nil)), None, null,
        List(
          Property("empno", TresqlValue(":empno"), false, true, true),
          Property("ename", TresqlValue(":ename"), false, true, true),
        ), null)
    }
    view = {
      import OrtMetadata._
      view.copy(properties = view.properties :+
        Property("emps", ViewValue(childView, SaveOptions(true, true, true)), false, true, true))
    }
    obj = Map("deptno" -> tresql"dept[dname = 'Space']{deptno}".unique[Long], "dname" -> "Space", "loc" -> "Venus",
      "emps" -> List(Map("ename" -> "Boris")))
    assertResult(List(("Space", "Venus" ,List("Boris")))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Space']{dname, loc, |emp{ename} emps}"
        .map(d => (d.dname, d.loc, d.emps.map(_.ename).toList)).toList
    }

    view = {
      import OrtMetadata._
      view.copy(properties = view.properties.dropRight(1) :+
        Property("emps", ViewValue(childView, SaveOptions(true, true, true)), false, true, false))
    }
    obj = Map("deptno" -> tresql"dept[dname = 'Space']{deptno}".unique[Long], "dname" -> "Space", "loc" -> "Venus",
      "emps" -> List(Map("ename" -> "Zina")))
    assertResult(List(("Space", "Venus", List("Boris")))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Space']{dname, loc, |emp{ename} emps}"
        .map(d => (d.dname, d.loc, d.emps.map(_.ename).toList)).toList
    }

    // null children - deletes all children
    view = {
      import OrtMetadata._
      view.copy(properties = view.properties.dropRight(1) :+
        Property("emps", ViewValue(childView, SaveOptions(true, true, true)), false, true, true))
    }
    obj = Map("deptno" -> tresql"dept[dname = 'Space']{deptno}".unique[Long], "dname" -> "Space", "loc" -> "Saturn",
      "emps" -> null)
    assertResult(List(("Space", "Saturn", Nil))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Space']{dname, loc, |emp{ename} emps}"
        .map(d => (d.dname, d.loc, d.emps.map(_.ename).toList)).toList
    }

    view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), Nil)), None, null,
          List(
            Property("deptno", TresqlValue(":deptno"), false, true, true),
            Property("dname", TresqlValue(":dname"), false, true, true),
            Property("loc", TresqlValue(":loc"), false, true, true)
          ), null)
      View(
        List(SaveTo("emp", Set(), Nil)), None, null,
        List(
          Property("empno", TresqlValue(":empno"), false, true, true),
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("deptno", LookupViewValue("dept", lookupView), false, false, true)
        ), null)
    }
    obj = Map("ename" -> "Gogi", "dept" -> Map("dname" -> "Mount", "loc" -> "Georgia"))
    intercept[Exception] { // should not save because lookup value - dept cannot be inserted
      ORT.save(view, obj)
    }

    // optional view flag
    view = {
      import OrtMetadata._
      val cars =
        View(
          List(SaveTo("car", Set(), List("name"))), None, null,
          List(
            Property("name", TresqlValue(":name"), false, true, true),
            Property("is_active", TresqlValue(":is_active?"), true, true, true),
            Property("tyres_nr", TresqlValue(":tyres_nr?"), true, true, true),
          ), null)
      View(
        List(SaveTo("dept", Set(), List("dname"))), None, null,
        List(
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("loc", TresqlValue(":loc?"), true, true, true),
          Property("cars", ViewValue(cars, SaveOptions(true, true, true)), true, true, true)
        ), null)
    }
    obj = Map("dname" -> "Floating", "loc" -> "Sea", /*"cars" -> List(
      Map("name" -> "Boat", "is_active" -> true),
      Map("name" -> "Ferry", "is_active" -> true),
    )*/)
    assertResult(List(("Sea", List()))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Floating']{loc, |car{name, is_active} cars}"
        .map(d => (d.loc, d.cars.map(c => c.name -> c.is_active).toList)).toList
    }
    obj = Map("dname" -> "Floating", "cars" -> List(
      Map("name" -> "Ship"),
      Map("name" -> "Ferry", "is_active" -> false),
    ))
    assertResult( List(("Sea", List(("Ship", null), ("Ferry", false))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Floating']{loc, |car{name, is_active} cars}"
        .map(d => (d.loc, d.cars.map(c => c.name -> c.is_active).toList)).toList
    }
    obj = Map("dname" -> "Floating", "loc" -> "Lake")
    assertResult(List(("Lake", List(("Ship", null), ("Ferry", false))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Floating']{loc, |car{name, is_active} cars}"
        .map(d => (d.loc, d.cars.map(c => c.name -> c.is_active).toList)).toList
    }
    obj = Map("dname" -> "Floating", "cars" -> List(
      Map("name" -> "Ship", "is_active" -> true),
      Map("name" -> "Ferry", "tyres_nr" -> null),
    ))
    assertResult(List(("Lake", List(("Ship", true), ("Ferry", false))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Floating']{loc, |car{name, is_active} cars}"
        .map(d => (d.loc, d.cars.map(c => c.name -> c.is_active).toList)).toList
    }
  }

  private def ortPkName_ne_BindVarName(implicit resources: Resources) = {
    println("-------- ORT table pk name differs from corresponding bind variable name --------")
    var view = {
      import OrtMetadata._
      View(
        List(SaveTo("dept", Set(), Nil)), None, null,
        List(
          Property("deptno", TresqlValue(":dept_id"), false, true, true),
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("loc", TresqlValue(":loc"), false, true, true)
        ), null)
    }
    var obj: Map[String, Any] = Map("dept_id" -> 4321, "dname" -> "Fish", "loc" -> "Vecaki")
    assertResult(List((4321, "Fish", "Vecaki"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Fish']{deptno, dname, loc}".map(d => (d.deptno, d.dname, d.loc)).toList
    }
    obj = Map("dept_id" -> 4321, "dname" -> "Fish", "loc" -> "Gauja")
    assertResult(List((4321, "Fish", "Gauja"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Fish']{deptno, dname, loc}".map(d => (d.deptno, d.dname, d.loc)).toList
    }
    var childView = {
      import OrtMetadata._
      View(
        List(SaveTo("emp", Set(), Nil)), None, null,
        List(
          Property("empno", TresqlValue(":emp_id"), false, true, true),
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("job", TresqlValue(":job"), false, true, true),
        ), null)
    }
    view = {
      import OrtMetadata._
      view.copy(properties = view.properties :+
        Property("emps", ViewValue(childView, SaveOptions(true, true, false)), false, true, true))
    }
    obj = Map("dept_id" -> 4321, "dname" -> "Fish", "loc" -> "Roja", "emps" ->
      List(Map("ename" -> "Girts", "job" -> "Fisherman")))
    assertResult(List(("Fish", "Roja", List(("Girts", "Fisherman"))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Fish']{dname, loc, |emp{ename, job}#(1) emps}"
        .map(d => (d.dname, d.loc, d.emps.map(e => (e.ename, e.job)).toList)).toList
    }
    obj = Map("dept_id" -> 4321, "dname" -> "Fish", "loc" -> "Roja", "emps" ->
      List(Map("emp_id" -> Query("emp[ename = 'Girts']{empno}").unique[Any], "ename" -> "Gatis", "job" -> "Fisher")))
    assertResult(List(("Fish", "Roja", List(("Gatis", "Fisher"))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept[dname = 'Fish']{dname, loc, |emp{ename, job}#(1) emps}"
        .map(d => (d.dname, d.loc, d.emps.map(e => (e.ename, e.job)).toList)).toList
    }
  }

  private def ortSaveToMultipleTablesWithRepeatingColName(implicit resources: Resources) = {
    println(s"-------- ORT save to multiple tables with repeating column name --------")
    var view = {
      import OrtMetadata._
      View(
        List(
          SaveTo("dept", Set(), List("dname")),
          SaveTo("dept_addr", Set("deptnr"), Nil),
          SaveTo("dept_sub_addr", Set("deptno"), Nil)
        ),
        None, null,
        List(
          Property("dname", TresqlValue(":dname"), false, true, true),
          Property("loc", TresqlValue(":loc?"), true, true, true),
          Property("dept_addr.addr", TresqlValue(":addr"), false, true, true),
          Property("dept_sub_addr.addr", TresqlValue(":sub_addr"), false, true, true),
          Property("zip_code", TresqlValue(":zip_code"), false, true, true),
        ), null)
    }
    var obj = Map(
      "dname" -> "Garden",
      "addr" -> "Vatican",
      "sub_addr" -> "Museum",
      "zip_code" -> "VA-1",
    )
    assertResult(List(("Garden", List(("Vatican", List("Museum")))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept [dname = 'Garden'] { dname, |dept_addr{addr, |dept_sub_addr{addr} dsa} da}"
        .map(d => (d.dname, d.da.map(da => (da.addr, da.dsa.map(_.addr).toList)).toList)).toList
    }
    obj = Map(
      "dname" -> "Garden",
      "loc" -> "Vatican",
      "addr" -> "Villa Marta",
      "sub_addr" -> "1",
      "zip_code" -> "VA-10",
    )
    assertResult(List(("Garden", "Vatican", List(("Villa Marta", "VA-10", List("1")))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"dept [dname = 'Garden'] { dname, loc, |dept_addr{addr, zip_code, |dept_sub_addr{addr} dsa} da }"
        .map(d => (d.dname, d.loc, d.da.map(da => (da.addr, da.zip_code, da.dsa.map(_.addr).toList)).toList)).toList
    }
  }
  private def ortLookupByKey(implicit resources: Resources) = {
    println("-------- ORT LOOKUP edit by key test --------")

    var obj: Map[String, Any] = Map("ename" -> "Gogi", "dept" -> Map("dname" -> "Mount", "loc" -> "Georgia"))
    var view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), List("dname"))), None, null,
          List(
            Property("dname", TresqlValue(":dname"), false, true, true),
            Property("loc", TresqlValue(":loc"), false, true, true)
          ), null)
      View(
        List(SaveTo("emp", Set(), List("ename"))), None, null,
        List(
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("deptno", LookupViewValue("dept", lookupView), false, true, true)
        ), null)
    }
    assertResult(List(("Gogi", "Mount"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Gogi']{ename, (dept d[d.deptno = emp.deptno]{dname}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }

    obj = Map("ename" -> "Gogi", "dept" -> Map("dname" -> "Mount", "loc" -> "Tbilisi"))
    assertResult(List(("Gogi", "Tbilisi"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Gogi']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }

    println("-------- ORT LOOKUP ref update, empty LOOKUP update test --------")

    view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), List("dname"))), None, null,
          List(
            Property("dname", TresqlValue(":dname"), false, true, true),
          ), null)
      View(
        List(SaveTo("emp", Set(), List("ename"))), None, null,
        List(
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("deptno", LookupViewValue("dept", lookupView), false, true, true)
        ), null)
    }
    obj = Map("ename" -> "Leo", "dept" -> Map("dname" -> "Space"))
    assertResult(List(("Leo", "Saturn"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Leo']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }
    obj = Map("ename" -> "Leo", "dept" -> Map("dname" -> "Mount"))
    assertResult(List(("Leo", "Tbilisi"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Leo']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }
    obj = Map("ename" -> "Fidel", "dept" -> Map("dname" -> "Havana"))
    assertResult(List(("Fidel", "Havana"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Fidel']{ename, (dept d[d.deptno = emp.deptno]{dname}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }
    obj = Map("ename" -> "Leo", "dept" -> Map("dname" -> "Malbec"))
    assertResult(List(("Leo", "Malbec"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Leo']{ename, (dept d[d.deptno = emp.deptno]{dname}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }

    view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), List("dname"))), None, null,
          List(
            Property("dname", TresqlValue(":dname"), false, true, true),
            Property("loc", TresqlValue(":loc"), false, true, true),
          ), null)
      View(
        List(SaveTo("car", Set(), List("name"))), None, null,
        List(
          Property("name", TresqlValue(":name"), false, true, true),
          Property("deptnr", LookupViewValue("dept", lookupView), false, true, true)
        ), null)
    }
    obj = Map("name" -> "ZAZ", "dept" -> null)
    assertResult(List(("ZAZ", null))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'ZAZ']{name, (dept d[d.deptno = car.deptnr]{dname}) dept}"
        .map(c => (c.name, c.dept)).toList
    }
    obj = Map("name" -> "ZAZ", "dept" -> Map("dname" -> "Malbec", "loc" -> "Argentina"))
    assertResult(List(("ZAZ", "Malbec", "Argentina"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'ZAZ']{name, (dept d[d.deptno = car.deptnr]{dname}) dept, (dept d[d.deptno = car.deptnr]{loc}) loc}"
        .map(c => (c.name, c.dept, c.loc)).toList
    }
    //nullify deptnr for car
    obj = Map("name" -> "ZAZ", "dept" -> null)
    assertResult(List(("ZAZ", null, null))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'ZAZ']{name, (dept d[d.deptno = car.deptnr]{dname}) dept, (dept d[d.deptno = car.deptnr]{loc}) loc}"
        .map(c => (c.name, c.dept, c.loc)).toList
    }

    view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), Nil)), None, null,
          List(
            Property("deptno", TresqlValue(":deptno"), false, true, true),
          ), null)
      View(
        List(SaveTo("emp", Set(), List("ename"))), None, null,
        List(
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("deptno", LookupViewValue("dept", lookupView), false, true, true)
        ), null)
    }
    obj = Map("ename" -> "Christina", "dept" -> Map("deptno" -> tresql"dept[dname = 'Space']{deptno}".unique[Long]))
    assertResult(List(("Christina", "Saturn"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Christina']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }
    obj = Map("ename" -> "Christina", "dept" -> Map("deptno" -> tresql"dept[dname = 'Mount']{deptno}".unique[Long]))
    assertResult(List(("Christina", "Tbilisi"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Christina']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }

    // lookup obj key equals table primary key
    view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), List("deptno"))), None, null,
          List(
            Property("deptno", TresqlValue(":deptno"), false, true, true),
          ), null)
      View(
        List(SaveTo("emp", Set(), List("ename"))), None, null,
        List(
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("deptno", LookupViewValue("dept", lookupView), false, true, true)
        ), null)
    }
    obj = Map("ename" -> "Britney", "dept" -> Map("deptno" -> tresql"dept[dname = 'Space']{deptno}".unique[Long]))
    assertResult(List(("Britney", "Saturn"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Britney']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }

    view = {
      import OrtMetadata._
      val lookupView =
        View(
          List(SaveTo("dept", Set(), List("deptno"))), None, null,
          List(
            Property("deptno", KeyValue(":deptno", AutoValue(":deptno"), Some(AutoValue(":deptno"))), false, true, true),
          ), null)
      View(
        List(SaveTo("emp", Set(), List("ename"))), None, null,
        List(
          Property("ename", TresqlValue(":ename"), false, true, true),
          Property("deptno", LookupViewValue("dept", lookupView), false, true, true)
        ), null)
    }
    obj = Map("ename" -> "Britney", "dept" -> Map("deptno" -> tresql"dept[dname = 'Mount']{deptno}".unique[Long]))
    assertResult(List(("Britney", "Tbilisi"))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"emp[ename = 'Britney']{ename, (dept d[d.deptno = emp.deptno]{loc}) dept}"
        .map(e => (e.ename, e.dept)).toList
    }

    //optional lookup view
    view = {
      import OrtMetadata._
      val lookupDept =
        View(
          List(SaveTo("dept", Set(), List("deptno"))), None, null,
          List(
            Property("deptno", TresqlValue(":deptno"), false, true, true),
            Property("dname", TresqlValue(":dname"), false, true, true),
            Property("loc", TresqlValue(":loc?"), true, true, true),
          ), null)
      val lookupTyres =
        View(
          List(SaveTo("tyres", Set(), Nil)), None, null,
          List(
            Property("nr", TresqlValue(":nr"), false, true, true),
            Property("carnr", TresqlValue(":carnr"), false, true, true),
            Property("brand", TresqlValue(":brand"), false, true, true),
            Property("season", TresqlValue(":season"), false, true, true),
          ), null)
      View(
        List(SaveTo("car", Set(), List("name"))), None, null,
        List(
          Property("name", TresqlValue(":name"), false, true, true),
          Property("deptnr", LookupViewValue("dept", lookupDept), true, true, true),
          Property("tyres_nr", LookupViewValue("tyres", lookupTyres), true, true, true),
        ), null)
    }

    obj = Map(
      "name" -> "BMW",
      "tyres" ->
        Map(
          "nr" -> null,
          "carnr" -> Query("car[name = 'BMW']{nr}").unique[Long],
          "brand" -> "Barum", "season" -> "W"
        )
    )
    assertResult(List(("BMW", "RESEARCH", List(("Barum", "W"))))) {
      ORT.update(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'BMW'] {name, (dept[deptno = deptnr]{dname}) dept, |[car.tyres_nr = nr]tyres {brand, season} tyres}"
        .map(c => (c.name, c.dept, c.tyres.map(t => t.brand -> t.season).toList)).toList
    }

    obj = Map(
      "name" -> "BMW",
      "tyres" ->
        Map(
          "nr" -> Query("car[name = 'BMW']{tyres_nr}").unique[Long],
          "carnr" -> Query("car[name = 'BMW']{nr}").unique[Long], "brand" -> "Barum", "season" -> "S"
        )
    )
    assertResult(List(("BMW", "RESEARCH", List(("Barum", "S"))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'BMW'] {name, (dept[deptno = deptnr]{dname}) dept, |[car.tyres_nr = nr]tyres {brand, season} tyres}"
        .map(c => (c.name, c.dept, c.tyres.map(t => t.brand -> t.season).toList)).toList
    }
    obj = Map(
      "name" -> "BMW",
      "dept" ->
        Map(
          "deptno" -> Query("car[name = 'BMW']{deptnr}").unique[Long],
          "dname" -> Query("dept[deptno = (car[name = 'BMW']{deptnr})]{dname}").unique[String]
        )
    )
    assertResult(List(("BMW", "DALLAS", List(("Barum", "S"))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'BMW'] {name, (dept[deptno = deptnr]{loc}) dept_loc, |[car.tyres_nr = nr]tyres {brand, season} tyres}"
        .map(c => (c.name, c.dept_loc, c.tyres.map(t => t.brand -> t.season).toList)).toList
    }

    obj = Map(
      "name" -> "BMW",
      "dept" ->
        Map(
          "deptno" -> Query("car[name = 'BMW']{deptnr}").unique[Long],
          "dname" -> Query("dept[deptno = (car[name = 'BMW']{deptnr})]{dname}").unique[String],
          "loc" -> "El paso",
        )
    )
    assertResult(List(("BMW", "El paso", List(("Barum", "S"))))) {
      ORT.save(view, obj)
      println(s"\nResult check:")
      tresql"car[name = 'BMW'] {name, (dept[deptno = deptnr]{loc}) dept_loc, |[car.tyres_nr = nr]tyres {brand, season} tyres}"
        .map(c => (c.name, c.dept_loc, c.tyres.map(t => t.brand -> t.season).toList)).toList
    }

    view = {
      import OrtMetadata._
      val lookupDept =
        View(
          List(SaveTo("dept", Set(), List("dname"))), None, null,
          List(
            Property("dname", TresqlValue(":dname"), false, true, true)
          ), null)
      View(
        List(SaveTo("emp", Set(), List())), None, null,
        List(
          Property("empno", TresqlValue(":empno"), false, true, true),
          Property("ename", TresqlValue(":ename?"), true, true, true),
          Property("job", TresqlValue(":job?"), true, true, true),
          Property("deptno", LookupViewValue("dept", lookupDept), true, true, true),
        ), null)
    }

    obj = Map("empno" -> Query("emp[ename = 'Leo']{empno}").unique[Long], "ename" -> "Leopold",
      "dept" -> Map("dname" -> "Pharmacy"))

    assertResult(List("Pharmacy")) {
      ORT.update(view, obj)
      println("\nResult check:")
      tresql"emp[ename = 'Leopold']/dept{dname}".map(_.dname).toList
    }

    view = {
      import OrtMetadata._
      val lookupDept =
        View(
          List(SaveTo("dept", Set(), List("dname"))), None, null,
          List(
            Property("dname", KeyValue(":dname", TresqlValue(":dname"), None), false, true, true),
            Property("loc", TresqlValue(":loc"), false, true, true),
          ), null)
      View(
        List(SaveTo("emp", Set(), List())), None, null,
        List(
          Property("empno", TresqlValue(":empno"), false, true, true),
          Property("ename", TresqlValue(":ename?"), true, true, true),
          Property("job", TresqlValue(":job?"), true, true, true),
          Property("deptno", LookupViewValue("dept", lookupDept), true, true, true),
        ), null)
    }

    obj = Map("empno" -> Query("emp[ename = 'Leopold']{empno}").unique[Long], "job" -> "pharmac",
      "dept" -> Map("dname" -> "Pharmacy", "loc" -> "Brunenieku st"))

    assertResult(List(("pharmac", "Pharmacy", "Brunenieku st"))) {
      ORT.update(view, obj)
      println("\nResult check:")
      tresql"emp[ename = 'Leopold']/dept{job, dname, loc}".map(r => (r.job, r.dname, r.loc)).toList
    }
  }

  override def compilerMacro(implicit resources: Resources) = {
      println("\n-------------- TEST compiler macro ----------------\n")

      // for scala 2/3 compatibility (Unit != EmptyTuple) convert result to string
      assertResult("()")(tresql"".toString)

      assertResult(("ACCOUNTING",
      List(("CLARK",List()), ("KING",List(3, 4)), ("Lara",List()), ("Nicky",List()))))(
          tresql"dept{dname, |emp {ename, |[empno]work {hours}#(1)}#(1)}#(1)"
           .map(d => d.dname -> d._2
              .map(e => e.ename -> e._2
                .map(w => w.hours).toList).toList).toList.head)

      assertResult("A B")(tresql"{concat('A', ' ', 'B')}".head._1)

      assertResult(15)(tresql"{inc_val_5(10)}".head._1)

      assertResult((new InsertResult(count = Some(1)),
        new UpdateResult(count = Some(1)), new DeleteResult(count = Some(1))))(
        tresql"dummy + [10], dummy[dummy = 10] = [11], dummy - [dummy = 11]"
      )

      //braces test
      assertResult(List(0, 0, 2, 2))(tresql"((dummy)d2 ++ ((dummy)d1)d3)d4#(1)".map(_.dummy).toList)

      assertResult(Vector("AMY", "DEVELOPMENT", 2))(
        tresql"work w[empno]emp/dept{ename, dname, hours}#(1, 2, 3)".toListOfVectors.head)

      assertResult(13)(tresql"work w[empno]emp/dept{count(*) cnt}".head.cnt)

      assertResult(2)(tresql"(dummy ++ dummy){count(# dummy)}".head._1)

      assertResult((("A B", 15)))(tresql"{concat('A', ' ', 'B') concat}, {inc_val_5(10) inc}" match {
        case (x, y) => (x.head.concat, y.head.inc)
      })

      assertResult((("A B", 15)))(tresql"[{concat('A', ' ', 'B') concat}, {inc_val_5(10) inc}]" match {
        case (x, y) => (x.head.concat, y.head.inc)
      })

      assertResult((java.sql.Date.valueOf("1980-12-17"), java.sql.Date.valueOf("2021-05-05"),
        850.00, 5000.00))(
          tresql"emp{min(hiredate) minh, max(hiredate) maxh, min(sal) mins, max(sal) maxs}".map { r =>
            import r._
            (minh, maxh, mins, maxs)
          }.toList.head)

      assertResult((("ACCOUNTING", "CLARK, KING, Lara, Nicky")))(
        tresql"dept {dname, |emp{ename}#(1) emps}#(1)"
          .map {d => d.dname -> d.emps.map(_.ename).mkString(", ")}.toList.head
      )

      //resources with params
      {
        val dn = "acc"
        val params = Map("ename" -> "cl%")
        assertResult(List(Vector("ACCOUNTING", "CLARK")))(
          tresql"emp/dept[dname ~~ $dn || '%' & ename ~~ :ename]{dname, ename}#(1, 2)"(using
            resources.withParams(params)).toListOfVectors)
      }

      //column type checking
      {
        val prefix = "Dept: "
        assertResult("ACCOUNTING")(tresql"dept{$prefix || dname}#(1)".head._1.substring(6))
        assertResult("ACCOUNTING")(tresql"dept{dname || $prefix}#(1)".head._1.substring(0, 10))
      }

      //function calls
      assertResult(12)(tresql"inc_val_5(7)")
      assertResult((10, "ACCOUNTING", "NEW YORK"))(
        tresql"sql_concat(sql('select * from dept where deptno = 10'))".head[Int, String, String])

      //recursive queries
      assertResult((7839, "KING", -1, null, 1))(
        tresql"""kings_descendants(nr, name, mgrnr, mgrname, level) {
            emp [ename ~~ 'kin%']{empno, ename, -1, null, 1} +
            emp[emp.mgr = kings_descendants.nr]kings_descendants;emp/emp mgr{
              emp.empno, emp.ename, emp.mgr, mgr.ename, level + 1}
          } kings_descendants{ nr, name, mgrnr, mgrname, level}#(level, mgrnr, nr)""".map(h =>
          (h.nr, h.name, h.mgrnr, h.mgrname, h.level)).toList.head)
        assertResult((7566, "JONES", 7839, "KING", 2))(
          tresql"""kings_descendants(nr, name, mgrnr, mgrname, level) {
              emp [ename ~~ 'kin%']{empno, ename, -1, null, 1} +
              emp[emp.mgr = kings_descendants.nr]kings_descendants;emp/emp mgr{
                emp.empno, emp.ename, emp.mgr, mgr.ename, level + 1}
            } kings_descendants{ nr, name, mgrnr, mgrname, level}#(level, mgrnr, nr)""".map(h =>
            (h.nr, h.name, h.mgrnr, h.mgrname, h.level)).toList.tail.head)
      assertResult((10, "ACCOUNTING"))(tresql"""dept[deptno in (emps(enr, mgr, dnr) {
          emp[ename ~~ 'kin%']{empno, mgr, deptno} + emps[enr = emp.mgr]emp {empno, emp.mgr, deptno}
        } emps{dnr})]{deptno, dname}#(1)""".map(h => (h.deptno, h.dname)).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} + t[t.empno = e.mgr]emp e{e.empno}}
        t{empno}#(1)""".map(_.empno).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} +
        t[t.empno = e.mgr]emp e{e.empno}} t#(1)""".map(_.empno).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} +
        t[t.empno = e.mgr]emp e{e.empno}} t {*}#(1)""".map(_.empno).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} +
        t[t.empno = e.mgr]emp e{e.empno}} t {t.*}#(1)""".map(_.empno).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} +
        t[t.empno = e.mgr]emp e{e.empno}} t a#(1)""".map(_.empno).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} +
        t[t.empno = e.mgr]emp e{e.empno}} t a{*}#(1)""".map(_.empno).toList.head)
      assertResult(7369)(tresql"""t(empno) {emp[ename ~~ 'kin%']{empno} +
        t[t.empno = e.mgr]emp e{e.empno}} t a{a.*}#(1)""".map(_.empno).toList.head)

      //type resolving when column contains select with from clause select
      assertResult(("KING", "ACCOUNTING"))(tresql"""emp[ename ~~ 'kin%'] {
        ename, ((dept[emp.deptno = dept.deptno]{dname}) {dname}) dname}"""
        .map(r => r.ename -> r.dname).toList.head)

      assertResult(List(3, 9))(tresql"dummy{dummy nr, dummy + 1 nr1, dummy + 2 nr2}"
        .map(r => r.nr + r.nr1 + r.nr2).toList.sorted)

    //expressions without select
    assertResult(2.34)(tresql"round(2.33555, 2)")
    assertResult((2.34, 3, 14))(tresql"round(2.33555, 2), round(3.1,0), 5 + 9")
    assertResult(7.3)(tresql"1 + 4 - 0 + round(2.3, 5)")
    assertResult((3,3,2,(2,2))) {
      val x = 1
      ( tresql"1 + $x" + 1,
        tresql"$x + 1" + 1,
        tresql"$x + $x" , // cannot add 1 (compiler error) because variable expression type is Any
        tresql"1 + $x, $x + 1")
    }
    assertResult((true,false)) {
      val (x, y) = (1, 2)
      (tresql"1 in (1,2)" && true, tresql"$x in ($y)" && false)
    }
    assertResult((0,"null",'x',4,false,true)){
      val x = 1
      tresql"1, null, 'x', 1 + $x, $x in (1), $x in (2)" match {
        case (i, n, s, p, i1, i2) =>
          (i - 1, String.valueOf(n), s.charAt(0), p * 2, !i1, !i2)
      }
    }
    assertResult(true)(tresql"1" + tresql"2 + 3" == tresql"6")
    assertResult(true) {
      val x = 0
      tresql"$x in(dummy)"
    }
    assertResult(java.sql.Timestamp.valueOf("2009-02-22 00:00:00.0"))(
      tresql"""date_add ( sql("date '2008-11-22'"), sql("interval 3 month"))""")

    assertResult(6000)(tresql"1000 + (emp[ename = 'KING']{sal})")

    assertResult("ACCOUNTING")(tresql"macro_interpolator_test4(dept, dname)".map(_.dname).toList.head)
    // deeper hierarchical query with type checking
    assertResult(List(("RESEARCH(8)",
      List(("adams", List()), ("ford", List()), ("jones", List()), ("mārtiņš žvūkšķis", List()), ("scott", List(7))),
      List(("bmw", List("barum")), ("mercedes", List("michelin", "nokian")))))) {
      tresql"dept [dname = 'RESEARCH'] {dname, |emp{ename, |[emp.empno = work.empno]work{hours}#(1) work}#(1) emps, |car{name, |tyres{brand}#(1) tyres}#(1) cars}#(1)"
        .map { d =>
          ( s"${d.dname}(${d.dname.size})",
            d.emps.map(e => e.ename.toLowerCase -> e.work.map(_.hours - 1).toList).toList,
            d.cars.map(c => c.name.toLowerCase -> c.tyres.map(_.brand.toLowerCase).toList).toList
          )
      }.toList
    }
    //builder macro invocation in column clause, must return Any type
    assertResult(List("ACCOUNTING")) {
      val l: List[Any] =
        tresql"dept[dname = 'ACCOUNTING']{sql('dname') dn}".map(_.dn).toList
      l
    }
    //builder macro with function signature
    assertResult(List(3)) {
      val l: List[java.lang.Long] = tresql"{plus(1,2) r}".map(_.r).toList
      l
    }
    //return type dependent on parameter type
    assertResult(List("yes")) {
      val l: List[String] = tresql"{(case(1 = 1, 'yes', 'no')) r}".map(_.r).toList
      l
    }
  }
}
