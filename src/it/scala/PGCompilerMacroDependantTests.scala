package org.tresql.test

import org.tresql._

import java.sql.{Date, SQLException}

import org.scalatest.funsuite.AnyFunSuite

class PGCompilerMacroDependantTests extends AnyFunSuite with PGCompilerMacroDependantTestsApi  {

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
    assertResult("ACCOUNTING")(Query.unique[String]("dept[deptno in ?]{dname}#(deptno)", Some(10)))
    assertResult("1981-11-17")(Query.unique[java.sql.Date]("emp[sal = 5000]{hiredate}").toString)
    assertResult(BigDecimal(10))(Query.unique[BigDecimal]("dept[10]{deptno}#(deptno)"))
    assertResult(5)(Query.unique[Int]("inc_val_5(?)", 0))
    assertResult(20)(Query.unique[Int]("inc_val_5(inc_val_5(?))", 10))
    assertResult(15)(Query.unique[Long]("inc_val_5(inc_val_5(?))", 5))
    intercept[Exception](Query.head[Int]("emp[?]{empno}", 'z'))
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

    //TODO
    //var op = OutPar()
    //
    //assertResult(List(Vector(List(10, "x"))))(Query("in_out(?, ?, ?)", InOutPar(5), op, "x")
    //    .toListOfVectors)
    //assertResult("x")(op.value)
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
        r.ename.toString + " " + r.job))
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
    //for postgres fails with: util.PSQLException: Bad value for type long : \x00206463
    //assertResult(List[Byte](0, 32, 100, 99)){
    //  val b = Query.head[java.sql.Blob]("car_image[carnr = ?] {image}", 2222).getBinaryStream
    //  Stream.continually(b.read).takeWhile(-1 !=).map(_.toByte).toArray.toList
    //}
    //assertResult(4){
    //  Query.head[java.sql.Blob]("car_image[carnr = ?] {image}", 2222).length
    //}
    assertResult(List("ACCOUNTING", "OPERATIONS", "RESEARCH", "SALES")) {
      Query.list[java.io.Reader]("dept{dname}#(1)").map(r =>
        new String(Stream.continually(r.read).takeWhile(-1 !=).map(_.toChar).toArray)).toList
    }
    //assertResult(List("ACCOUNTING", "OPERATIONS", "RESEARCH", "SALES")) {
    //  Query.list[java.sql.Clob]("dept{dname}#(1)").map(c => {
    //    val r = c.getCharacterStream
    //    new String(Stream.continually(r.read).takeWhile(-1 !=).map(_.toChar).toArray)
    //  }).toList
    //}
    //assertResult(List(Vector(1))) {
    //  Query("+dept_addr", 10, new java.io.StringReader("Strelnieku str."),
    //      new java.io.StringReader("LV-1010"), null).toListOfVectors
    //}
    assertResult(List(Vector(1)))(Query("car_image[carnr = ?]{image} = [?]", 2222,
        new java.io.ByteArrayInputStream(scala.Array[Byte](1, 2, 3, 4, 5, 6, 7))).toListOfVectors)
    assertResult(List(1, 2, 3, 4, 5, 6, 7))(
        Query("car_image[carnr = ?] {image}", 2222).flatMap(_.b("image")).toList)
    //array binding
    assertResult(List(10, 20, 30))(Query.list[Int]("dept[deptno in ?]{deptno}#(1)", List(30, 20, 10)))
    assertResult(List(10, 20, 30))(Query.list[Int]("dept[deptno in ?]{deptno}#(1)", scala.Array(30, 20, 10)))
    //hierarchical inserts, updates test
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emps", List(new InsertResult(count = Some(1)), new InsertResult(count = Some(1)))))))(Query(
      """dept{deptno, dname, loc, +emp {empno, ename, deptno}[:empno, :ename, :deptno] emps} +
        [:deptno, :dname, :loc]""",
      Map("deptno" -> 50, "dname" -> "LAW", "loc" -> "DALLAS",
        "emps" -> List(Map("empno" -> 1111, "ename" -> "SMITH", "deptno" -> 50),
          Map("empno" -> 2222, "ename" -> "LEWIS", "deptno" -> 50)))))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new DeleteResult(count = Some(2))),
      ("emps", List(new InsertResult(count = Some(1)), new InsertResult(count = Some(1)))))))(Query(
      """dept[:deptno]{deptno, dname, loc,
               -emp[deptno = :deptno],
               +emp {empno, ename, deptno} [:empno, :ename, :deptno] emps} =
        [:deptno, :dname, :loc]""",
      Map("deptno" -> 50, "dname" -> "LAW", "loc" -> "FLORIDA",
        "emps" -> List(Map("empno" -> 1111, "ename" -> "BROWN", "deptno" -> 50),
          Map("empno" -> 2222, "ename" -> "CHRIS", "deptno" -> 50)))))
    assertResult(List(new DeleteResult(count = Some(2)), new DeleteResult(count = Some(1))))(
      Query("emp - [deptno = 50], dept - [50]"))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emps", List(new InsertResult(count = Some(1), id = Some(10002)),
        new InsertResult(count = Some(1), id = Some(10003))))), id = Some(10001)))(Query(
      """dept{deptno, dname, loc, +emp {empno, ename, deptno} [#emp, :ename, :#dept] emps} +
        [#dept, :dname, :loc]""",
      Map("dname" -> "LAW", "loc" -> "DALLAS", "emps" -> scala.Array(
        Map("ename" -> "SMITH"), Map("ename" -> "LEWIS")))))

    //tresql string interpolation tests
    assertResult("CLARK, KING, MILLER")(tresql"dept[10] {dname, |emp {ename}#(1) emps}"
        .head.emps.map(_.ename).mkString(", "))
    assertResult((List(Vector(0), Vector(10)),List(Vector(0)))){
      val (a, b) = ("acc%", -1)
      tresql"/(dept[dname ~~ $a]{deptno} + dummy) a#(1), salgrade[$b] {grade} + dummy" match {
        case (r1, r2) => r1.toListOfVectors -> r2.toListOfVectors
      }
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

    //sql array test
    assertResult(("D", "D")) {
      (tresql"results{names}".map(_.names.getArray.asInstanceOf[Array[_]].toList).toList.head.head,
        Query("results{names}").head[java.sql.Array].getArray.asInstanceOf[Array[_]].toList.head)
    }
    assertResult(List("x", "y")) {
      Query("=results [id = 49] { names = :a::'text[]', scores = :b::'decimal[]'}")(
        implicitly[Resources].withParams(Map("a" -> Array("x", "y"),
          "b" -> Array[java.lang.Integer/*must be castable to Array[Object] for java.sql.Array binding*/](1,2,3)))
      )
      // do not choose scores because java.math.BigDecimal cannot be compared with Int
      tresql"results[id = 49] {names, scores}".map(r => r.names.getArray.asInstanceOf[Array[_]].toList).toList.head
    }
  }

  override def ort(implicit resources: Resources) = {
    println("\n----------- ORT tests ------------\n")
    println("\n--- INSERT ---\n")

    var obj:Map[String, Any] = Map("deptno" -> null, "dname" -> "LAW2", "loc" -> "DALLAS",
      "calculated_field"->333, "another_calculated_field"->"A",
      "emp" -> scala.Array(Map("empno" -> null, "ename" -> "SMITH", "deptno" -> null,
          "deptno_name" -> List(Map("name" -> "20, RESEARCH (DALLAS)")),
          "work:empno"->List(Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "empno"->null, "hours"->8, "empno_mgr"->null),
              Map("wdate"->java.sql.Date.valueOf("2012-7-10"), "empno"->null, "hours"->8, "empno_mgr"->null))),
        Map("empno" -> null, "ename" -> "LEWIS", "deptno" -> null,
            "deptno_name" -> List(Map("name" -> "20, RESEARCH (DALLAS)")),
            "work:empno"->List(Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "empno"->null, "hours"->8, "empno_mgr"->null)))))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emp", List(new InsertResult(count = Some(1), children = List(
        ("work:empno", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1))))), id = Some(10005)),
        new InsertResult(count = Some(1), children = List(
          ("work:empno",List(new InsertResult(count = Some(1))))), id = Some(10006))))),
      id = Some(10004)))(
        ORT.insert("dept", obj))
    intercept[Exception](ORT.insert("no_table", obj))

    //insert with set parent id and do not insert existing tables with no link to parent
    //(work under dept)
    obj = Map("deptno" -> 50, "dname" -> "LAW3", "loc" -> "FLORIDA",
        "emp" -> List(Map("empno" -> null, "ename" -> "BROWN", "deptno" -> null),
          Map("empno" -> null, "ename" -> "CHRIS", "deptno" -> null)),
        "work"->List(Map("wdate"->"2012-7-9", "empno"->null, "hours"->8, "empno_mgr"->null)))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emp", List(new InsertResult(count = Some(1), id = Some(10007)),
        new InsertResult(count = Some(1), id = Some(10008))))), id = Some(50)))(ORT.insert("dept", obj))

    obj = Map("dname" -> "FOOTBALL", "loc" -> "MIAMI",
        "emp" -> List(Map("ename" -> "BROWN"), Map("ename" -> "CHRIS")))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emp", List(new InsertResult(count = Some(1), id = Some(10010)),
        new InsertResult(count = Some(1), id = Some(10011))))),
      id = Some(10009)))(ORT.insert("dept", obj))

    obj = Map("ename" -> "KIKI", "deptno" -> 50, "car"-> List(Map("name"-> "GAZ")))
    assertResult(new InsertResult(count = Some(1), id = Some(10012)))(ORT.insert("emp", obj))

    //Ambiguous references to table: emp. Refs: List(Ref(List(empno)), Ref(List(empno_mgr)))
    obj = Map("emp" -> Map("empno" -> null, "ename" -> "BROWN", "deptno" -> null,
            "work"->List(Map("wdate"->"2012-7-9", "empno"->null, "hours"->8, "empno_mgr"->null))))
    intercept[Exception](ORT.insert("emp", obj))

    //child foreign key is also its primary key
    obj = Map("deptno" -> 60, "dname" -> "POLAR", "loc" -> "ALASKA",
              "dept_addr" -> List(Map("addr" -> "Halibut")))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("dept_addr", List(new InsertResult(count = Some(1), id = Some(60))))),
      id = Some(60)))(ORT.insert("dept", obj))
    //child foreign key is also its primary key
    obj = Map("dname" -> "BEACH", "loc" -> "HAWAII",
              "dept_addr" -> List(Map("deptnr" -> 1, "addr" -> "Honolulu", "zip_code" -> "1010")))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("dept_addr", List(new InsertResult(count = Some(1), id = Some(10013))))),
      id = Some(10013)))(ORT.insert("dept", obj))

    obj = Map("deptno" -> null, "dname" -> "DRUGS",
              "car" -> List(Map("nr" -> "UUU", "name" -> "BEATLE")))
    assertResult(new InsertResult(count = Some(1), children = List(
      ("car", List(new InsertResult(count = Some(1), id = Some("UUU"))))),
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
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new DeleteResult(count = Some(0))),
      ("emp", List(new InsertResult(count = Some(1), children = List(
        ("work:empno", List(new InsertResult(count = Some(1)), new InsertResult(count = Some(1))))),
        id = Some(10015)), new InsertResult(count = Some(1), children = List(
        ("work:empno", List())), id = Some(10016)))),
      (null, new DeleteResult(count = Some(0))),
      ("car", List(new InsertResult(count = Some(1), id = Some("EEE")),
        new InsertResult(count = Some(1), id = Some("III")))))))(ORT.update("dept", obj))

    obj = Map("empno"->7788, "ename"->"SCOTT", "mgr"-> 7839,
        "work:empno"->List(
          Map("wdate"->java.sql.Date.valueOf("2012-7-9"), "empno"->7788, "hours"->8, "empno_mgr"->7839),
          Map("wdate"->java.sql.Date.valueOf("2012-7-10"), "empno"->7788, "hours"->8, "empno_mgr"->7839)),
        "calculated_children"->List(Map("x"->5)), "deptno"->40)
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null,new DeleteResult(count = Some(2))),
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
    assertResult(new UpdateResult(count = Some(1), children = List(
      ("dept_addr", List(new UpdateResult(count = Some(1)))))))(ORT.update("dept", obj))

    //value clause test
    //nr column is varchar. postgres is picky about types: 4444 passed in as varchar "4444"
    obj = Map("nr" -> "4444", "deptnr" -> 10)
    assertResult(new UpdateResult(count = Some(1)))(ORT.update("car", obj))
    obj = Map("nr" -> "4444", "deptnr" -> -1)
    //intercept[java.sql.SQLIntegrityConstraintViolationException](ORT.update("car", obj))
    intercept[TresqlException](ORT.update("car", obj))

    //update only children (no first level table column updates)
    obj = Map("nr" -> "4444", "tyres" -> List(Map("brand" -> "GOOD YEAR", "season" -> "S"),
        Map("brand" -> "PIRELLI", "season" -> "W")))
    assertResult(new UpdateResult(None, children = List(
      (null, new DeleteResult(count = Some(0))),
      ("tyres", List(new InsertResult(count = Some(1), id = Some(10017)),
        new InsertResult(count = Some(1), id = Some(10018)))))))(ORT.update("car", obj))

    //delete children
    obj = Map("nr" -> "4444", "name" -> "LAMBORGHINI", "tyres" -> List())
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new DeleteResult(count = Some(2))),
      ("tyres", List()))))(ORT.update("car", obj))

    //update three level, for the first second level object it's third level is empty
    obj = Map("deptno" -> 10013, "emp" -> List(
        Map("ename" -> "ELKHADY",
            "work:empno" -> List()),
        Map("ename" -> "GUNTER",
            "work:empno" -> List(
            Map("wdate" -> java.sql.Date.valueOf("2014-08-27"), "hours" -> 8),
            Map("wdate" -> java.sql.Date.valueOf("2012-08-28"), "hours" -> 8)))))
    assertResult(new UpdateResult(None, children = List(
      (null, new DeleteResult(count = Some(0))),
      ("emp", List(new InsertResult(count = Some(1), children = List(
        ("work:empno", List())), id = Some(10019)), new InsertResult(count = Some(1), children = List(
        ("work:empno", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1))))), id = Some(10020)))))))(ORT.update("dept", obj))
    //delete third level children
    obj = Map("deptno" -> 10013, "emp" -> List(
        Map("ename" -> "ELKHADY",
            "work:empno" -> List()),
        Map("ename" -> "GUNTER",
            "work:empno" -> List())))
    assertResult(new UpdateResult(None, children = List(
      (null,new DeleteResult(count = Some(2))),
      ("emp", List(new InsertResult(count = Some(1), children = List(
        ("work:empno", List())), id = Some(10021)),
        new InsertResult(count = Some(1), children = List(
          ("work:empno", List())), id = Some(10022)))))))(ORT.update("dept", obj))


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
      assertResult(new UpdateResult(count = Some(1), children = List(
        (null,new DeleteResult(count = Some(3))),
        ("emp[+-=]", List(new UpdateResult(count = Some(1)), new UpdateResult(count = Some(1)),
          new InsertResult(count = Some(1), id = Some(10023)),
          new UpdateResult(count = Some(1)))))))(ORT.update("dept", obj))

    obj = Map("empno" -> 7788, "ename"->"SCOTT", "mgr"-> 7839,
      "work:empno[+-=]" -> List(
        Map("wdate"->java.sql.Date.valueOf("2012-7-12"), "empno"->7788, "hours"->10, "empno_mgr"->7839),
        Map("wdate"->java.sql.Date.valueOf("2012-7-13"), "empno"->7788, "hours"->3, "empno_mgr"->7839)),
      "calculated_children"->List(Map("x"->5)), "deptno"->20)
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new DeleteResult(count = Some(2))),
      ("work:empno[+-=]", List(new InsertResult(count = Some(1)),
        new InsertResult(count = Some(1)))))))(ORT.update("emp", obj))

    obj = Map("dname"->"DEVELOPMENT", "loc"->"DETROIT", "calculated_field"-> 222,
        "emp[+-=]"->List(
          Map("empno"->null, "ename"->"AMY", "mgr"->7788, "job"-> "SUPERVIS", "mgr_name"->null, "deptno"->40,
            "work:empno[+-=]"->List(
              Map("wdate"->java.sql.Date.valueOf("2012-7-12"), "empno"->null, "hours"->5, "empno_mgr"->7839),
              Map("wdate"->java.sql.Date.valueOf("2012-7-12"), "empno"->null, "hours"->2, "empno_mgr"->7839))),
          Map("empno"->null, "ename"->"LENE", "mgr"->7566, "job"-> "SUPERVIS", "mgr_name"->null, "deptno"->40,
            "work:empno[+-=]"->List(
              Map("wdate"->java.sql.Date.valueOf("2012-7-14"), "empno"->null, "hours"->5, "empno_mgr"->7839),
              Map("wdate"->java.sql.Date.valueOf("2012-7-15"), "empno"->null, "hours"->2, "empno_mgr"->7839)))),
        "calculated_children"->List(Map("x"->5)), "deptno"->40)
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null,new DeleteResult(count = Some(2))),
      ("emp[+-=]", List(new InsertResult(count = Some(1), children = List(
        ("work:empno[+-=]", List(new InsertResult(count = Some(1)), new InsertResult(count = Some(1))))),
        id = Some(10024)), new InsertResult(count = Some(1), children = List(
        ("work:empno[+-=]", List(new InsertResult(count = Some(1)), new InsertResult(count = Some(1))))),
        id = Some(10025)))))))(
        ORT.update("dept", obj))

    obj = Map("empno"->7788, "ename"->"SCOTT", "mgr"-> 7839,
        "work:empno[+-=]"->List(),
        "calculated_children"->List(Map("x"->5)), "deptno"->20)
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new DeleteResult(count = Some(2))),
      ("work:empno[+-=]", List()))))(ORT.update("emp", obj))

    println("\n---- Multiple table INSERT, UPDATE ------\n")

    obj = Map("dname" -> "SPORTS", "addr" -> "Brisbane", "zip_code" -> "4000")
    assertResult(new InsertResult(count = Some(1), children = List(
      (null, new InsertResult(count = Some(1), id = Some(10026)))), id = Some(10026)))(
      ORT.insertMultiple(obj, "dept", "dept_addr")())

    obj = Map("deptno" -> 10026, "loc" -> "Brisbane", "addr" -> "Roma st. 150")
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new UpdateResult(count = Some(1))))))(ORT.updateMultiple(obj, "dept", "dept_addr")())

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
    assertResult(new InsertResult(count = Some(1), children = List(
      ("dept_addr", List(new InsertResult(count = Some(1), children = List(
        ("dept_sub_addr", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1))))), id = Some(10027))))),
      id = Some(10027))) {ORT.insert("dept", obj)}

    obj = Map("deptno" -> 10027, "dname" -> "PANDA BREEDING",
      "dept_addr" -> List(Map("addr" -> "Chengdu", "zip_code" -> "CN-1234",
        "dept_sub_addr" -> List(Map("addr" -> "Jinli str. 10", "zip_code" -> "CN-1234"),
          Map("addr" -> "Jinjiang District", "zip_code" -> "CN-1234")))))
    assertResult(new UpdateResult(count = Some(1), children = List(
      ("dept_addr", List(new UpdateResult(count = Some(1), children = List(
        (null, new DeleteResult(count = Some(2))),
        ("dept_sub_addr", List(new InsertResult(count = Some(1)),
          new InsertResult(count = Some(1))))))))))) {ORT.update("dept", obj)}

    println("\n-------- LOOKUP object editing --------\n")
    //edit lookup object
    obj = Map("brand" -> "DUNLOP", "season" -> "W", "carnr" -> Map("name" -> "VW"))
    assertResult(new InsertResult(count = Some(1), id = Some(10029))) { ORT.insert("tyres", obj) }
    obj = Map("brand" -> "CONTINENTAL", "season" -> "W", "carnr" -> Map("nr" -> "UUU", "name" -> "VW"))
    assertResult(new InsertResult(count = Some(1), id = Some(10030))) { ORT.insert("tyres", obj) }
    obj = Map("nr" -> 10029, "season" -> "S", "carnr" -> Map("name" -> "SKODA"))
    assertResult(new UpdateResult(count = Some(1))) { ORT.update("tyres", obj) }
    obj = Map("nr" -> 10029, "brand" -> "DUNLOP", "carnr" -> Map("nr" -> "UUU", "name" -> "VOLKSWAGEN"))
    assertResult(new UpdateResult(count = Some(1))) { ORT.update("tyres", obj) }
    //one to one relationship with lookup for extended table
    obj = Map("dname" -> "MARKETING", "addr" -> "Valkas str. 1",
        "zip_code" -> "LV-1010", "addr_nr" -> Map("addr" -> "Riga"))
    assertResult(new InsertResult(count = Some(1), children = List(
      (null, List(10033, new InsertResult(count = Some(1), id = Some(10032))))),
      id = Some(10032))) { ORT.insertMultiple(obj, "dept", "dept_addr")() }
    obj = Map("deptno" -> 10032, "dname" -> "MARKET", "addr" -> "Valkas str. 1a",
      "zip_code" -> "LV-1010", "addr_nr" -> Map("nr" -> 10033, "addr" -> "Riga, LV"))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, List(10033, new UpdateResult(count = Some(1))))))) {
      ORT.updateMultiple(obj, "dept", "dept_addr")()
    }
    obj = Map("deptno" -> 10032, "dname" -> "MARKETING", "addr" -> "Liela str.",
      "zip_code" -> "LV-1010", "addr_nr" -> Map("addr" -> "Saldus"))
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, List(10034, new UpdateResult(count = Some(1))))))) {
      ORT.updateMultiple(obj, "dept", "dept_addr")()
    }
    //insert of lookup object where it's pk is present but null
    obj = Map("nr" -> 10029, "brand" -> "DUNLOP", "carnr" -> Map("nr" -> null, "name" -> "AUDI"))
    assertResult(new UpdateResult(count = Some(1))) { ORT.update("tyres", obj) }

    println("\n----------------- Multiple table INSERT UPDATE extended cases ----------------------\n")

    obj = Map("dname" -> "AD", "ename" -> "Lee", "wdate" -> java.sql.Date.valueOf("2000-01-01"), "hours" -> 3)
    assertResult(new InsertResult(count = Some(1), children = List(
      (null, new InsertResult(count = Some(1), id = Some(10036))),
      (null, new InsertResult(count = Some(1), id = Some(10036)))), id = Some(10036))) {
      ORT.insertMultiple(obj, "dept", "emp", "work:empno")()
    }

    obj = Map("deptno" -> 10036, "wdate" -> java.sql.Date.valueOf("2015-10-01"), "hours" -> 4)
    assertResult(new UpdateResult(None, List((null, new UpdateResult(Some(1))), (null, new UpdateResult(Some(1)))))) {
      ORT.updateMultiple(obj, "dept", "emp", "work:empno")()
    }

    obj = Map("deptno" -> 10036, "work:empno" -> List(Map("wdate" -> java.sql.Date.valueOf("2015-10-10"), "hours" -> 5)))
    assertResult(new UpdateResult(None, children = List(
      (null, new UpdateResult(None, children = List(
        (null, new DeleteResult(count = Some(1))),
        ("work:empno", List(new InsertResult(count = Some(1)))))))))) {
      ORT.updateMultiple(obj, "dept", "emp")()
    }

    obj = Map("deptno" -> 10036, "dname" -> "ADVERTISING", "ename" -> "Andy")
    assertResult(new UpdateResult(count = Some(1), children = List(
      (null, new UpdateResult(count = Some(1)))))) { ORT.updateMultiple(obj, "dept", "emp")() }

    obj = Map("dname" -> "DIG", "emp" -> List(Map("ename" -> "O'Jay",
        "work:empno" -> List(Map("wdate" -> java.sql.Date.valueOf("2010-05-01"), "hours" -> 7)))), "addr" -> "Tvaika 1")
    assertResult(new InsertResult(count = Some(1), children = List(
      ("emp", List(new InsertResult(count = Some(1), children = List(
        ("work:empno", List(new InsertResult(count = Some(1))))), id = Some(10038)))),
      (null, new InsertResult(count = Some(1), id = Some(10037)))), id = Some(10037))) {
      ORT.insertMultiple(obj, "dept", "dept_addr")()
    }

    obj = Map("dname" -> "GARAGE", "name" -> "Nissan", "brand" -> "Dunlop", "season" -> "S")
    assertResult(new InsertResult(count = Some(1), children = List(
      (null, new InsertResult(count = Some(1), id = Some(10039))),
      (null, new InsertResult(count = Some(1), id = Some(10039)))), id = Some(10039))) {
      ORT.insertMultiple(obj, "dept", "car", "tyres")()
    }

    //obj = Map("deptno" -> 10039, "dname" -> "STOCK", "name" -> "Nissan", "brand" -> "PIRELLI", "season" -> "W")
    //obj = Map("dname" -> "STOCK", "name" -> "Nissan", "brand" -> "PIRELLI", "season" -> "W")
    //assertResult((1, List(1, 1))) { ORT.updateMultiple(obj, "dept", "car", "tyres")() }

    //obj = Map("deptno" -> 10039, "dname" -> "STOCK", "name" -> "Nissan Patrol",
    //    "tyres" -> List(Map("brand" -> "CONTINENTAL", "season" -> "W")))
    //assertResult((1,List((1,List(1, List((1,10040))))))) { ORT.updateMultiple(obj, "dept", "car")() }

    //obj = Map("dname" -> "NEW STOCK", "name" -> "Audi Q7",
    //    "tyres" -> List(Map("brand" -> "NOKIAN", "season" -> "S")))
    //assertResult(((1,List(((1,List(List((1,10042)))),10041))),10041)) { ORT.insertMultiple (obj, "dept", "car")() }

    println("\n-------- LOOKUP extended cases - chaining, children --------\n")

    //obj = Map("brand" -> "Nokian", "season" -> "W", "carnr" ->
    //  Map("name" -> "Mercedes", "deptnr" -> Map("dname" -> "Logistics")))
    //assertResult(List(10044, (1,10045))) { ORT.insert("tyres", obj) }

    //obj = Map("nr" -> 10045, "brand" -> "Nokian", "season" -> "S", "carnr" ->
    //  Map("nr" -> 10044, "name" -> "Mercedes Benz", "deptnr" -> Map("deptno" -> 10043, "dname" -> "Logistics dept")))
    //assertResult(List(10044, 1)) { ORT.update("tyres", obj) }

    //obj = Map("dname" -> "MILITARY", "loc" -> "Alabama", "emp" -> List(
    //  Map("ename" -> "Selina", "mgr" -> null),
    //  Map("ename" -> "Vano", "mgr" -> Map("ename" -> "Carlo", "deptno" -> Map("dname" -> "Head")))))
    //assertResult(((1,List(List(List(null, (1,10047)), List(10049, (1,10050))))),10046)) { ORT.insert("dept", obj) }

    //obj = Map("deptno" -> 10046, "dname" -> "METEO", "loc" -> "Texas", "emp" -> List(
    //  Map("ename" -> "Selina", "mgr" -> null),
    //  Map("ename" -> "Vano", "mgr" -> Map("ename" -> "Pedro", "deptno" -> Map("deptno" -> 10048, "dname" -> "Head")))))
    //assertResult((1,List(2, List(List(null, (1,10051)), List(10052, (1,10053)))))) { ORT.update("dept", obj) }

    //obj = Map("name" -> "Dodge", "car_usage" -> List(
    //  Map("empno" -> Map("empno" -> null, "ename" -> "Nicky", "job" -> "MGR", "deptno" -> 10)),
    //  Map("empno" -> Map("empno" -> 10052, "ename" -> "Lara", "job" -> "MGR", "deptno" -> 10))))
    //assertResult(((1,List(List(List(10055, 1), List(10052, 1)))),10054)) { ORT.insert("car", obj) }

    println("\n-------- INSERT, UPDATE, DELETE with additional filter --------\n")
    //insert, update with additional filter
    assertResult(new InsertResult(count = Some(0))){ORT.insert("dummy", Map("dummy" -> 2), "dummy = -1")}
    assertResult(new InsertResult(count = Some(1))){ORT.insert("dummy d", Map("dummy" -> 2), "d.dummy = 2")}
    assertResult(new UpdateResult(count = Some(0))){ORT.update("address a", Map("nr" -> 10033, "addr" -> "gugu"), "a.addr ~ 'Ri'")}
    assertResult(new DeleteResult(count = Some(0))) { ORT.delete("emp e", 10053, "ename ~~ :ename", Map("ename" -> "ivans%")) }
    //assertResult(1) { ORT.delete("emp e", 10053, "ename ~~ :ename", Map("ename" -> "van%")) }

    println("\n---- Object INSERT, UPDATE ------\n")

    implicit def pohatoMap[T <: Poha](o: T): (String, Map[String, _]) = o match {
      case Car(nr, name) => "car" -> Map("nr" -> nr, "name" -> name)
    }
    assertResult(new InsertResult(count = Some(1), id = Some(8888)))(ORT.insertObj(Car(8888, "OPEL")))
    //assertResult(1)(ORT.updateObj(Car(8888, "SAAB")))

    println("\n-------- SAVE - extended cases --------\n")

//    obj = Map("dname" -> "TRUCK DEPT",
//      "car[+=]" -> List(
//        Map("name" -> "VOLVO",
//          "tyres[+=]" -> List(
//            Map("brand" -> "BRIDGESTONE", "season" -> "S",
//              "tyres_usage[+=]" -> List(
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2015-04-25")),
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2015-05-01")))),
//            Map("brand" -> "COPARTNER", "season" -> "W",
//              "tyres_usage[+=]" -> List(
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2015-09-25")),
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2015-10-01")))))
//       ),
//       Map("name" -> "TATA",
//          "tyres[+=]" -> List(
//           Map("brand" -> "METRO TYRE", "season" -> "S",
//             "tyres_usage[+=]" -> List(
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2016-04-25")),
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2016-05-01")))),
//           Map("brand" -> "GRL", "season" -> "W",
//             "tyres_usage[+=]" -> List(
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2016-09-25")),
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> java.sql.Date.valueOf("2016-10-01"))))))))
//    assertResult(((1,List(
//        List(((1,List(List(((1,List(List(1, 1))),10058), ((1,List(List(1, 1))),10059)))),10057),
//             ((1,List(List(((1,List(List(1, 1))),10061), ((1,List(List(1, 1))),10062)))),10060))
//        )),10056))(ORT.insert("dept", obj))

//    obj = Map("deptno" -> 10056, "dname" -> "TRUCK DEPT",
//      "car[+=]" -> List(
//        Map("nr" -> 10057, "name" -> "VOLVO",
//          "tyres[+=]" -> List(
//            Map("nr" -> 10058, "brand" -> "BRIDGESTONE", "season" -> "S",
//              "tyres_usage[+=]" -> List(
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2016-04-01"))),
//            Map("nr" -> null, "brand" -> "ADDO", "season" -> "W",
//              "tyres_usage[+=]" -> List(
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2016-09-25"),
//                Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2016-10-01"))))),
//        Map("nr" -> 10060, "name" -> "TATA MOTORS",
//          "tyres[+=]" -> List(
//           Map("nr" -> 10061, "brand" -> "METRO TYRE", "season" -> "S",
//             "tyres_usage[+=]" -> List(
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2015-04-25"),
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2015-05-01"))),
//           Map("nr" -> 10062, "brand" -> "GRL", "season" -> "W",
//             "tyres_usage[+=]" -> List(
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2015-09-25"),
//               Map("carnr" -> null /*value expr is used from env*/, "date_from" -> "2015-10-01")))))))
//      assertResult((1,List(List((1,List(List((1,List(List(1))), ((1,List(List(1, 1))),10063)))),
//          (1,List(List((1,List(List(1, 1))), (1,List(List(1, 1))))))))))(ORT.update("dept", obj))

//    obj = Map("deptno" -> 10056, "car[+-=]" -> List(
//      Map("nr" -> 10060, "tyres[+-=]" -> List(
//        Map("brand" -> "BRIDGESTONE", "season" -> "W")
//      )),
//      Map("nr" -> 10057, "tyres[+-=]" -> List(
//        Map("nr" -> 10063, "brand" -> "HANKOOK", "season" -> "S"),
//        Map("nr" -> 10059, "brand" -> "FIRESTONE", "season" -> "W"),
//        Map("brand" -> "YOKOHAMA", "season" -> "S")
//      ))
//    ))
//    assertResult(new UpdateResult(None, scala.collection.immutable.ListMap(
//      "_1" -> new DeleteResult(Some(0)), "car[+-=]" -> List(
//        new UpdateResult(None, scala.collection.immutable.ListMap(
//          "_1" -> new DeleteResult(Some(2)), "tyres[+-=]" -> List(
//            new InsertResult(Some(1), Map(), Some(10064))))),
//        new UpdateResult(None, scala.collection.immutable.ListMap(
//          "_1" -> new DeleteResult(Some(1)), "tyres[+-=]" -> List(
//            new UpdateResult(Some(1)),
//            new UpdateResult(Some(1)),
//            new InsertResult(Some(1), Map(), Some(10065)))))
//        ))))(ORT.update("dept", obj))

//    obj = Map("deptno" -> 10056, "car[+-=]" -> List(
//      Map("nr" -> 10060, "tyres[+-=]" -> List(
//        Map("nr" -> 10064, "brand" -> "BRIDGESTONE", "season" -> "S")
//      )),
//      Map("nr" -> 10057, "tyres[+-=]" -> List(
//        Map("nr" -> 10063, "brand" -> "HANKOOK", "season" -> "W"),
//        Map("nr" -> 10059, "brand" -> "FIRESTONE", "season" -> "S"),
//        Map("nr" -> 10065, "brand" -> "YOKOHAMA", "season" -> "W")
//      ))
//    ))
//    assertResult(new UpdateResult(None, scala.collection.immutable.ListMap(
//      "_1" -> new DeleteResult(Some(0)), "car[+-=]" -> List(
//        new UpdateResult(None, scala.collection.immutable.ListMap(
//          "_1" -> new DeleteResult(Some(0)), "tyres[+-=]" -> List(
//            new UpdateResult(Some(1))))),
//        new UpdateResult(None, scala.collection.immutable.ListMap(
//          "_1" -> new DeleteResult(Some(0)), "tyres[+-=]" -> List(
//            new UpdateResult(Some(1)),
//            new UpdateResult(Some(1)),
//            new UpdateResult(Some(1)))))
//      ))))(ORT.update("dept", obj))

//    obj = Map("deptno" -> 10056, "car[+-=]" -> List(
//      Map("nr" -> 10057, "tyres[+-=]" -> List(
//        Map("nr" -> 10063, "brand" -> "HANKOOK", "season" -> "S"),
//        Map("nr" -> 10059, "brand" -> "FIRESTONE", "season" -> "W"),
//        Map("nr" -> 10065, "brand" -> "YOKOHAMA", "season" -> "S")
//      )),
//      Map("nr" -> 10060, "tyres[+-=]" -> List(
//        Map("brand" -> "BRIDGESTONE", "season" -> "S"),
//        Map("brand" -> "BRIDGESTONE", "season" -> "W")
//      ))
//    ))
//    assertResult(new UpdateResult(None, Map("_1" -> new DeleteResult(Some(0)), "car[+-=]" -> List(
//      new UpdateResult(None, Map("_1" -> new DeleteResult(Some(0)), "tyres[+-=]" -> List(
//        new UpdateResult(Some(1)), new UpdateResult(Some(1)), new UpdateResult(Some(1))))),
//      new UpdateResult(None, Map("_1" -> new DeleteResult(Some(1)), "tyres[+-=]" -> List(
//        new InsertResult(Some(1), Map(), Some(10066)),
//        new InsertResult(Some(1), Map(), Some(10067)))))))))(
//    ORT.update("dept", obj))

//    println("\n-------- SAVE - extended cases - multiple children --------\n")

//    obj = Map("dname" -> "Service", "emp#work:empno" ->
//      Map("ename" -> "Sophia", "wdate" -> java.sql.Date.valueOf("2015-10-30"), "hours" -> 2))
//    assertResult(((1,List(((1,List((1,10069))),10069))),10068))(ORT.insert("dept", obj))

//    obj = Map("deptno" -> 10068, "dname" -> "Services", "emp#work:empno[+-=]" ->
//      Map("empno" -> 10069, "wdate" -> java.sql.Date.valueOf("2015-10-30"), "hours" -> 8))
//    assertResult((1,List(0, List(1))))(ORT.update("dept", obj))

//    obj = Map("deptno" -> 10068, "emp#work:empno[=]" ->
//      Map("empno" -> 10069, "empno_mgr" -> Map("ename" -> "Jean", "deptno" -> 10068)))
//    assertResult(List(List(List(10070, 1))))(ORT.update("dept", obj))

//    obj = Map("nr" -> 10057, "is_active" -> true, "emp#car_usage" ->
//      Map("ename" -> "Peter", "date_from" -> "2015-11-02",
//        "deptno" -> Map("dname" -> "Supervision")))
//    assertResult((1,List(0, List(10071, ((1,List((1,10072))),10072)))))(ORT.update("car", obj))

//    println("\n--- LOOKUP extended case - separate lookup expression from previous insert expr values ---\n")
//    obj = Map("dname" -> "Design", "name" -> "Tesla", "date_from" -> "2015-11-20",
//      "empno" -> Map("ename" -> "Inna", "deptno" -> 10068))
//    assertResult(((1,List((1,10073), List(10074, (1,10073)))),10073))(
//      ORT.insertMultiple(obj, "dept", "car", "car_usage")())

//    println("\n--- Delete all children with save options specified ---\n")
//    obj = Map("nr" -> 10035, "tyres[+-=]" -> Nil)
//    assertResult(List(1, List()))(ORT.update("car", obj))

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

//    obj = Map("dname" -> "Temp", "addr" -> "Field", "zip_code" -> "none", "dept_sub_addr" ->
//      List(Map("addr" -> "Hill", "zip_code" -> "----"),
//           Map("addr" -> "Pot", "zip_code" -> "----")
//      )
//    )
//    assertResult(new InsertResult(Some(1),
//      Map("_1" -> new InsertResult(
//        Some(1),
//        Map("dept_sub_addr" -> List(new InsertResult(Some(1)), new InsertResult(Some(1)))),
//        Some(10075))
//      ), Some(10075)))(ORT.insertMultiple(obj, "dept", "dept_addr")())

    obj = Map("dname" -> "Temp", "addr" -> "Field", "zip_code" -> "none", "dept_sub_addr" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      ), "filter_condition" -> false
    )
//    assertResult(new InsertResult(Some(0), Map(), Some(10076)))(
    assertResult(new InsertResult(Some(0), Nil, Some(10040)))(
      ORT.insertMultiple(obj, "dept", "dept_addr")(":filter_condition = true"))

    obj = Map("dname" -> "Temp1", "addr" -> "Field1", "zip_code" -> "none",
      "dept_sub_addr|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
      List(Map("addr" -> "Hill", "zip_code" -> "----"),
           Map("addr" -> "Pot", "zip_code" -> "----")
      ), "filter_condition" -> false
    )
    assertResult(new InsertResult(Some(1),
      List((null, new InsertResult(
        Some(1),
        List("dept_sub_addr|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
          List(new InsertResult(Some(0)), new InsertResult(Some(0)))),
    Some(10041)))
    ), Some(10041)))(ORT.insertMultiple(obj, "dept", "dept_addr")())

//    obj = Map("deptno" -> 10077, "dname" -> "Temp2", "addr" -> "Field2", "zip_code" -> "----",
//      "dept_sub_addr[+-=]|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
//      List(Map("addr" -> "Hill", "zip_code" -> "----"),
//           Map("addr" -> "Pot", "zip_code" -> "----")
//      ),
//      "emp[+-=]|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
//      List(Map("ename" -> "X"), Map("empno" -> 7369, "ename" -> "Y")),
//      "filter_condition" -> false
//    )
//    assertResult(
//      new UpdateResult(
//        Some(1),
//        Map(
//          "_1" -> new DeleteResult(Some(0)),
//          "emp[+-=]|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
//            List(new InsertResult(Some(0), Map(), Some(10078)), new UpdateResult(Some(0))),
//          "_3" -> new UpdateResult(
//            Some(1),
//            Map(
//              "_1" -> new DeleteResult(Some(0)),
//              "dept_sub_addr[+-=]|:filter_condition = true,:filter_condition = true,:filter_condition = true" ->
//                List(new InsertResult(Some(0)), new InsertResult(Some(0)))
//            ))
//        )
//      )
//    )(ORT.updateMultiple(obj, "dept", "dept_addr")())

    //should not delete dept_sub_addr children
//    obj = Map("deptno" -> 10075, "addr" -> "Field alone",
//      "dept_sub_addr[+-=]|:filter_condition = true, :filter_condition = true, :filter_condition = true" ->
//      List(Map("addr" -> "Hill", "zip_code" -> "----"),
//           Map("addr" -> "Pot", "zip_code" -> "----")
//      ), "filter_condition" -> false
//    )
//    assertResult(new UpdateResult(
//      None,
//      Map("_1" ->
//        new UpdateResult(
//          Some(1),
//          Map(
//            "_1" -> new DeleteResult(Some(0)),
//            "dept_sub_addr[+-=]|:filter_condition = true, :filter_condition = true, :filter_condition = true" ->
//              List(new InsertResult(Some(0)), new InsertResult(Some(0)))
//          )
//        )
//      )
//    ))(ORT.updateMultiple(obj, "dept", "dept_addr")())

    //should delete dept_sub_addr children
//    obj = Map("deptno" -> 10075, "addr" -> "Field alone",
//      "dept_sub_addr[+-=]|:filter_condition = true, null, :filter_condition = true" ->
//      List(Map("addr" -> "Hill", "zip_code" -> "----"),
//           Map("addr" -> "Pot", "zip_code" -> "----")
//      ), "filter_condition" -> false
//    )
//    assertResult(new UpdateResult(
//      None,
//      Map("_1" ->
//        new UpdateResult(
//          Some(1),
//          Map(
//            "_1" -> new DeleteResult(Some(2)),
//            "dept_sub_addr[+-=]|:filter_condition = true, null, :filter_condition = true" ->
//             List(new InsertResult(Some(0)), new InsertResult(Some(0)))
//          )
//        )
//      )
//    ))(ORT.updateMultiple(obj, "dept", "dept_addr")())

    println("----- SAVE with resolver for self column -----")
    //resolving the same column
    obj = Map("deptno" -> 10004,
      "dname" -> "legal",
      "dname->" -> "dname=upper(_)")
    assertResult(new UpdateResult(Some(1)))(
      ORT.updateMultiple(obj, "dept")())

    obj = Map("dname" -> "attorney", "dname->" -> "dname=upper(_)")
//    assertResult(new InsertResult(Some(1), Map(), Some(10079)))(ORT.insert("dept", obj)())
    assertResult(new InsertResult(Some(1), Nil, Some(10042)))(ORT.insert("dept", obj))

    obj = Map("deptno" -> 10079, "dname" -> "devop", "dname->" -> "dname=upper(_)")
//    assertResult(new UpdateResult(Some(1)))(ORT.update("dept", obj)())
    assertResult(new UpdateResult(Some(0)))(ORT.update("dept", obj))

//    assertResult(List("DEVOP"))(tresql"dept[10079]{dname}".map(_.dname).toList)
    assertResult(List("ATTORNEY"))(tresql"dept[10042]{dname}".map(_.dname).toList)

    //primary key component not specified error must be thrown
    obj = Map("car_nr" -> "1111")
    intercept[TresqlException](ORT.insert("car_usage", obj))
    obj = Map("date_from" -> "2013-10-24")
    intercept[TresqlException](ORT.insert("car_usage", obj))
    obj = Map("empno" -> 7839)
    intercept[TresqlException](ORT.insert("car_usage", obj))
  }
  override def compilerMacro(implicit resources: Resources) = {
    println("\n-------------- TEST compiler macro ----------------\n")

    assertResult(("ACCOUNTING",
    //List(("CLARK",List()), ("KING",List(3, 4)), ("Lara",List()), ("Nicky",List()))))(
    List(("CLARK",List()), ("KING",List(3, 4)))))(
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
    //assertResult(List(0, 0, 2, 2))(tresql"((dummy)d2 ++ ((dummy)d1)d3)d4#(1)".map(_.dummy).toList)
    assertResult(List(0, 0, 2, 2))(tresql"((dummy)d2 ++ ((dummy)d1)d3)d4#(1)".map(_.dummy).toList)

    assertResult(Vector("AMY", "DEVELOPMENT", 2))(
      tresql"work w[empno]emp/dept{ename, dname, hours}#(1, 2, 3)".toListOfVectors.head)

    //assertResult(13)(tresql"work w[empno]emp/dept{count(*) cnt}".head.cnt)
    assertResult(12)(tresql"work w[empno]emp/dept{count(*) cnt}".head.cnt)

    //assertResult(2)(tresql"(dummy d3 ++ dummy d2){count(# dummy) d1}".head._1)

    assertResult((("A B", 15)))(tresql"{concat('A', ' ', 'B') concat}, {inc_val_5(10) inc}" match {
      case (x, y) => (x.head.concat, y.head.inc)
    })

    assertResult((("A B", 15)))(tresql"[{concat('A', ' ', 'B') concat}, {inc_val_5(10) inc}]" match {
      case (x, y) => (x.head.concat, y.head.inc)
    })

    assertResult((java.sql.Date.valueOf("1980-12-17"), java.sql.Date.valueOf("1983-01-12"),
      850.00, 5000.00))(
        tresql"emp{min(hiredate) minh, max(hiredate) maxh, min(sal) mins, max(sal) maxs}".map { r =>
          import r._
          (minh, maxh, mins, maxs)
        }.toList.head)

    //assertResult((("ACCOUNTING", "CLARK, KING, Lara, Nicky")))(
    assertResult((("ACCOUNTING", "CLARK, KING")))(
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
    //assertResult((7839, "KING", -1, null, 1))(
    //  tresql"""kings_descendants(nr, name, mgrnr, mgrname, level) {
    //      emp [ename ~~ 'kin%']{empno, ename, -1, null, 1} +
    //      emp[emp.mgr = kings_descendants.nr]kings_descendants;emp/emp mgr{
    //        emp.empno, emp.ename, emp.mgr, mgr.ename, level + 1}
    //    } kings_descendants{ nr, name, mgrnr, mgrname, level}#(level, mgrnr, nr)""".map(h =>
    //    (h.nr, h.name, h.mgrnr, h.mgrname, h.level)).toList.head)
    //  assertResult((7566, "JONES", 7839, "KING", 2))(
    //    tresql"""kings_descendants(nr, name, mgrnr, mgrname, level) {
    //        emp [ename ~~ 'kin%']{empno, ename, -1, null, 1} +
    //        emp[emp.mgr = kings_descendants.nr]kings_descendants;emp/emp mgr{
    //          emp.empno, emp.ename, emp.mgr, mgr.ename, level + 1}
    //     } kings_descendants{ nr, name, mgrnr, mgrname, level}#(level, mgrnr, nr)""".map(h =>
    //     (h.nr, h.name, h.mgrnr, h.mgrname, h.level)).toList.tail.head)
    assertResult((10, "ACCOUNTING"))(tresql"""dept[deptno in (emps(enr, mgr, dnr) {
        emp[ename ~~ 'kin%']{empno, mgr, deptno} + emps[enr = emp.mgr]emp {empno, emp.mgr, deptno}
      } emps{dnr})]{deptno, dname}#(1)""".map(h => (h.deptno, h.dname)).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} + t[t.empno = e.mgr]emp e{e.empno}}
      t{empno}#(1)""".map(_.empno).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} +
      t[t.empno = e.mgr]emp e{e.empno}} t#(1)""".map(_.empno).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} +
      t[t.empno = e.mgr]emp e{e.empno}} t {*}#(1)""".map(_.empno).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} +
      t[t.empno = e.mgr]emp e{e.empno}} t {t.*}#(1)""".map(_.empno).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} +
      t[t.empno = e.mgr]emp e{e.empno}} t a#(1)""".map(_.empno).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} +
      t[t.empno = e.mgr]emp e{e.empno}} t a{*}#(1)""".map(_.empno).toList.head)
    assertResult(7369)(tresql"""t(*) {emp[ename ~~ 'kin%']{empno} +
      t[t.empno = e.mgr]emp e{e.empno}} t a{a.*}#(1)""".map(_.empno).toList.head)
    assertResult((List(7),List(8),List(8)))(
      tresql"""[+dummy {dummy} [7] {dummy},
              =dummy[dummy = 7] {dummy = dummy + 1} {*},
              dummy - [dummy = 8] {*}]""" match { case (a, b, c) =>
          (a.map(_.dummy).toList, b.map(_.dummy).toList, c.map(_.dummy).toList)
        }
    )
    assertResult(List((0, "kiki"), (2, "kiki")))(tresql"dummy {*, 'kiki' k} #(1)".map(r => r.dummy -> r.k).toList)
    assertResult(List((10033, "Riga, LV")))(tresql"=dept_addr [addr_nr = a.nr] (address a {a.nr, a.addr}) a [a.addr ~ 'Riga%'] {addr = a.addr} {dept_addr.addr_nr, a.addr}".map(r => r.addr_nr -> r.addr).toList)

    //expressions without select
    assertResult(1)(tresql"1")
    assertResult((1,2,3))(tresql"1, 2, 3")
    assertResult(2.34)(tresql"round(2.33555, 2)")
    assertResult((2.34, 3 ,14))(tresql"round(2.33555, 2), round(3.1,0), 5 + 9")
    assertResult(7.3)(tresql"1 + 4 - 0 + round(2.3, 5)")
    assertResult("2.34")(tresql"round(2.33555, 2)::string")
    assertResult(2)(tresql"2.3::int")
    assertResult(2.3)(tresql"'2.3'::decimal")
    assertResult(java.sql.Date.valueOf("2000-01-01"))(tresql"'2000/01/01'::date")
    assertResult(java.sql.Timestamp.valueOf("2000-01-01 00:00:00.0"))(tresql"'2000/01/01'::timestamp")
    assertResult(2)(tresql"round(2.3, 1)::int")
    assertResult((2, 2.3, java.sql.Date.valueOf("2000-01-01"), java.sql.Timestamp.valueOf("2000-01-01 00:00:00.0")))(
      tresql"2.3::int, '2.3'::decimal, '2000/01/01'::date, '2000/01/01'::timestamp"
    )
    assertResult((3,5))(tresql"(1 + 2)::int, (2 + 3)::int")
    assertResult(2.3) {
      val x = 1;
      tresql"round(2.33555, $x)"
    }
    assertResult(3)(
      tresql"""sql("(date '2001-10-01' - date '2001-09-28')")::int""")
    assertResult(java.time.LocalDateTime.parse("2001-09-28T01:00"))(
      tresql"""sql("(date '2001-09-28' + interval '1 hour')")::timestamp""".toLocalDateTime)
    assertResult(true) {
      val x = 0
      tresql"$x in(dummy)"
    }
    assertResult(List(0, 2))(tresql"dummy{dummy::long}#(1)".map(_.dummy).toList)

    assertResult(10)(tresql"dept{min(deptno)::integer}".unique[Long])

    //type resolving when column contains select with from clause select
    assertResult(("KING", "ACCOUNTING"))(tresql"""emp e[ename ~~ 'kin%'] {
      ename, ((dept d[e.deptno = d.deptno]{dname}) x {dname}) dname}"""
        .map(r => r.ename -> r.dname).toList.head)

    assertResult(List(3, 9))(tresql"dummy{dummy nr, dummy + 1 nr1, dummy + 2 nr2}"
      .map(r => r.nr + r.nr1 + r.nr2).toList.sorted)
  }
}
