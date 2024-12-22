val scalaV = "3.3.4"

lazy val commonSettings = Seq(
  organization := "org.tresql",
  scalaVersion := scalaV,
  crossScalaVersions := Seq(
      scalaV,
      "2.13.15",
      "2.12.20",
  ),
  //coverageEnabled := true,
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:dynamics",
    "-language:postfixOps", "-language:implicitConversions", "-language:reflectiveCalls",
    "-language:existentials",
  ),
  scalacOptions ++= (if (scalaVersion.value.startsWith("3")) Seq("-explain") else Nil),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
  Compile / publishArtifact := false,
  publishMavenStyle := true,
  Compile / doc / sources := Seq.empty
)

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / versionPolicyIntention := Compatibility.BinaryCompatible

def coreDependencies(scalaVer: String) =
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  ) ++ (if (scalaVer.startsWith("3")) Nil else Seq("org.scala-lang" % "scala-reflect" % scalaVer))

javacOptions ++= Seq("-source", "11", "-target", "11", "-Xlint")
initialize := {
  val _ = initialize.value
  val javaVersion = sys.props("java.specification.version")
  if (javaVersion != "11")
    sys.error("Java 11 is required for this project. Found " + javaVersion + " instead")
}

lazy val core = (project in file("core"))
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .settings(
    name := "tresql-core",
    libraryDependencies ++= coreDependencies(scalaVersion.value),
    unmanagedSources / excludeFilter :=
      (if(scalaVersion.value.startsWith("3")) "" else "Scala3*.scala"),
    publish / skip := true,
  ).settings(commonSettings: _*)

lazy val macros = (project in file("macro"))
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .dependsOn(core)
  .settings(
    name := "tresql-interpolator-macro",
    unmanagedSources / excludeFilter :=
      (if(scalaVersion.value.startsWith("3")) "*.*" else ""),
    publish / skip := true,
)
  .settings(commonSettings: _*)

val packageScopes = Seq(packageBin, packageSrc)

val packageProjects = Seq(core, macros)

val packageMerges = for {
  project <- packageProjects
  scope <- packageScopes
} yield Compile / scope / mappings := (Compile / scope / mappings).value ++ (project / Compile / scope / mappings).value


import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}

lazy val it = (project in file("src/it"))
  .dependsOn(tresql % "compile -> compile; test -> test")
  .settings(commonSettings: _*)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql"       % "42.7.4"  %  Test,
    ),
    Test / scalaSource := baseDirectory.value / "scala",
    Test / resourceDirectory := baseDirectory.value / "resources",
    // IntegrationTest / console / initialCommands := "import org.tresql._; import org.scalatest._; import org.tresql.test.ITConsoleResources._",
  )

lazy val tresql = (project in file("."))
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .dependsOn(core % "test->test;compile->compile", macros)
  .aggregate(core, macros)
  .settings(commonSettings: _*)
  .settings(packageMerges: _*)
  .settings(
    Compile / doc / sources := (core / Compile / sources).value ++ (macros / Compile / sources).value,

    name := "tresql",
    libraryDependencies ++= coreDependencies(scalaVersion.value) ++ {
      val borerV    = scalaVersion.value match {
        case v if v startsWith "2.12" => "1.7.2"
        case v if v startsWith "2.13" => "1.8.0"
        case v if v startsWith "3"    => "1.15.0"
      }
      Seq(
        "org.scalatest" %% "scalatest"        % "3.2.19"  %  Test,
        "org.hsqldb"     % "hsqldb"           % "2.7.3"   %  Test,
        "io.bullet"     %% "borer-core"       % borerV    %  Test,
        "io.bullet"     %% "borer-derivation" % borerV    %  Test,
      )
    },
    Test / console / initialCommands := "import org.tresql._; import org.scalatest._; import org.tresql.test.ConsoleResources._",
    Test / publishArtifact := false,
    Compile / publishArtifact := true,
    pomIncludeRepository := { _ => false },
    pomPostProcess := { (node: XmlNode) =>
      new RuleTransformer(new RewriteRule {
        override def transform(node: XmlNode): XmlNodeSeq = node match {
          case e: Elem if e.label == "dependency" && e.child.exists(child => child.text == "org.tresql") =>
            val organization = e.child.filter(_.label == "groupId").flatMap(_.text).mkString
            val artifact = e.child.filter(_.label == "artifactId").flatMap(_.text).mkString
            val version = e.child.filter(_.label == "version").flatMap(_.text).mkString
            Comment(s"provided dependency $organization#$artifact;$version has been omitted")
          case _ => node
        }
      }).transform(node).head
    },
    pomExtra := <url>https://github.com/mrumkovskis/tresql</url>
      <licenses>
        <license>
          <name>MIT</name>
          <url>http://www.opensource.org/licenses/MIT</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:mrumkovskis/tresql.git</url>
        <connection>scm:git:git@github.com:mrumkovskis/tresql.git</connection>
      </scm>
      <developers>
        <developer>
          <id>mrumkovskis</id>
          <name>Martins Rumkovskis</name>
          <url>https://github.com/mrumkovskis/</url>
        </developer>
        <developer>
          <id>guntiso</id>
          <name>Guntis Ozols</name>
          <url>https://github.com/guntiso/</url>
        </developer>
      </developers>
  )
