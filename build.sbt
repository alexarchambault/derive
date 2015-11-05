
lazy val root = project.in(file("."))
  .aggregate(deriveJVM, deriveJS)
  .settings(commonSettings)
  .settings(compileSettings)
  .settings(noPublishSettings)

lazy val derive = crossProject
  .settings(commonSettings: _*)
  .settings(compileSettings: _*)
  .settings(publishSettings: _*)
  .jsSettings(scalaJSStage in Test := FastOptStage)

lazy val deriveJVM = derive.jvm
lazy val deriveJS = derive.js

lazy val commonSettings = Seq(
  organization := "com.github.alexarchambault",
  name := "derive",
  version := "0.1.0-SNAPSHOT",
  resolvers += Resolver.sonatypeRepo("snapshots")
)

lazy val compileSettings = Seq(
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.6", "2.11.7"),
  libraryDependencies ++= Seq(
    "com.chuusai" %%% "shapeless" % "2.3.0-SNAPSHOT",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    "org.scalatest" %%% "scalatest" % "3.0.0-M11" % "test",
    "org.typelevel" %%% "macro-compat" % "1.0.6",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  ),
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <developers>
      <developer>
        <id>alexarchambault</id>
        <name>Alexandre Archambault</name>
        <url>https://github.com/alexarchambault</url>
      </developer>
    </developers>
  },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  credentials += {
    Seq("SONATYPE_USER", "SONATYPE_PASS").map(sys.env.get) match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
      case _ =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

