val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cheatsheet",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-effect" % "3.2.0",
      "org.scalactic" %% "scalactic" % "3.2.9",
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "co.fs2" %% "fs2-core" % "3.2.4", // FS2 is based on CE but we do not need to specify them as direct dependencies since it is already contained in FS2
      "net.java.dev.jna" % "jna" % "5.8.0"
    )
  ).enablePlugins(JmhPlugin)
