import sbt.Keys.libraryDependencies

lazy val root = project.
  enablePlugins(ScalaJSPlugin)
  .in(file(".")).
  aggregate(coreJS, coreJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val core = crossProject.in(file(".")).
  settings(
    name := "plenty-core",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.3",
    libraryDependencies ++= Seq(
      "io.suzaku" %%% "boopickle" % "1.2.6",
      "com.lihaoyi" %%% "utest" % "0.4.8" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
    )
  ).
  jsSettings(
    libraryDependencies ++= Seq(
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js