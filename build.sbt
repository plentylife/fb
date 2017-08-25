import sbt.Keys.{libraryDependencies, scalaVersion}

lazy val root = project.
  enablePlugins(ScalaJSPlugin)
  .in(file(".")).
  aggregate(coreJS, coreJVM).
  settings(
    publish := {},
    publishLocal := {},
    scalacOptions ++= Seq("-deprecation", "-feature"),
    scalaVersion := "2.12.3"
  )

lazy val core = crossProject.in(file(".")).
  settings(
    name := "plenty-core",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.3",
    libraryDependencies ++= Seq(
      "io.suzaku" %%% "boopickle" % "1.2.6",
      "com.lihaoyi" %%% "utest" % "0.4.8" % "test",
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
//    ,
//    scalacOptions += "-feature"
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.0.9",
      "com.restfb" % "restfb" % "1.43.0"
    )
//    ,
//    scalacOptions += "-feature"
  ).
  jsSettings(
    libraryDependencies ++= Seq()
//    ,
//    scalacOptions += "-feature"
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
