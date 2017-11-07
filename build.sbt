import sbt.Keys.{libraryDependencies, mainClass, scalaVersion}

lazy val plenty = project.
  enablePlugins(ScalaJSPlugin)
  .in(file(".")).
  aggregate(coreJS, coreJVM).
  settings(
    publish := {},
    publishLocal := {},
    scalacOptions ++= Seq("-deprecation", "-feature"),
    scalaVersion := "2.12.3",
    test in assembly := {}
  )

lazy val core = crossProject.in(file(".")).
  settings(
    name := "plenty-core",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.3",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalarx" % "0.3.2",
      "io.circe" %%% "circe-core" % "0.8.0",
      "io.circe" %%% "circe-parser" % "0.8.0",
      "io.circe" %%% "circe-generic" % "0.8.0",
      "com.softwaremill.quicklens" %%% "quicklens" % "1.4.8",
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    mainClass in assembly := Some("fb.FbMain"),
    assemblyJarName in assembly := "plenty.jar",
    test in assembly := {},
    scalacOptions ++= Seq("-deprecation", "-feature")
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
      "javax.mail" % "javax.mail-api" % "1.6.0",
      "com.sun.mail" % "javax.mail" % "1.6.0",
      "com.typesafe.akka" %% "akka-http" % "10.0.10",
      "com.restfb" % "restfb" % "1.47.0"
    ),
    mainClass in assembly := Some("fb.FbMain"),
    assemblyJarName in assembly := "plenty.jar",
    test in assembly := {}
    //    ,
    //    scalacOptions += "-feature"
  ).
  jsSettings(
    libraryDependencies ++= Seq(),
    test in assembly := {}
    //    ,
    //    scalacOptions += "-feature"
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
