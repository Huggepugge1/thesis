val scala3Version = "3.8.3"

lazy val boc = project
  .in(file("."))
  .settings(
    name := "boc",
    version := "0.1.0-SNAPSHOT",

    Test / parallelExecution := false,
    Global / concurrentRestrictions := Seq(
      Tags.limit(Tags.Test, 2)
    ),

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.2.4" % Test,
    scalacOptions ++= Seq(
      // "-Vprint:cc",
      // "-Ycc-verbose",
      // "-Ycc-debug",
      // "-explain",
      // "-Djava.util.concurrent.ForkJoinPool.common.parallelism=1"
    ),
  )
