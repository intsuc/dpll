lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.0.1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.28" % Test
  )
