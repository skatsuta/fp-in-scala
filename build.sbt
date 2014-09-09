val scalazVersion = "7.1.0"

lazy val root = (project in file(".")).settings(
  scalaVersion := "2.11.2",
  name := "fp-in-scala",
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4.2",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
  )
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"
