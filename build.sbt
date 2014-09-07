lazy val root = (project in file(".")).settings(
  scalaVersion := "2.11.2",
  name := "fp-in-scala",
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4.2"
  )
)
