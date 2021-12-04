name := "test"

version := "1.0"

scalaVersion := "2.13.3"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.1" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)
