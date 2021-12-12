name := "aoc2021"

version := "1.0"

scalaVersion := "2.13.3"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.1" withSources() withJavadoc()
libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.2" withSources()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)
