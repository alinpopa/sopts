import sbt._

class SoptsProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalaTestRepo = "ScalaTest Repository" at "http://www.scala-tools.org/repo-releases"

  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val mockito = "org.mockito" % "mockito-all" % "1.8.5" % "test" withSources()
}

