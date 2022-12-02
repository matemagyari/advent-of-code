import sbt._

object Dependencies {

  val specs2version = "4.12.4"
  val testLibs: Seq[ModuleID] = Seq(
    "org.specs2" %% "specs2-core" % specs2version % "test",
    "org.specs2" %% "specs2-junit" % specs2version % "test",
    "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )
}
