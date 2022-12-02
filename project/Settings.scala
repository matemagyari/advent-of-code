import sbt.Keys._
import sbt.{Def, _}

object Settings {

  val commonSettings: Seq[Def.Setting[_]] = Seq[Def.Setting[_]](
    organization := "home",
    scalaVersion := "2.13.6"
  )


}
