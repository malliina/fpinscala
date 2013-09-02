import sbt._
import sbt.Keys._

/**
 * A scala build file template.
 */
object FpisBuild extends Build {

  import Dependencies._

  lazy val template = Project("fpis", file("."), settings = projectSettings)
    .settings(libraryDependencies ++= Seq(scalaTest))

  lazy val projectSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.2",
    fork in Test := true
  )
}

object Dependencies {
  val scalaTest = "org.scalatest" %% "scalatest" % "1.9.1" % "test"
}