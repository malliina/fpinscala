name := "fpis"

scalaVersion := "2.11.4"

fork in Test := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"

incOptions := incOptions.value.withNameHashing(true)