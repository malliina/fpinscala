name := "fpis"

scalaVersion := "2.10.3"

fork in Test := true

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

incOptions := incOptions.value.withNameHashing(true)