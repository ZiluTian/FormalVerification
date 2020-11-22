scalaVersion := "2.13.3"
name := "ZAMSAT"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

mainClass in (Compile, packageBin) := Some("zamsat.Benchmark")
