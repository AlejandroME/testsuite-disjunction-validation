name := """disjunction"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.4" % "test",
                            "org.scalaz" %% "scalaz-core" % "7.1.4",
                            "joda-time" % "joda-time" % "2.9")