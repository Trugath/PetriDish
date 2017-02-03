name := "petridish"

version := "1.1"

scalaVersion := "2.12.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" changing()

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck_2.12
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.12
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

coverageExcludedPackages := "<empty>;petridish.example.*"