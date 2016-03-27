import sbt._

object BuildDef extends Build {
  override lazy val projects = Seq(root)
  lazy val root = Project("petridish", file(".")) dependsOn evolve dependsOn cafebabe
  lazy val evolve = RootProject(uri("https://github.com/Trugath/evolve.git"))
  lazy val cafebabe = RootProject(uri("https://github.com/Trugath/cafebabe.git"))
}