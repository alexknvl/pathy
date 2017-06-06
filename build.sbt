val commonSettings = List(
  addCompilerPlugin(Versions.kindProjector),
  organization := "com.alexknvl",
  version      := "0.0.2",
  licenses     += ("Apache-2.0", url("http://www.apache.org/licenses/")),
  scalaVersion := "2.12.2",
  scalacOptions ++= List(
    "-deprecation", "-unchecked", "-feature",
    "-encoding", "UTF-8",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Xfuture"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")))

lazy val root = (project in file("."))
  .settings(name := "pathy")
  .settings(commonSettings: _*)
  .settings(libraryDependencies
    ++= Versions.cats)
