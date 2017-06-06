import sbt._

object Versions {
  val kindProjector : ModuleID       =      "org.spire-math"       %% "kind-projector" % "0.9.3"
  val scalacheck    : List[ModuleID] = List("org.scalacheck"       %% "scalacheck"     % "1.13.5" % "test")
  val discipline    : List[ModuleID] = List("org.typelevel"        %% "discipline"     % "0.7.3"  % "test")
  val scalatest     : List[ModuleID] = List("org.scalatest"        %% "scalatest"      % "3.0.3"  % "test")
  val testing       : List[ModuleID] = scalacheck ++ scalatest ++ discipline

  val cats       : List[ModuleID] = List("org.typelevel"        %% "cats"       % "0.9.0")
}