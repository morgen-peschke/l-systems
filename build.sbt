name := "l-system renderer"

version := "1.0.0"
scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-adapted-args",
  "-Ywarn-inaccessible",
  "-Ywarn-unused",
  "-Ywarn-dead-code",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Ypartial-unification",
  "-Xfatal-warnings")

val catsVersion = "1.4.0"
val monocleVersion = "1.5.0"

val enumeratumVersion = "1.5.13"
libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "com.typesafe" % "config" % "1.4.0")
