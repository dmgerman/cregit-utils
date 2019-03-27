name := "cregitblame"
organization := "org.turingmachine"
version := "1.0"

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.6.0.201612231935-r"
)

resolvers ++= Seq(
  "SQLite-JDBC Repository" at "https://oss.sonatype.org/content/repositories/snapshots",
  "jgit-repo" at "http://download.eclipse.org/jgit/maven"
)
