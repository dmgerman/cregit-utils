name := "blameFollow"
organization := "org.turingmachine"
version := "1.0"

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "5.3.0.201903130848-r"
)

resolvers ++= Seq(
  "jgit-repo" at "http://download.eclipse.org/jgit/maven"
)
