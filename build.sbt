name := "mtest"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalaz" %% "scalaz-zio" % "0.1.0-SNAPSHOT"
libraryDependencies += "org.scalaz" %% "scalaz-zio-interop" % "0.1.0-SNAPSHOT"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"
resolvers += Opts.resolver.sonatypeSnapshots
resolvers += Opts.resolver.sonatypeReleases

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
