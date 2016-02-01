  
scalaVersion := "2.10.6"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats" % "0.3.0",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" %% "scalaz-core" % "7.2.0"
)

scalacOptions ++= Seq(
  "-unchecked", 
  "-deprecation", 
  "-feature", 
  // "-Xprint:typer", 
  // "-Xlog-implicit-conversions",
  "-language:higherKinds")
