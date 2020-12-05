
name := "wardrobe"
version := "0.1"

scalaVersion := "2.13.4"

resolvers ++= Seq(
  "Artima Maven Repository" at "https://repo.artima.com/releases"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test
)
