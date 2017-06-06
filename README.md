# pathy

A type-safe abstraction for platform-independent file system paths.

Ported from [purescript-pathy](slamengine/purescript-pathy).
Ada(o)pted from [slamdata/scala-pathy](https://github.com/slamdata/scala-pathy) to Cats.

## Example

```scala
val fullPath = rootDir </> dir("baz") </> file("foo.png")
```

## Getting Started

### Installation

pathy is built against Scala 2.12.x.

If you're using SBT, add the following to your build file

```scala
resolvers += Resolver.bintrayRepo("alexknvl", "maven")
libraryDependencies += "com.alexknvl" %% "pathy" % "0.0.2"
```

### Usage

The following imports will bring all types and operations into scope

```scala
import pathy._
````
