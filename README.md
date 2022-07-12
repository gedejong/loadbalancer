# Load Balancer with State Monad.

Implementation of the Load Balancer exercise for IpTiq.

## How to build?

[Install](https://www.scala-sbt.org/1.x/docs/Setup.html) the Scala Build Tool.

Build using:

```sbt compile```

and test using:

```sbt test```

## What is a State Monad?

A state monad encode changes in state. This makes systems more testable and easier to
compose. This implementation uses a very minimal implementation, but is still
fully functional.

## Why not use traditional OOP technologies?

That would work as well, although in highly concurrent systems working without
specific state makes it more difficult to reason about correctness. The
ZIO libraries use the `Ref` Monad as its primary structure to communicate
about state. 

In addition, using the State monad we can automatically generate programs
and test them for correctness using property based testing with libraries 
such as [ScalaCheck](https://scalacheck.org/)