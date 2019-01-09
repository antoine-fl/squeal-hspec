# squeal-hspec

Helpers for creating database tests with hspec and squeal, inspired by Jonathan Fischoff's
[hspec-pg-transact](http://hackage.haskell.org/package/hspec-pg-transact).

This uses @tmp-postgres@ to automatically and connect to a temporary instance of postgres on a random port.

Current version is done to operate with Squeal 0.4.0.0.

`describeDB` lets you initate a series of `itDB` specs which will operate in the same context.
It takes migrations to run and a series of fixtures to fill the database.
