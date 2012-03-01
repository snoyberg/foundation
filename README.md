An alternate Prelude. Unique features:

* Does not replace any of the existing type classes. Because of this, using
  Foundation does not introduce a high overhead.

* Places as many functions as possible in type classes. This means functions
  like `takeWhile` work for normal lists, `Text`, and `ByteString`.

* Promotes modern best practices, e.g. use `Text` for textual data.
