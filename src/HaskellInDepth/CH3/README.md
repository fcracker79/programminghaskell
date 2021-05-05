Exercises
---------

1. Convert `StatValue` into a `Fixed` type.
2. `field2fun` always returns `Double`. Try to make it return the right type.


Notes
-----

It is possible to use RecordWildcard extension to create a structure, such as:

```

:set -XRecordWildCards

data Data = Data {f1:: Int, f2:: Int} deriving Show
:{
mydata = let f1 = 1
             f2 = 2
             whatever = 3
             in Data {..}
:}
```