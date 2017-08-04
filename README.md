# purescript-baseRationals
Little project to play around with purescript. It reimplements some
functionality of *BaseCalculator* in purescript.

Namely, you can convert between a string and a rational number in
arbitrary basis given arbitrary digits. The main point here is, that you
can also compute non-fractional representations, which are recurring in a
certain basis. A few examples, given standard digits from "0" to "F":

* 4/3 in base 10 -> "0.[3]"
* 1/3 in base 2  -> "0.[01]"
* "0.[1]" in base 3 -> 1/3
* "0.F" in base 16 -> 15/16

Although, recurring numbers can be rendered with `toString`, they can not be
parsed at the moment, thus parsing `"0.[1]"` will fail.

# Installation

```
bower install purescript-baseRationals
```

# Documentation

Documentation is published on [pursuit](pursuit.purescript.org/packages/purescript-baseRationals/).
