# purescript-baseRationals
Little project to play around with purescript. It reimplements some
functionality of *BaseCalculator* in purescript.

Namely, you can convert between a string and a rational number in
arbitrary basis given arbitrary digits. The main point here is, that you
can also compute non-fractional representations, which are recurring in a
certain basis. Eg.:

```
let digits = [ '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']`:
```
```
> toString digits 10 (1 % 3)
Right "0.[3]"
```
```
> toString digits 2 (1 % 3)
Right "0.[01]"
```
```
> fromString digits 3 "0.1"
Right (1 % 3)
```
```
> fromString digits 16 "0.F"
Right (15 % 16)
```

Although, recurring numbers can be rendered with `toString`, they can not be
parsed at the moment, thus `fromString digits 10 "0.[3]"` will fail.

# Installation

```
bower install purescript-baseRationals
```

# Documentation

Documentation is published on [pursuit](pursuit.purescript.org/packages/purescript-baseRationals/).
