* Lazy vs non-strict: Lazy requires that all evaluations are memoized (permanently in the liftetime of a program).
  Obviously this can have unacceptable implications for memory footprint. 

* Haskell standard is only non-strict. A compliant Haskell implementation might never memoize anything and do no sharing.
  GHC Haskell does sharing sometimes and sometimes not.

* Haskell evaluates expressions outside-in, unlike a strict language. For example:
  `foldr const 'z' ['a'..'e']` not only evalutes to simply `a`, but never recurses, since the second argument to const (the recursive foldr with the rest of the list) is simply unused. In a strict language, the recursive foldr would still be evaluated (forced), even as we'd end with the same eventual result. Similarly, an expression like `foldr const 'z' ['a',undefined] is fine and will still evaluate without bottoming out.

* Contrast to a strict language, you can't do any binding without evaluation (without using some technique like a callback function etc). x = 1+1 will immediately evaluate. The function (+) being call by value would be another way to think about this. Similarly, you can bind bottom (undefined) to a name as long as that name is never forced in a non-strict language like Haskell.

* `seq :: a -> b -> b` returns the second arg but forces evaluation of both args. This is not just (\a -> \b -> b). In other words, it's not just (flip const) in terms of evaluation behavior.

```
seq undefined b = undefined
seq defined b = b
```

* case expressions generally force evaluation to the point where you are doing pattern matching (where it is relevant for your pattern matching)-- this of course makes sense, since you can't match without knowing what you're matching against.:

```
data Test = A Test2 | B Test2 deriving (Show)
data Test2 = C Int | D Int deriving (Show)

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = 1
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = 1

forceTest (A undefined) -- fine, result 1
forceTest undefined -- not fine, bottom out

forceTest2 (A (C 0)) -- fine, result 0
forceTest2 (A (C undefined)) -- not fine, bottom out
forceTest2 (B (C undefined) -- fine, result 1
forceTest2 (B undefined) -- not fine, bottom out
```

* This is fine since we never evaluate y in the expression "x":

```
wc x z = let y = undefined `seq` 'y' in x
foldr wc 'z' ['a'..'e']
```

* seq evaluates your expression to WHNF. That is, it will stop at the first data constructor or lambda. Example:

```
v = (,) undefined undefined -- fine
w = Just undefined -- fine
x = \_ -> undefined -- fine
y = undefined -- fine
z = undefined + undefined -- fine

v `seq` 1 -- fine since (,) is a DC
w `seq` 1 -- fine since Just is a DC
x `seq` 1 -- fine since x is a lambda expression
y `seq` 1 -- not fine since y is just bottom and is being forced
z `seq` 1 -- not fine since (+) is a function application, not a DC or lambda
```
* In other words, `seq` will FORCE YOUR EXPRESSION TO WHNF. So it will stop at any DCs or unapplied lambdas, but fully apply named functions to their arguments.

* GHC Core is a GHC Haskell IR that follows after some desguaring. It can be useful to looking at which expressions are forced and which aren't. The semantics of core are slightly different from the semantics of Haskell in terms of things like forcing case expressions etc.

* In Core, for example, casing on nested data constructors actually desguars to nested case statements, one for each DC in the nesting.

* Haskell to core

```
-- Haskell
discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
  in case x `seq` b of
    False -> 0
    True -> 1
```

```
-- Core
discriminatory =
  \ b_a10D ->
    let {
      x_a10E
      x_a10E = undefined } in
    case
      case x_a10E of _ {
        __DEFAULT -> b_a10D
      } of _ {
        False -> I# 0;
        True -> I# 1
    }
```

* The semantics of case matching in core are stricter than in Haskell. For example, in Haskell:

```
case undefined of { _ -> False } -- legal Haskell expression that gets desugared in Core to just "False" (with no case)
case undefined of { DEFAULT -> False } -- in Core, this will bottom out since undefined will be forced.
-- that is to say, in Core, whatever is being cased is strictly evaluated to WHNF immediately, no matter what is being matched. 
-- In Haskell, only what needs to be MATCHED is evaluated to WHNF.
```

* Call by name: Expressions don't have to be evaluated when passed as arguments to a function. Bindings without evaluation. Nonstrict strategy.

* Call by need: Same as call-by-name, but with memomization. GHC Haskell uses this sometimes, usually when an expression isn't a lambda that takes arguments and also has a name. So something like "f = \a -> 1 + 1" will be call by need, where f a = 1 + 1 will be call by name.

* Due to that^, pointfree functions that have been evaluated are generally shared.

* GHC won't delay evaluation of data constants or constructors that are fully applied (constructors like `1`, `False`, or `2 : []`).
  It's important to note that anything with a typeclass constraint is not fully applied. So, the expression x = Just 5 will not be
  forced, but the expression x = Just (5 :: Int) will be forced. The former is x = Just (5 :: Num a => a). Num a => a desugars to
  something like Num a -> a in Core. Only data constants and fully applied data constructors are opportunistically forced by GHC. Even
  fully applied functions are NOT opportunistically forced:

```
xs = [1,2,3] :: [Integer]
xs' = xs ++ undefined
:spr xs -- [1,2,3]
:spr xs' -- _ 
```

* Names promote sharing as a general rule of thumb. Inlining expressions subverts sharing

* Functions with explicit named arguments (NOT pointfree functions) are generally not shared (memoized) by GHC Haskell.

* Implicit parameters == dynamic scoping (? this point wasn't that clear but either way they prevent sharing similarly to typeclass constraints

* Typeclass constraints prevent sharing due to the earlier note that they desugar to functions in Core. Ex:

```
blah = Just 1
fmap ((+1) :: Int -> Int) blah -- Just 2
:spr blah -- _ NOT MEMOIZED, STILL A THUNK

blahh = Just (trace "Just eval'd 1" (1 :: Int))
fm = fmap ((+1) :: Int -> Int) blahh
fm -- trace printout and 2
:sprint fm' -- Just 2 shared
```
* Re-evaluating (not memoizing/sharing) due to typeclass constraint `Just (1 :: Num a => a)`
```
Prelude> :{
Prelude| let blah =
Prelude|
Just (trace "eval'd 1" 1)
Prelude| :}
Prelude> :sprint blah
blah = _
Prelude> :t blah
blah :: Num a => Maybe a
Prelude> fmap (+1) blah
Just eval'd 1
2
Prelude> fmap (+1) blah
Just eval'd 1
2
Prelude> :sprint blah
blah = _
```

* Applying the "typeclass constraint function" (as it would be in Core) will memoize or share. Ex:

```
poly = 1
conc = poly :: int
:spr poly -- poly = _
:spr conc -- conc = _
poly -- 1
conc -- 1
:spr poly -- poly = _
:spr conc -- conc = 1 -- now conc has been shared since poly was beta reduced to 1, and conc is now forced to 1 :: Int
```

* Another example of polymorphic values not being shared
```
blah :: Int -> Int
blah x = x + 1

woot = blah 1

:spr blah -- blah = _
:spr woot -- woot = _
woot -- 2
:spr woot -- woot = 2 -- shared

blah :: Num a => a -> a
blah x = x + 1

woot = blah 1
:spr blah -- blah = _
:spr woot -- woot = _
woot -- 2
:spr woot -- woot = _ -- not shared since "blah 1" is now a polymorphic expression until woot is evaluated and 1 is type-defaulted to Int on eval
```

* Again, any named arguments, even ignored as in `\_ -> 2` will disable or subvert sharing. Pointfree functions promote sharing.

* An irrefutable pattern is one that will match against any possible inhabitant of the type you're matching.

* Lazy patterns are tilde-prefix and irrefutable:

```
strictPattern :: (a, b) -> String
strictPattern (a,b) = const "Cousin It" a

lazyPattern :: (a, b) -> String
lazyPattern ~(a,b) = const "Cousin It" a

strictPattern undefined -- bottom, crash
lazyPattern undefined -- "Cousin It" 
```
* ^In the lazy pattern, we don't force evaluation until we need to use b. That is, never, in this case. You cannot use lazy patterns to discriminate between members of a sum type. It is useful if you have a product type that might not get used and you just need a piece of it.

* Bang patterns can be used to force evaluation of a function argument whether or not it's used. This is pretty much just alternative syntax for seq:

```
banging :: Bool -> Int
banging !b = 1 -- b will be forced when banging is applied
banging undefined -- crash
```

* Bang patterns can be used in data constructors as well:

```
data BadShot = ShotMissed Int String
data BangBang = SheShotMeDown !Int !String
gimmeString (SheShotMeDown _ s) = s
gimmeString' (ShotMissed _ s) = s

gimmeString' (ShotMissed undefined "blah" -- "blah"
gimmeString (SheShotMeDown undefined "blah") -- crash
```

* In GHC 8.0 or newer, the Strict and StrictData pragmas force strictness where possible and avoid littering bang patterns in a module. This will not changed the behavior lazy data structures or functions defined in other modules. In this case, you can still use tilde to create lazy irrefutable patterns and recover laziness.
