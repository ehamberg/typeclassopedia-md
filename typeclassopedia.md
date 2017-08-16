% Typeclassopedia
% Brent Yorgey

Originally published 12 March 2009 in [issue 13](http://www.haskell.org/wikiupload/8/85/TMR-Issue13.pdf) of [the Monad.Reader](http://themonadreader.wordpress.com/). Ported to the Haskell wiki in November 2011 by Geheimdienst. Converted to Markdown in August 2013 by Erlend Hamberg. Converted to EPUB format with [pandoc](http://www.pandoc.org).

*This is not the official version of the Typeclassopedia. The official version is now the [Haskell wiki version](http://www.haskell.org/haskellwiki/Talk:Typeclassopedia) which supersedes the version published in the Monad.Reader.  Please help update and extend it by editing it yourself or by leaving comments, suggestions, and questions on the [talk page](http://www.haskell.org/haskellwiki/Talk:Typeclassopedia).*

# Abstract

The standard Haskell libraries feature a number of type classes with algebraic or category-theoretic underpinnings. Becoming a fluent Haskell hacker requires intimate familiarity with them all, yet acquiring this familiarity often involves combing through a mountain of tutorials, blog posts, mailing list archives, and IRC logs.

The goal of this document is to serve as a starting point for the student of Haskell wishing to gain a firm grasp of its standard type classes. The essentials of each type class are introduced, with examples, commentary, and extensive references for further reading.

# Introduction

Have you ever had any of the following thoughts?

* What the heck is a monoid, and how is it different from a mon<u>a</u>d?

* I finally figured out how to use [Parsec](http://www.haskell.org/haskellwiki/Parsec) with do-notation, and someone told me I should use something called `Applicative` instead. Um, what?

* Someone in the [#haskell](http://www.haskell.org/haskellwiki/IRC channel) IRC channel used `(***)`, and when I asked Lambdabot to tell me its type, it printed out scary gobbledygook that didn’t even fit on one line! Then someone used `fmap fmap fmap` and my brain exploded.

* When I asked how to do something I thought was really complicated, people started typing things like `zip.ap fmap.(id &&& wtf)` and the scary thing is that they worked! Anyway, I think those people must actually be robots because there’s no way anyone could come up with that in two seconds off the top of their head.

If you have, look no further! You, too, can write and understand concise, elegant, idiomatic Haskell code with the best of them.

There are two keys to an expert Haskell hacker’s wisdom:

#. Understand the types.
#. Gain a deep intuition for each type class and its relationship to other type classes, backed up by familiarity with many examples.

It’s impossible to overstate the importance of the first; the patient student of type signatures will uncover many profound secrets. Conversely, anyone ignorant of the types in their code is doomed to eternal uncertainty. “Hmm, it doesn’t compile ... maybe I’ll stick in an
`fmap` here ... nope, let’s see ... maybe I need another `(.)` somewhere? ... um ...”

The second key—gaining deep intuition, backed by examples—is also important, but much more difficult to attain. A primary goal of this document is to set you on the road to gaining such intuition. However—

> There is no royal road to Haskell. —Euclid^[Well, he probably would have said it if he knew Haskell.]

This document can only be a starting point, since good intuition comes from hard work, [not from learning the right metaphor](http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/). Anyone who reads and understands all of it will still have an arduous journey ahead—but sometimes a good starting point makes a big difference.

It should be noted that this is not a Haskell tutorial; it is assumed that the reader is already familiar with the basics of Haskell, including the standard [`Prelude`](https://hackage.haskell.org/package/base/docs/Prelude.html), the type system, data types, and type classes.

The type classes we will be discussing and their interrelationships ([source code for this graph can be found here](http://www.haskell.org/haskellwiki/:File:Dependencies.txt)):

![](Typeclassopedia-diagram.svg)

* <span style="border-bottom: 2px solid black">Solid arrows</span> point from the general to the specific; that is, if there is an arrow from `Foo` to `Bar` it means that every `Bar` is (or should be, or can be made into) a `Foo`.
* <span style="border-bottom: 2px dotted black">Dotted lines</span> indicate some other sort of relationship.
* `Monad` and `ArrowApply` are equivalent.
* `Apply` and `Comonad` are greyed out since they are not actually (yet?) in the standard Haskell libraries ^[`Apply` can be found in the [`semigroupoids` package](http://hackage.haskell.org/package/semigroupoids), and `Comonad` in the [`comonad` package](http://hackage.haskell.org/package/comonad).].

One more note before we begin. The original spelling of “type class” is with two words, as evidenced by, for example, the [Haskell 2010 Language Report](http://www.haskell.org/onlinereport/haskell2010/), early papers on type classes like [Type classes in Haskell](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.103.5639) and [Type classes: exploring the design space](http://research.microsoft.com/en-us/um/people/simonpj/papers/type-class-design-space/), and [Hudak et al.’s history of Haskell](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.168.4008).  However, as often happens with two-word phrases that see a lot of use, it has started to show up as one word (“typeclass”) or, rarely, hyphenated (“type-class”).  When wearing my prescriptivist hat, I prefer “type class”, but realize (after changing into my descriptivist hat) that there's probably not much I can do about it.

We now begin with the simplest type class of all: `Functor`.

# Functor

The `Functor` class ([haddock](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Functor)) is the most basic and ubiquitous type class in the Haskell libraries. A simple intuition is that a `Functor` represents a “container” of some sort, along with the ability to apply a function uniformly to every element in the container. For example, a list is a container of elements, and we can apply a function to every element of a list, using `map`. As another example, a binary tree is also a container of elements, and it’s not hard to come up with a way to recursively apply a function to every element in a tree.

Another intuition is that a `Functor` represents some sort of “computational context”. This intuition is generally more useful, but is more difficult to explain, precisely because it is so general. Some examples later should help to clarify the `Functor`-as-context point of view.

In the end, however, a `Functor` is simply what it is defined to be; doubtless there are many examples of `Functor` instances that don’t exactly fit either of the above intuitions. The wise student will focus their attention on definitions and examples, without leaning too heavily on any particular metaphor. Intuition will come, in time, on its own.

## Definition

Here is the type class declaration for `Functor`:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: a        -> f b -> f a
  (<$) = fmap . const
```

`Functor` is exported by the `Prelude`, so no special imports are needed to use it.  Note that the `(<$)` operator is provided for convenience, with a default implementation in terms of `fmap`; it is included in the class just to give `Functor` instances the opportunity to provide a more efficient implementation than the default.  To understand `Functor`, then, we really need to understand `fmap`.

First, the `f a` and `f b` in the type signature for `fmap` tell us that `f` isn’t a concrete type like `Int`; it is a sort of *type function* which takes another type as a parameter. More precisely, the *kind* of `f` must be `* -> *`. For example, `Maybe` is such a type with kind `* -> *`: `Maybe` is not a concrete type by itself (that is, there are no values of type `Maybe`), but requires another type as a parameter, like `Maybe Integer`. So it would not make sense to say `instance Functor Integer`, but it could make sense to say `instance Functor Maybe`.

Now look at the type of `fmap`: it takes any function from `a` to `b`, and a value of type `f a`, and outputs a value of type `f b`. From the container point of view, the intention is that `fmap` applies a function to each element of a container, without altering the structure of the container. From the context point of view, the intention is that `fmap` applies a function to a value without altering its context. Let’s look at a few specific examples.

Finally, we can understand `(<$)`: instead of applying a function to the values a container/context, it simply replaces them with a given value.  This is the same as applying a constant function, so `(<$)` can be implemented in terms of `fmap`.

## Instances

As noted before, the list constructor `[]` is a functor ^[Recall that `[]` has two meanings in Haskell: it can either stand for the empty list, or, as here, it can represent the list type constructor (pronounced “list-of”). In other words, the type `[a]` (list-of-`a`) can also be written `[] a`.]; we can use the standard list function `map` to apply a function to each element of a list ^[You might ask why we need a separate `map` function. Why not just do away with the current list-only `map` function, and rename `fmap` to `map` instead? Well, that’s a good question. The usual argument is that someone just learning Haskell, when using `map` incorrectly, would much rather see an error about lists than about `Functor`s.]. The `Maybe` type constructor is also a functor, representing a container which might hold a single element. The function `fmap g` has no effect on `Nothing` (there are no elements to which `g` can be applied), and simply applies `g` to the single element inside a `Just`. Alternatively, under the context interpretation, the list functor represents a context of nondeterministic choice; that is, a list can be thought of as representing a single value which is nondeterministically chosen from among several possibilities (the elements of the list). Likewise, the `Maybe` functor represents a context with possible failure. These instances are:

```haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ []     = []
  fmap g (x:xs) = g x : fmap g xs
  -- or we could just say fmap = map

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap g (Just a) = Just (g a)
```

As an aside, in idiomatic Haskell code you will often see the letter `f` used to stand for both an arbitrary `Functor` and an arbitrary function. In this document, `f` represents only `Functor`s, and `g` or `h` always represent functions, but you should be aware of the potential confusion. In practice, what `f` stands for should always be clear from the context, by noting whether it is part of a type or part of the code.

There are other `Functor` instances in the standard library as well:

* `Either e` is an instance of `Functor`; `Either e a` represents a container which can contain either a value of type `a`, or a value of type `e` (often representing some sort of error condition). It is similar to `Maybe` in that it represents possible failure, but it can carry some extra information about the failure as well.

* `((,) e)` represents a container which holds an “annotation” of type `e` along with the actual value it holds. It might be clearer to write it as `(e,)`, by analogy with an operator section like `(1+)`, but that syntax is not allowed in types (although it is allowed in expressions with the `TupleSections` extension enabled). However, you can certainly *think* of it as `(e,)`.

* `((->) e)` (which can be thought of as `(e ->)`; see above), the type of functions which take a value of type `e` as a parameter, is a `Functor`.  As a container, `(e -> a)` represents a (possibly infinite) set of values of `a`, indexed by values of `e`. Alternatively, and more usefully, `((->) e)` can be thought of as a context in which a value of type `e` is available to be consulted in a read-only fashion. This is also why `((->) e)` is sometimes referred to as the *reader monad*; more on this later.

* `IO` is a `Functor`; a value of type `IO a` represents a computation producing a value of type `a` which may have I/O effects. If `m` computes the value `x` while producing some I/O effects, then `fmap g m` will compute the value `g x` while producing the same I/O effects.

* Many standard types from the [containers library](http://hackage.haskell.org/package/containers/) (such as `Tree`, `Map`, and `Sequence`) are instances of `Functor`. A notable exception is `Set`, which cannot be made a `Functor` in Haskell (although it is certainly a mathematical functor) since it requires an `Ord` constraint on its elements; `fmap` must be applicable to *any* types `a` and `b`. However, `Set` (and other similarly restricted data types) can be made an instance of a suitable generalization of `Functor`, either by [making `a` and `b` arguments to the `Functor` type class themselves](http://article.gmane.org/gmane.comp.lang.haskell.cafe/78052/), or by adding an [associated constraint](http://blog.omega-prime.co.uk/?p=127).

### Exercises

#. Implement `Functor` instances for `Either e` and `((->) e)`.

#. Implement `Functor` instances for `((,) e)` and for `Pair`, defined as

    ```haskell
    data Pair a = Pair a a
    ```

    Explain their similarities and differences.

#. Implement a `Functor` instance for the type `ITree`, defined as

    ```haskell
    data ITree a = Leaf (Int -> a)
                | Node [ITree a]
    ```
#. Give an example of a type of kind `* -> *` which cannot be made an instance of `Functor` (without using `undefined`).

#. Is this statement true or false?

  > *The composition of two `Functor`s is also a `Functor`.*

  If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

## Laws

As far as the Haskell language itself is concerned, the only requirement to be a `Functor` is an implementation of `fmap` with the proper type. Any sensible `Functor` instance, however, will also satisfy the *functor laws*, which are part of the definition of a mathematical functor. There are two:

```haskell
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)
```

Together, these laws ensure that `fmap g` does not change the *structure* of a container, only the elements. Equivalently, and more simply, they ensure that `fmap g` changes a value without altering its context^[Technically, these laws make `f` and `fmap` together an endofunctor on *Hask*, the category of Haskell types (ignoring [$\bot$](http://www.haskell.org/haskellwiki/Bottom), which is a party pooper). See [Wikibook: Category theory](http://en.wikibooks.org/wiki/Haskell/Category_theory).].

The first law says that mapping the identity function over every item in a container has no effect. The second says that mapping a composition of two functions over every item in a container is the same as first mapping one function, and then mapping the other.

As an example, the following code is a “valid” instance of `Functor` (it typechecks), but it violates the functor laws. Do you see why?

```haskell
-- Evil Functor instance
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x:xs) = g x : g x : fmap g xs
```

Any Haskeller worth their salt would reject this code as a gruesome abomination.

Unlike some other type classes we will encounter, a given type has at most one valid instance of `Functor`. This [can be proven](http://article.gmane.org/gmane.comp.lang.haskell.libraries/15384) via the [*free theorem*](http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html#free) for the type of `fmap`.  In fact, [GHC can automatically derive](http://byorgey.wordpress.com/2010/03/03/deriving-pleasure-from-ghc-6-12-1/) `Functor` instances for many data types.



A [similar argument also shows](https://github.com/quchen/articles/blob/master/second_functor_law.md) that any `Functor` instance satisfying the first law (`fmap id = id`) will automatically satisfy the second law as well.  Practically, this means that only the first law needs to be checked (usually by a very straightforward induction) to ensure that a `Functor` instance is valid.^[Actually, if `seq`/`undefined` are considered, it [is possible](http://stackoverflow.com/a/8323243/305559) to have an implementation which satisfies the first law but not the second.  The rest of the comments in this section should be considered in a context where `seq` and `undefined` are excluded.]

### Exercises

#. Although it is not possible for a `Functor` instance to satisfy the first `Functor` law but not the second (excluding `undefined`), the reverse is possible. Give an example of a (bogus) `Functor` instance which satisfies the second law but not the first.
#. Which laws are violated by the evil `Functor` instance for list shown above: both laws, or the first law alone? Give specific counterexamples.

## Intuition

There are two fundamental ways to think about `fmap`. The first has already been mentioned: it takes two parameters, a function and a container, and applies the function “inside” the container, producing a new container. Alternately, we can think of `fmap` as applying a function to a value in a context (without altering the context).

Just like all other Haskell functions of “more than one parameter”, however, `fmap` is actually *curried*: it does not really take two parameters, but takes a single parameter and returns a function. For emphasis, we can write `fmap`’s type with extra parentheses: `fmap :: (a -> b) -> (f a -> f b)`. Written in this form, it is apparent that `fmap` transforms a “normal” function (`g :: a -> b`) into one which operates over containers/contexts (`fmap g :: f a -> f b`). This transformation is often referred to as a *lift*; `fmap` “lifts” a function from the “normal world” into the “`f` world”.

## Utility functions

There are a few more `Functor`-related functions which can be imported from the `Data.Functor` module.

* `(<$>)` is defined as a synonym for `fmap`.  This enables a nice infix style that mirrors the `($)` operator for function application. For example, `f $ 3` applies the function `f` to 3, whereas `f <$> [1,2,3]` applies `f` to each member of the list.
* `($>) :: Functor f => f a -> b -> f b` is just `flip (<$)`, and can occasionally be useful.  To keep them straight, you can remember that `(<$)` and `($>)` point towards the value that will be kept.
* `void :: Functor f => f a -> f ()` is a specialization of `(<$)`, that is, `void x = () <$ x`.  This can be used in cases where a computation computes some value but the value should be ignored.

## Further reading

A good starting point for reading about the category theory behind the concept of a functor is the excellent [Haskell wikibook page on category theory](http://en.wikibooks.org/wiki/Haskell/Category_theory).

# Applicative

A somewhat newer addition to the pantheon of standard Haskell type classes, *applicative functors* represent an abstraction lying in between `Functor` and `Monad` in expressivity, first described by McBride and Paterson. The title of their classic paper, [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html), gives a hint at the intended intuition behind the [`Applicative`](https://hackage.haskell.org/package/base/docs/Control-Applicative.html) type class. It encapsulates certain sorts of “effectful” computations in a functionally pure way, and encourages an “applicative” programming style. Exactly what these things mean will be seen later.

## Definition

Recall that `Functor` allows us to lift a “normal” function to a function on computational contexts. But `fmap` doesn’t allow us to apply a function which is itself in a context to a value in a context. `Applicative` gives us just such a tool, `(<*>)` (variously pronounced as "apply", "app", or "splat").  It also provides a method, `pure`, for embedding values in a default, “effect free” context.  Here is the type class declaration for `Applicative`, as defined in `Control.Applicative`:

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  infixl 4 <*>, *>, <*
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const
```

Note that every `Applicative` must also be a `Functor`. In fact, as we will see, `fmap` can be implemented using the `Applicative` methods, so every `Applicative` is a functor whether we like it or not; the `Functor` constraint forces us to be honest.

`(*>)` and `(<*)` are provided for convenience, in case a particular instance of `Applicative` can provide more efficient implementations, but they are provided with default implementations.  For more on these operators, see the section on [Utility functions](http://www.haskell.org/haskellwiki/#Utility functions) below.

As always, it’s crucial to understand the type signatures.  First, consider `(<*>)`: the best way of thinking about it comes from noting that the type of `(<*>)` is similar to the type of `($)` ^[Recall that `($)` is just function application: `f $ x = f x`], but with everything enclosed in an `f`. In other words, `(<*>)` is just function application within a computational context. The type of `(<*>)` is also very similar to the type of `fmap`; the only difference is that the first parameter is `f (a -> b)`, a function in a context, instead of a “normal” function `(a -> b)`.

`pure` takes a value of any type `a`, and returns a context/container of type `f a`.  The intention is that `pure` creates some sort of “default” container or “effect free” context.  In fact, the behavior of `pure` is quite constrained by the laws it should satisfy in conjunction with `(<*>)`.  Usually, for a given implementation of `(<*>)` there is only one possible implementation of `pure`.

(Note that previous versions of the Typeclassopedia explained `pure` in terms of a type class `Pointed`, which can still be found in the [`pointed` package](http://hackage.haskell.org/package/pointed).  However, the current consensus is that `Pointed` is not very useful after all.  For a more detailed explanation, see [Why not Pointed?](http://www.haskell.org/haskellwiki/Why not Pointed?))

## Laws

Traditionally, there are four laws that `Applicative` instances should satisfy^[See [haddock for Applicative](https://hackage.haskell.org/package/base/docs/Control-Applicative.html) and [Applicative programming with effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html)].  In some sense, they are all concerned with making sure that `pure` deserves its name:

* The identity law:

    ```haskell
    pure id <*> v = v
    ```

* Homomorphism:

    ```haskell
    pure f <*> pure x = pure (f x)
    ```

    Intuitively, applying a non-effectful function to a non-effectful argument in an effectful context is the same as just applying the function to the argument and then injecting the result into the context with `pure`{.haskell}.

* Interchange:

    ```haskell
    u <*> pure y = pure ($ y) <*> u
    ```

    Intuitively, this says that when evaluating the application of an effectful function to a pure argument, the order in which we evaluate the function and its argument doesn't matter.

* Composition:

    ```haskell
    u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
    ```

    This one is the trickiest law to gain intuition for.  In some sense it is expressing a sort of associativity property of `(<*>)`{.haskell}.  The reader may wish to simply convince themselves  that this law is type-correct.

Considered as left-to-right rewrite rules, the homomorphism, interchange, and composition laws actually constitute an algorithm for transforming any expression using `pure` and `(<*>)` into a canonical form with only a single use of `pure` at the very beginning and only left-nested occurrences of `(<*>)`.  Composition allows reassociating `(<*>)`; interchange allows moving occurrences of `pure` leftwards; and homomorphism allows collapsing multiple adjacent occurrences of `pure` into one.

There is also a law specifying how `Applicative` should relate to `Functor`:

```haskell
fmap g x = pure g <*> x
```

It says that mapping a pure function `g` over a context `x` is the same as first injecting `g` into a context with `pure`, and then applying it to `x` with `(<*>)`. In other words, we can decompose `fmap` into two more atomic operations: injection into a context, and application within a context. Since `(<$>)` is a synonym for `fmap`, the above law can also be expressed as:

`g <$> x = pure g <*> x`.

### Exercises

- (Tricky) One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument.  Using the above laws, prove that `pure f <*> x = pure (flip ($)) <*> x <*> pure f`{.haskell}

## Instances

Most of the standard types which are instances of `Functor` are also instances of `Applicative`.

`Maybe` can easily be made an instance of `Applicative`; writing such an instance is left as an exercise for the reader.

The list type constructor `[]` can actually be made an instance of `Applicative` in two ways; essentially, it comes down to whether we want to think of lists as ordered collections of elements, or as contexts representing multiple results of a nondeterministic computation (see Wadler’s [How to replace failure by a list of successes](http://www.springerlink.com/content/y7450255v2670167/)).

Let’s first consider the collection point of view.  Since there can only be one instance of a given type class for any particular type, one or both of the list instances of `Applicative` need to be defined for a `newtype` wrapper; as it happens, the nondeterministic computation instance is the default, and the collection instance is defined in terms of a `newtype` called `ZipList`. This instance is:

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure = undefined   -- exercise

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
```

To apply a list of functions to a list of inputs with `(<*>)`, we just match up the functions and inputs elementwise, and produce a list of the resulting outputs. In other words, we “zip” the lists together with function application, `($)`; hence the name `ZipList`.

The other `Applicative` instance for lists, based on the nondeterministic computation point of view, is:

```haskell
instance Applicative [] where
  pure :: a -> [a]
  pure x    = [x]

  (<*>) :: [a -> b] -> [a] -> [b]
  gs <*> xs = [ g x | g <- gs, x <- xs ]
```

Instead of applying functions to inputs pairwise, we apply each function to all the inputs in turn, and collect all the results in a list.

Now we can write nondeterministic computations in a natural style. To add the numbers `3` and `4` deterministically, we can of course write `(+) 3 4`. But suppose instead of `3` we have a nondeterministic computation that might result in `2`, `3`, or `4`; then we can write

```haskell
  pure (+) <*> [2,3,4] <*> pure 4
```

or, more idiomatically,

```haskell
  (+) <$> [2,3,4] <*> pure 4.
```

There are several other `Applicative` instances as well:

* `IO` is an instance of `Applicative`, and behaves exactly as you would think: to execute `m1 <*> m2`, first `m1` is executed, resulting in a function `f`, then `m2` is executed, resulting in a value `x`, and finally the value `f x` is returned as the result of executing `m1 <*> m2`.

* `((,) a)` is an `Applicative`, as long as `a` is an instance of `Monoid` ([section Monoid](http://www.haskell.org/haskellwiki/#Monoid)). The `a` values are accumulated in parallel with the computation.

* The `Applicative` module defines the `Const` type constructor; a value of type `Const a b` simply contains an `a`. This is an instance of `Applicative` for any `Monoid a`; this instance becomes especially useful in conjunction with things like `Foldable` ([section Foldable](http://www.haskell.org/haskellwiki/#Foldable)).

* The `WrappedMonad` and `WrappedArrow` newtypes make any instances of `Monad` ([section Monad](http://www.haskell.org/haskellwiki/#Monad)) or `Arrow` ([section Arrow](http://www.haskell.org/haskellwiki/#Arrow)) respectively into instances of `Applicative`; as we will see when we study those type classes, both are strictly more expressive than `Applicative`, in the sense that the `Applicative` methods can be implemented in terms of their methods.

### Exercises

#. Implement an instance of `Applicative` for `Maybe`.
#. Determine the correct definition of `pure` for the `ZipList` instance of `Applicative`—there is only one implementation that satisfies the law relating `pure` and `(<*>)`.

## Intuition

McBride and Paterson’s paper introduces the notation $[[g \; x_1 \; x_2 \; \cdots \; x_n]]$ to denote function application in a computational context. If each $x_i$ has type $f \; t_i$ for some applicative functor $f$, and $g$ has type $t_1 \to t_2 \to \dots \to t_n \to t$, then the entire expression $[[g \; x_1 \; x_2 \; \cdots \; x_n]]$ has type $f \; t$. You can think of this as applying a function to multiple “effectful” arguments. In this sense, the double bracket notation is a generalization of `fmap`, which allows us to apply a function to a single argument in a context.

Why do we need `Applicative` to implement this generalization of `fmap`? Suppose we use `fmap` to apply `g` to the first parameter `x1`. Then we get something of type `f (t2 -> ... t)`, but now we are stuck: we can’t apply this function-in-a-context to the next argument with `fmap`. However, this is precisely what `(<*>)` allows us to do.

This suggests the proper translation of the idealized notation $[[g \; x_1 \; x_2 \; \cdots \; x_n]]$ into Haskell, namely

```haskell
  g <$> x1 <*> x2 <*> ... <*> xn,
```

recalling that `Control.Applicative` defines `(<$>)` as convenient infix shorthand for `fmap`. This is what is meant by an “applicative style”—effectful computations can still be described in terms of function application; the only difference is that we have to use the special operator `(<*>)` for application instead of simple juxtaposition.

Note that `pure` allows embedding “non-effectful” arguments in the middle of an idiomatic application, like

```haskell
  g <$> x1 <*> pure x2 <*> x3
```

which has type `f d`, given

```haskell
g  :: a -> b -> c -> d
x1 :: f a
x2 :: b
x3 :: f c
```

The double brackets are commonly known as “idiom brackets”, because they allow writing “idiomatic” function application, that is, function application that looks normal but has some special, non-standard meaning (determined by the particular instance of `Applicative` being used).  Idiom brackets are not supported by GHC, but they are supported by the [Strathclyde Haskell Enhancement](http://personal.cis.strath.ac.uk/~conor/pub/she/), a preprocessor which (among many other things) translates idiom brackets into standard uses of `(<$>)` and `(<*>)`.  This can result in much more readable code when making heavy use of `Applicative`.

In addition, as of GHC 8, the `ApplicativeDo` extension enables `g <$> x1 <*> x2 <*> ... <*> xn` to be written in a different style:
```haskell
do v1 <- x1
   v2 <- x2
   ...
   vn <- xn
   pure (g v1 v2 ... vn)
```
See the Further Reading section below as well as the discussion of `do`-notation in the Monad section for more information.

## Utility functions

`Control.Applicative` provides several utility functions that work generically with any `Applicative` instance.

* `liftA :: Applicative f => (a -> b) -> f a -> f b`.  This should be familiar; of course, it is the same as `fmap` (and hence also the same as `(<$>)`), but with a more restrictive type.  This probably exists to provide a parallel to `liftA2` and `liftA3`, but there is no reason you should ever need to use it.

* `liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c` lifts a 2-argument function to operate in the context of some `Applicative`.  When `liftA2` is fully applied, as in `liftA2 f arg1 arg2`, it is typically better style to instead use `f <$> arg1 <*> arg2`.  However, `liftA2` can be useful in situations where it is partially applied.  For example, one could define a `Num` instance for `Maybe Integer` by defining `(+) = liftA2 (+)` and so on.

* There is a `liftA3` but no `liftAn` for larger `n`.

* `(*>) :: Applicative f => f a -> f b -> f b` sequences the effects of two `Applicative` computations, but discards the result of the first.  For example, if `m1, m2 :: Maybe Int`, then `m1 *> m2` is `Nothing` whenever either `m1` or `m2` is `Nothing`; but if not, it will have the same value as `m2`.

* Likewise, `(<*) :: Applicative f => f a -> f b -> f a` sequences the effects of two computations, but keeps only the result of the first, discarding the result of the second.  Just as with `(<$)` and `($>)`, to keep `(<*)` and `(*>)` straight, remember that they point towards the values that will be kept.

* `(<**>) :: Applicative f => f a -> f (a -> b) -> f b` is similar to `(<*>)`, but where the first computation produces value(s) which are provided as input to the function(s) produced by the second computation.  Note this is not the same as `flip (<*>)`, because the effects are performed in the opposite order.  This is possible to observe with any `Applicative` instance with non-commutative effects, such as the instance for lists: `(<**>) [1,2] [(+5),(*10)]` produces a different result than `(flip (<*>))` on the same arguments.

* `when :: Applicative f => Bool -> f () -> f ()` conditionally executes a computation, evaluating to its second argument if the test is `True`, and to `pure ()` if the test is `False`.

* `unless :: Applicative f => Bool -> f () -> f ()` is like `when`, but with the test negated.

* The `guard` function is for use with instances of `Alternative` (an extension of `Applicative` to incorporate the ideas of failure and choice), which is discussed in the [section on `Alternative` and friends](http://www.haskell.org/haskellwiki/#Failure_and_choice:_Alternative.2C_MonadPlus.2C_ArrowPlus).

### Exercises

- Implement a function `sequenceAL :: Applicative f => [f a] -> f [a]`{.haskell}. There is a generalized version of this, `sequenceA`, which works for any `Traversable` (see the later section on Traversable), but implementing this version specialized to lists is a good exercise.

## Alternative formulation

An alternative, equivalent formulation of `Applicative` is given by

```haskell
class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)
```


Intuitively, this states that a *monoidal* functor^[In category-theory speak, we say `f` is a *lax* monoidal functor because there aren't necessarily functions in the other direction, like `f (a, b) -> (f a, f b)`.] is one which has some sort of "default shape" and which supports some sort of "combining" operation.  `pure` and `(<*>)` are equivalent in power to `unit` and `(**)` (see the Exercises below).  More technically, the idea is that `f` preserves the "monoidal structure" given by the pairing constructor `(,)` and unit type `()`.  This can be seen even more clearly if we rewrite the types of `unit` and `(**)` as
```haskell
  unit' :: () -> f ()
  (**') :: (f a, f b) -> f (a, b)
```

Furthermore, to deserve the name "monoidal" (see the [section on Monoids](http://www.haskell.org/haskellwiki/#Monoid)), instances of `Monoidal` ought to satisfy the following laws, which seem much more straightforward than the traditional `Applicative` laws:


* Left identity^[In this and the following laws, $\cong$ refers to *isomorphism* rather than equality.  In particular we consider `(x,())` $\cong$ `x` $\cong$ `((),x)` and `((x,y),z)` $\cong$ `(x,(y,z))`.]: `unit ** v`{.haskell} $\cong$ `v`
* Right identity: `u ** unit`{.haskell} $\cong$ `u`
* Associativity: `u ** (v ** w)`{.haskell} $\cong$ `(u ** v) ** w`{.haskell}

These turn out to be equivalent to the usual `Applicative` laws. In a category theory setting, one would also require a naturality law:

* Naturality^[Here `g *** h = \(x,y) -> (g x, h y)`.  See [Arrows](http://www.haskell.org/haskellwiki/#Arrow).]: `fmap (g *** h) (u ** v) = fmap g u ** fmap h v`{.haskell}

but in the context of Haskell, this is a free theorem.

Much of this section was taken from [a blog post by Edward Z. Yang](http://blog.ezyang.com/2012/08/applicative-functors/); see his actual post for a bit more information.

### Exercises
#. Implement `pure` and `(<*>)` in terms of `unit` and `(**)`, and vice versa.
#. Are there any `Applicative` instances for which there are also functions `f () -> ()` and `f (a,b) -> (f a, f b)`, satisfying some "reasonable" laws?
#. (Tricky) Prove that given your implementations from the first exercise, the usual `Applicative` laws and the `Monoidal` laws stated above are equivalent.

## Further reading

[McBride and Paterson’s original paper](http://www.soi.city.ac.uk/~ross/papers/Applicative.html) is a treasure-trove of information and examples, as well as some perspectives on the connection between `Applicative` and category theory. Beginners will find it difficult to make it through the entire paper, but it is extremely well-motivated—even beginners will be able to glean something from reading as far as they are able.



Conal Elliott has been one of the biggest proponents of `Applicative`. For example, the [Pan library for functional images](http://conal.net/papers/functional-images/) and the reactive library for functional reactive programming (FRP) ^[Introduced by [an earlier paper](http://conal.net/papers/simply-reactive/) that was since superseded by [Push-pull functional reactive programming](http://conal.net/papers/push-pull-frp/).] make key use of it; his blog also contains [many examples of `Applicative` in action](http://conal.net/blog/tag/applicative-functor). Building on the work of McBride and Paterson, Elliott also built the [TypeCompose](http://www.haskell.org/haskellwiki/TypeCompose) library, which embodies the observation (among others) that `Applicative` types are closed under composition; therefore, `Applicative` instances can often be automatically derived for complex types built out of simpler ones.

Although the [Parsec parsing library](http://hackage.haskell.org/package/parsec) ([paper](http://legacy.cs.uu.nl/daan/download/papers/parsec-paper.pdf)) was originally designed for use as a monad, in its most common use cases an `Applicative` instance can be used to great effect; [Bryan O’Sullivan’s blog post](http://www.serpentine.com/blog/2008/02/06/the-basics-of-applicative-functors-put-to-practical-work/) is a good starting point. If the extra power provided by `Monad` isn’t needed, it’s usually a good idea to use `Applicative` instead.

A couple other nice examples of `Applicative` in action include the [ConfigFile and HSQL libraries](http://web.archive.org/web/20090416111947/chrisdone.com/blog/html/2009-02-10-applicative-configfile-hsql.html) and the [formlets library](http://groups.inf.ed.ac.uk/links/formlets/).

Gershom Bazerman's [post](http://comonad.com/reader/2012/abstracting-with-applicatives/) contains many insights into applicatives.

The `ApplicativeDo` extension is described in [this wiki page](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo), and in more detail in [this Haskell Symposium paper](http://doi.org/10.1145/2976002.2976007).

# Monad

It’s a safe bet that if you’re reading this, you’ve heard of monads—although it’s quite possible you’ve never heard of `Applicative` before, or `Arrow`, or even `Monoid`. Why are monads such a big deal in Haskell? There are several reasons.

* Haskell does, in fact, single out monads for special attention by making them the framework in which to construct I/O operations.
* Haskell also singles out monads for special attention by providing a special syntactic sugar for monadic expressions: the `do`-notation. (As of GHC 8, `do`-notation can be used with `Applicative` as well, but the notation is still fundamentally related to monads.)
* `Monad` has been around longer than other abstract models of computation such as `Applicative` or `Arrow`.
* The more monad tutorials there are, the harder people think monads must be, and the more new monad tutorials are written by people who think they finally “get” monads (the [monad tutorial fallacy](http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/)).

I will let you judge for yourself whether these are good reasons.

In the end, despite all the hoopla, `Monad` is just another type class. Let’s take a look at its definition.

## Definition
As of GHC 7.10, [`Monad`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monad) is defined as:

```haskell
class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  m >> n = m >>= \_ -> n

  fail   :: String -> m a
```

(Prior to GHC 7.10, `Applicative` was not a superclass of `Monad`, for historical reasons.)

The `Monad` type class is exported by the `Prelude`, along with a few standard instances. However, many utility functions are found in [`Control.Monad`](https://hackage.haskell.org/package/base/docs/Control-Monad.html).

Let’s examine the methods in the `Monad` class one by one. The type of `return` should look familiar; it’s the same as `pure`. Indeed, `return` *is* `pure`, but with an unfortunate name. (Unfortunate, since someone coming from an imperative programming background might think that `return` is like the C or Java keyword of the same name, when in fact the similarities are minimal.) For historical reasons, we still have both names, but they should always denote the same value (although this cannot be enforced). It is possible that `return` may eventually be removed from the `Monad` class.

We can see that `(>>)` is a specialized version of `(>>=)`, with a default implementation given. It is only included in the type class declaration so that specific instances of `Monad` can override the default implementation of `(>>)` with a more efficient one, if desired. Also, note that although `_ >> n = n` would be a type-correct implementation of `(>>)`, it would not correspond to the intended semantics: the intention is that `m >> n` ignores the *result* of `m`, but not its *effects*.

The `fail` function is an awful hack that has no place in the `Monad` class; more on this later.

The only really interesting thing to look at—and what makes `Monad` strictly more powerful than `Applicative`—is `(>>=)`, which is often called *bind*.

We could spend a while talking about the intuition behind `(>>=)`—and we will. But first, let’s look at some examples.

## Instances

Even if you don’t understand the intuition behind the `Monad` class, you can still create instances of it by just seeing where the types lead you. You may be surprised to find that this actually gets you a long way towards understanding the intuition; at the very least, it will give you some concrete examples to play with as you read more about the `Monad` class in general. The first few examples are from the standard `Prelude`; the remaining examples are from the [`transformers` package](http://hackage.haskell.org/package/transformers).

- The simplest possible instance of `Monad` is [`Identity`](http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/Control-Monad-Identity.html), which is described in Dan Piponi’s highly recommended blog post on [The Trivial Monad](http://blog.sigfpe.com/2007/04/trivial-monad.html). Despite being “trivial”, it is a great introduction to the `Monad` type class, and contains some good exercises to get your brain working.

- The next simplest instance of `Monad` is `Maybe`. We already know how to write `return`/`pure` for `Maybe`. So how do we write `(>>=)`? Well, let’s think about its type. Specializing for `Maybe`, we have

    ```haskell
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b.
    ```

    If the first argument to `(>>=)` is `Just x`, then we have something of type `a` (namely, `x`), to which we can apply the second argument—resulting in a `Maybe b`, which is exactly what we wanted. What if the first argument to `(>>=)` is `Nothing`? In that case, we don’t have anything to which we can apply the `a -> Maybe b` function, so there’s only one thing we can do: yield `Nothing`. This instance is:

    ```haskell
    instance Monad Maybe where
      return :: a -> Maybe a
      return = Just

      (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
      (Just x) >>= g = g x
      Nothing  >>= _ = Nothing
    ```

    We can already get a bit of intuition as to what is going on here: if we build up a computation by chaining together a bunch of functions with `(>>=)`, as soon as any one of them fails, the entire computation will fail (because `Nothing >>= f` is `Nothing`, no matter what `f` is). The entire computation succeeds only if all the constituent functions individually succeed. So the `Maybe` monad models computations which may fail.

- The `Monad` instance for the list constructor `[]` is similar to its `Applicative` instance; see the exercise below.

- Of course, the `IO` constructor is famously a `Monad`, but its implementation is somewhat magical, and may in fact differ from compiler to compiler. It is worth emphasizing that the `IO` monad is the *only* monad which is magical. It allows us to build up, in an entirely pure way, values representing possibly effectful computations. The special value `main`, of type `IO ()`, is taken by the runtime and actually executed, producing actual effects. Every other monad is functionally pure, and requires no special compiler support. We often speak of monadic values as “effectful computations”, but this is because some monads allow us to write code *as if* it has side effects, when in fact the monad is hiding the plumbing which allows these apparent side effects to be implemented in a functionally pure way.

- As mentioned earlier, `((->) e)` is known as the *reader monad*, since it describes computations in which a value of type `e` is available as a read-only environment.  The [`Control.Monad.Reader`](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.html) module provides the `Reader e a` type, which is just a convenient `newtype` wrapper around `(e -> a)`, along with an appropriate `Monad` instance and some `Reader`-specific utility functions such as `ask` (retrieve the environment), `asks` (retrieve a function of the environment), and `local` (run a subcomputation under a different environment).

- The [`Control.Monad.Writer`](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Writer-Lazy.html) module provides the `Writer` monad, which allows information to be collected as a computation progresses. `Writer w a` is isomorphic to `(a,w)`, where the output value `a` is carried along with an annotation or “log” of type `w`, which must be an instance of `Monoid` (see [section Monoid](http://www.haskell.org/haskellwiki/#Monoid)); the special function `tell` performs logging.

- The [`Control.Monad.State`](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html) module provides the `State s a` type, a `newtype` wrapper around `s -> (a,s)`. Something of type `State s a` represents a stateful computation which produces an `a` but can access and modify the state of type `s` along the way. The module also provides `State`-specific utility functions such as `get` (read the current state), `gets` (read a function of the current state), `put` (overwrite the state), and `modify` (apply a function to the state).

- The [`Control.Monad.Cont`](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Cont.html) module provides the `Cont` monad, which represents computations in continuation-passing style. It can be used to suspend and resume computations, and to implement non-local transfers of control, co-routines, other complex control structures—all in a functionally pure way. `Cont` has been called the [“mother of all monads”](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html) because of its universal properties.

### Exercises

#. Implement a `Monad` instance for the list constructor, `[]`. Follow the types!

#. Implement a `Monad` instance for `((->) e)`.

#. Implement `Functor` and `Monad` instances for `Free f`, defined as

    ```haskell
    data Free f a = Var a
                  | Node (f (Free f a))
    ```

    You may assume that `f` has a `Functor` instance.  This is known as the *free monad* built from the functor `f`.

## Intuition

Let’s look more closely at the type of `(>>=)`. The basic intuition is that it combines two computations into one larger computation. The first argument, `m a`, is the first computation. However, it would be boring if the second argument were just an `m b`; then there would be no way for the computations to interact with one another (actually, this is exactly the situation with `Applicative`). So, the second argument to `(>>=)` has type `a -> m b`: a function of this type, given a *result* of the first computation, can produce a second computation to be run. In other words, `x >>= k` is a computation which runs `x`, and then uses the result(s) of `x` to *decide* what computation to run second, using the output of the second computation as the result of the entire computation.

Intuitively, it is this ability to use the output from previous computations to decide what computations to run next that makes `Monad` more powerful than `Applicative`. The structure of an `Applicative` computation is fixed, whereas the structure of a `Monad` computation can change based on intermediate results.  This also means that parsers built using an `Applicative` interface can only parse context-free languages; in order to parse context-sensitive languages a `Monad` interface is needed.^[Actually, because Haskell allows general recursion, this is a lie: using a Haskell parsing library one can recursively construct *infinite* grammars, and hence `Applicative` (together with `Alternative`) is enough to parse any context-sensitive language with a finite alphabet. See [Parsing context-sensitive languages with Applicative](http://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/).]

To see the increased power of `Monad` from a different point of view, let’s see what happens if we try to implement `(>>=)` in terms of `fmap`, `pure`, and `(<*>)`. We are given a value `x` of type `m a`, and a function `k` of type `a -> m b`, so the only thing we can do is apply `k` to `x`. We can’t apply it directly, of course; we have to use `fmap` to lift it over the `m`. But what is the type of `fmap k`? Well, it’s `m a -> m (m b)`. So after we apply it to `x`, we are left with something of type `m (m b)`—but now we are stuck; what we really want is an `m b`, but there’s no way to get there from here. We can *add* `m`’s using `pure`, but we have no way to *collapse* multiple `m`’s into one.

This ability to collapse multiple `m`’s is exactly the ability provided by the function `join :: m (m a) -> m a`, and it should come as no surprise that an alternative definition of `Monad` can be given in terms of `join`:

```haskell
class Applicative m => Monad'' m where
  join :: m (m a) -> m a
```

In fact, the canonical definition of monads in category theory is in terms of `return`, `fmap`, and `join` (often called $\eta$, $T$, and $\mu$ in the mathematical literature). Haskell uses an alternative formulation with `(>>=)` instead of `join` since it is more convenient to use ^[You might hear some people claim that that the definition in terms of `return`, `fmap`, and `join` is the “math definition” and the definition in terms of `return` and `(>>=)` is something specific to Haskell. In fact, both definitions were known in the mathematics community long before Haskell picked up monads.]. However, sometimes it can be easier to think about `Monad` instances in terms of `join`, since it is a more “atomic” operation. (For example, `join` for the list monad is just `concat`.)

### Exercises

#. Implement `(>>=)` in terms of `fmap` (or `liftM`) and `join`.
#. Now implement `join` and `fmap` (`liftM`) in terms of `(>>=)` and `return`.

## Utility functions

The [`Control.Monad`](https://hackage.haskell.org/package/base/docs/Control-Monad.html) module provides a large number of convenient utility functions, all of which can be implemented in terms of the basic `Monad` operations (`return` and `(>>=)` in particular).  We have already seen one of them, namely, `join`.  We also mention some other noteworthy ones here; implementing these utility functions oneself is a good exercise.  For a more detailed guide to these functions, with commentary and example code, see Henk-Jan van Tuyl’s [tour](http://members.chello.nl/hjgtuyl/tourdemonad.html).

* `liftM :: Monad m => (a -> b) -> m a -> m b`.  This should be familiar; of course, it is just `fmap`.  The fact that we have both `fmap` and `liftM` is a consequence of the fact that the `Monad` type class did not require a `Functor` instance until recently, even though mathematically speaking, every monad is a functor. If you are using GHC 7.10 or newer, you should avoid using `liftM` and just use `fmap` instead.

* `ap :: Monad m => m (a -> b) -> m a -> m b` should also be familiar: it is equivalent to `(<*>)`, justifying the claim that the `Monad` interface is strictly more powerful than `Applicative`. We can make any `Monad` into an instance of `Applicative` by setting `pure = return` and `(<*>) = ap`.

* `sequence :: Monad m => [m a] -> m [a]` takes a list of computations and combines them into one computation which collects a list of their results.  It is again something of a historical accident that `sequence` has a `Monad` constraint, since it can actually be implemented only in terms of `Applicative` (see the exercise at the end of the Utility Functions section for Applicative).  Note that the actual type of `sequence` is more general, and works over any `Traversable` rather than just lists; see the [section on `Traversable`](http://www.haskell.org/haskellwiki/#Traversable).

* `replicateM :: Monad m => Int -> m a -> m [a]` is simply a combination of [`replicate`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:replicate) and `sequence`.

* `mapM :: Monad m => (a -> m b) -> [a] -> m [b]` maps its first argument over the second, and `sequence`s the results. The `forM` function is just `mapM` with its arguments reversed; it is called `forM` since it models generalized `for` loops: the list `[a]` provides the loop indices, and the function `a -> m b` specifies the “body” of the loop for each index.  Again, these functions actually work over any `Traversable`, not just lists, and they can also be defined in terms of `Applicative`, not `Monad`: the analogue of `mapM` for `Applicative` is called `traverse`.

* `(=<<) :: Monad m => (a -> m b) -> m a -> m b` is just `(>>=)` with its arguments reversed; sometimes this direction is more convenient since it corresponds more closely to function application.

* `(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c` is sort of like function composition, but with an extra `m` on the result type of each function, and the arguments swapped. We’ll have more to say about this operation later. There is also a flipped variant, `(<=<)`.

Many of these functions also have “underscored” variants, such as `sequence_` and `mapM_`; these variants throw away the results of the computations passed to them as arguments, using them only for their side effects.

Other monadic functions which are occasionally useful include `filterM`, `zipWithM`, `foldM`, and `forever`.

## Laws

There are several laws that instances of `Monad` should satisfy (see also the [Monad laws](http://www.haskell.org/haskellwiki/Monad laws) wiki page). The standard presentation is:

```haskell
return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
```

The first and second laws express the fact that `return` behaves nicely: if we inject a value `a` into a monadic context with `return`, and then bind to `k`, it is the same as just applying `k` to `a` in the first place; if we bind a computation `m` to `return`, nothing changes. The third law essentially says that `(>>=)` is associative, sort of.

However, the presentation of the above laws, especially the third, is marred by the asymmetry of `(>>=)`.  It’s hard to look at the laws and see what they’re really saying. I prefer a much more elegant version of the laws, which is formulated in terms of `(>=>)` ^[I like to pronounce this operator “fish”.]. Recall that `(>=>)` “composes” two functions of type `a -> m b` and `b -> m c`.  You can think of something of type `a -> m b` (roughly) as a function from `a` to `b` which may also have some sort of effect in the context corresponding to `m`. `(>=>)` lets us compose these “effectful functions”, and we would like to know what properties `(>=>)` has.  The monad laws reformulated in terms of `(>=>)` are:

```haskell
return >=> g  =  g
g >=> return  =  g
(g >=> h) >=> k  =  g >=> (h >=> k)
```

Ah, much better!  The laws simply state that `return` is the identity of `(>=>)`, and that `(>=>)` is associative ^[As fans of category theory will note, these laws say precisely that functions of type `a -> m b` are the arrows of a category with `(>=>)` as composition!  Indeed, this is known as the *Kleisli category* of the monad `m`.  It will come up again when we discuss `Arrow`s.].

There is also a formulation of the monad laws in terms of `fmap`, `return`, and `join`; for a discussion of this formulation, see the Haskell [wikibook page on category theory](http://en.wikibooks.org/wiki/Haskell/Category_theory).

### Exercises

- Given the definition `g >=> h = \x -> g x >>= h`, prove the equivalence of the above laws and the usual monad laws.

## `do` notation

Haskell’s special `do` notation supports an “imperative style” of programming by providing syntactic sugar for chains of monadic expressions.  The genesis of the notation lies in realizing that something like `a >>= \x -> b >> c >>= \y -> d ` can be more readably written by putting successive computations on separate lines:

```haskell
a >>= \x ->
b >>
c >>= \y ->
d
```

This emphasizes that the overall computation consists of four computations `a`, `b`, `c`, and `d`, and that `x` is bound to the result of `a`, and `y` is bound to the result of `c` (`b`, `c`, and `d` are allowed to refer to `x`, and `d` is allowed to refer to `y` as well).  From here it is not hard to imagine a nicer notation:

```haskell
do { x <- a
   ;      b
   ; y <- c
   ;      d
   }
```

(The curly braces and semicolons may optionally be omitted; the Haskell parser uses layout to determine where they should be inserted.)  This discussion should make clear that `do` notation is just syntactic sugar.  In fact, `do` blocks are recursively translated into monad operations (almost) like this:

                      do e → e
           do { e; stmts } → e >> do { stmts }
      do { v <- e; stmts } → e >>= \v -> do { stmts }
    do { let decls; stmts} → let decls in do { stmts }

This is not quite the whole story, since `v` might be a pattern instead of a variable.  For example, one can write

```haskell
do (x:xs) <- foo
   bar x
```

but what happens if `foo` is an empty list?  Well, remember that ugly `fail` function in the `Monad` type class declaration?  That’s what happens.  See [section 3.14 of the Haskell Report](http://www.haskell.org/onlinereport/exps.html#sect3.14) for the full details. See also the discussion of `MonadPlus` and `MonadZero` in the [section on other monoidal classes](http://www.haskell.org/haskellwiki/#Other monoidal classes: Alternative, MonadPlus, ArrowPlus).

A final note on intuition: `do` notation plays very strongly to the “computational context” point of view rather than the “container” point of view, since the binding notation `x <- m` is suggestive of “extracting” a single `x` from `m` and doing something with it.  But `m` may represent some sort of a container, such as a list or a tree; the meaning of `x <- m` is entirely dependent on the implementation of `(>>=)`.  For example, if `m` is a list, `x <- m` actually means that `x` will take on each value from the list in turn.

Sometimes, the full power of `Monad` is not needed to desugar `do`-notation. For example,

```haskell
do x <- foo1
   y <- foo2
   z <- foo3
   return (g x y z)
```

would normally be desugared to `foo1 >>= \x -> foo2 >>= \y -> foo3 >>= \z -> return (g x y z)`, but this is equivalent to `g <$> foo1 <*> foo2 <*> foo3`.  With the `ApplicativeDo` extension enabled (as of GHC 8.0), GHC tries hard to desugar `do`-blocks using `Applicative` operations wherever possible.  This can sometimes lead to efficiency gains, even for types which also have `Monad` instances, since in general `Applicative` computations may be run in parallel, whereas monadic ones may not.  For example, consider

```haskell
g :: Int -> Int -> M Int

-- These could be expensive
bar, baz :: M Int

foo :: M Int
foo = do
  x <- bar
  y <- baz
  g x y
```

`foo` definitely depends on the `Monad` instance of `M`, since the effects generated by the whole computation may depend (via `g`) on the `Int` outputs of `bar` and `baz`.  Nonetheless, with `ApplicativeDo` enabled, `foo` can be desugared as
```haskell
join (g <$> bar <*> baz)
```
which may allow `bar` and `baz` to be computed in parallel, since they at least do not depend on each other.

The `ApplicativeDo` extension is described in [this wiki page](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo), and in more detail in [this Haskell Symposium paper](http://doi.org/10.1145/2976002.2976007).

## Further reading

Philip Wadler was the first to propose using monads to structure functional programs.  [His paper](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html) is still a readable introduction to the subject.

There are, of course, numerous monad tutorials of varying quality ^[[All About Monads](http://www.haskell.org/haskellwiki/All About Monads), [Monads as containers](http://www.haskell.org/haskellwiki/Monads_as_Containers), [Understanding monads](http://en.wikibooks.org/w/index.php?title=Haskell/Understanding_monads), [The Monadic Way](http://www.haskell.org/haskellwiki/The Monadic Way), [You Could Have Invented Monads! (And Maybe You Already Have.)](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html), [there’s a monster in my Haskell!](http://www.haskell.org/pipermail/haskell-cafe/2006-November/019190.html), [Understanding Monads. For real.](http://kawagner.blogspot.com/2007/02/understanding-monads-for-real.html), [Monads in 15 minutes: Backtracking and Maybe](http://www.randomhacks.net/articles/2007/03/12/monads-in-15-minutes), [Monads as computation](http://www.haskell.org/haskellwiki/Monads_as_computation), [Practical Monads](http://metafoo.co.uk/practical-monads.txt)].

A few of the best include Cale Gibbard’s [Monads as containers](http://www.haskell.org/haskellwiki/Monads_as_Containers) and [Monads as computation](http://www.haskell.org/haskellwiki/Monads_as_computation); Jeff Newbern’s [All About Monads](http://www.haskell.org/haskellwiki/All About Monads), a comprehensive guide with lots of examples; and Dan Piponi’s [You Could Have Invented Monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html), which features great exercises.  If you just want to know how to use `IO`, you could consult the [Introduction to IO](http://www.haskell.org/haskellwiki/Introduction to IO). Even this is just a sampling; the [monad tutorials timeline](http://www.haskell.org/haskellwiki/monad tutorials timeline) is a more complete list. (All these monad tutorials have prompted parodies like [think of a monad ...](http://koweycode.blogspot.com/2007/01/think-of-monad.html) as well as other kinds of backlash like [Monads! (and Why Monad Tutorials Are All Awful)](http://ahamsandwich.wordpress.com/2007/07/26/monads-and-why-monad-tutorials-are-all-awful/) or [Abstraction, intuition, and the “monad tutorial fallacy”](http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/).)

Other good monad references which are not necessarily tutorials include [Henk-Jan van Tuyl’s tour](http://members.chello.nl/hjgtuyl/tourdemonad.html) of the functions in `Control.Monad`, Dan Piponi’s [field guide](http://blog.sigfpe.com/2006/10/monads-field-guide.html), Tim Newsham’s [What’s a Monad?](http://www.thenewsh.com/~newsham/haskell/monad.html), and Chris Smith's excellent article [Why Do Monads Matter?](http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/). There are also many blog posts which have been written on various aspects of monads; a collection of links can be found under [Blog articles/Monads](http://www.haskell.org/haskellwiki/Blog articles/Monads).

For help constructing monads from scratch, and for obtaining a "deep embedding" of monad operations suitable for use in, say, compiling a domain-specific language, see [Apfelmus's operational package](http://projects.haskell.org/operational).

One of the quirks of the `Monad` class and the Haskell type system is that it is not possible to straightforwardly declare `Monad` instances for types which require a class constraint on their data, even if they are monads from a mathematical point of view. For example, `Data.Set` requires an `Ord` constraint on its data, so it cannot be easily made an instance of `Monad`.  A solution to this problem was [first described by Eric Kidd](http://www.randomhacks.net/articles/2007/03/15/data-set-monad-haskell-macros), and later made into a [library named rmonad](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/rmonad) by Ganesh Sittampalam and Peter Gavin.

There are many good reasons for eschewing `do` notation; some have gone so far as to [consider it harmful](http://www.haskell.org/haskellwiki/Do_notation_considered_harmful).

Monads can be generalized in various ways; for an exposition of one possibility, see Robert Atkey’s paper on [parameterized monads](http://homepages.inf.ed.ac.uk/ratkey/paramnotions-jfp.pdf), or Dan Piponi’s [Beyond Monads](http://blog.sigfpe.com/2009/02/beyond-monads.html).

For the categorically inclined, monads can be viewed as monoids ([From Monoids to Monads](http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html)) and also as closure operators ([Triples and Closure](http://blog.plover.com/math/monad-closure.html)).  Derek Elkins’ article in [issue 13 of the Monad.Reader](http://www.haskell.org/wikiupload/8/85/TMR-Issue13.pdf) contains an exposition of the category-theoretic underpinnings of some of the standard `Monad` instances, such as `State` and `Cont`.  Jonathan Hill and Keith Clarke have [an early paper explaining the connection between monads as they arise in category theory and as used in functional programming](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.53.6497).  There is also a [web page by Oleg Kiselyov](http://okmij.org/ftp/Computation/IO-monad-history.html) explaining the history of the IO monad.

Links to many more research papers related to monads can be found under [Research papers/Monads and arrows](http://www.haskell.org/haskellwiki/Research papers/Monads and arrows).

# MonadFail

Some monads support a notion of *failure*, without necessarily supporting the notion of *recovery* suggested by `MonadPlus`, and possibly including a primitive error reporting mechanism. This notion is expressed by the relatively unprincipled `MonadFail`. When the `MonadFailDesugaring` language extension is enabled, the `fail` method from `MonadFail` is used for pattern match failure in `do` bindings rather than the traditional `fail` method of the `Monad` class. This language change is being implemented because there are many monads, such as `Reader`, `State`, `Writer`, `RWST`, and `Cont` that simply do not support a legitimate `fail` method.

## Definition

```haskell
class Monad m => MonadFail m where
  fail :: String -> m a
```

## Law

```haskell
fail s >>= m = fail s
```

# Monad transformers

One would often like to be able to combine two monads into one: for example, to have stateful, nondeterministic computations (`State` + `[]`), or computations which may fail and can consult a read-only environment (`Maybe` + `Reader`), and so on.  Unfortunately, monads do not compose as nicely as applicative functors (yet another reason to use `Applicative` if you don’t need the full power that `Monad` provides), but some monads can be combined in certain ways.

## Standard monad transformers

The [transformers](http://hackage.haskell.org/package/transformers) library provides a number of standard *monad transformers*. Each monad transformer adds a particular capability/feature/effect to any existing monad.

* [`IdentityT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Identity.html) is the identity transformer, which maps a monad to (something isomorphic to) itself.  This may seem useless at first glance, but it is useful for the same reason that the `id` function is useful -- it can be passed as an argument to things which are parameterized over an arbitrary monad transformer, when you do not actually want any extra capabilities.
* [`StateT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-State.html) adds a read-write state.
* [`ReaderT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Reader.html) adds a read-only environment.
* [`WriterT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Writer.html) adds a write-only log.
* [`RWST`](http://hackage.haskell.org/packages/archive/transformers/0.2.2.0/doc/html/Control-Monad-Trans-RWS.html) conveniently combines `ReaderT`, `WriterT`, and `StateT` into one.
* [`MaybeT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Maybe.html) adds the possibility of failure.
* [`ErrorT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Error.html) adds the possibility of failure with an arbitrary type to represent errors.
* [`ListT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-List.html) adds non-determinism (however, see the discussion of `ListT` below).
* [`ContT`](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Cont.html) adds continuation handling.

For example, `StateT s Maybe` is an instance of `Monad`; computations of type `StateT s Maybe a` may fail, and have access to a mutable state of type `s`.  Monad transformers can be multiply stacked.  One thing to keep in mind while using monad transformers is that the order of composition matters.  For example, when a `StateT s Maybe a` computation fails, the state ceases being updated (indeed, it simply disappears); on the other hand, the state of a `MaybeT (State s) a` computation may continue to be modified even after the computation has "failed". This may seem backwards, but it is correct. Monad transformers build composite monads “inside out”; `MaybeT (State s) a` is isomorphic to `s -> (Maybe a, s)`.  (Lambdabot has an indispensable `@unmtl` command which you can use to “unpack” a monad transformer stack in this way.)
Intuitively, the monads become "more fundamental" the further inside the stack you get, and the effects of inner monads "have precedence" over the effects of outer ones.  Of course, this is just handwaving, and if you are unsure of the proper order for some monads you wish to combine, there is no substitute for using `@unmtl` or simply trying out the various options.

## Definition and laws

All monad transformers should implement the `MonadTrans` type class, defined in `Control.Monad.Trans.Class`:

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

It allows arbitrary computations in the base monad `m` to be “lifted” into computations in the transformed monad `t m`. (Note that type application associates to the left, just like function application, so `t m a = (t m) a`.)

`lift` must satisfy the laws
```haskell
lift . return   =  return
lift (m >>= f)  =  lift m >>= (lift . f)
```
which intuitively state that `lift` transforms `m a` computations into `t m a` computations in a "sensible" way, which sends the `return` and `(>>=)` of `m` to the `return` and `(>>=)` of `t m`.

### Exercises
- What is the kind of `t` in the declaration of `MonadTrans`?

## Transformer type classes and "capability" style

There are also type classes (provided by the [`mtl` package](http://hackage.haskell.org/package/mtl)) for the operations of each transformer.  For example, the `MonadState` type class provides the state-specific methods `get` and `put`, allowing you to conveniently use these methods not only with `State`, but with any monad which is an instance of `MonadState`—including `MaybeT (State s)`, `StateT s (ReaderT r IO)`, and so on. Similar type classes exist for `Reader`, `Writer`, `Cont`, `IO`, and others ^[The only problem with this scheme is the quadratic number of instances required as the number of standard monad transformers grows—but as the current set of standard monad transformers seems adequate for most common use cases, this may not be that big of a deal.].

These type classes serve two purposes.  First, they get rid of (most of) the need for explicitly using `lift`, giving a type-directed way to automatically determine the right number of calls to `lift`. Simply writing `put` will be automatically translated into `lift . put`, `lift . lift . put`, or something similar depending on what concrete monad stack you are using.

Second, they give you more flexibility to switch between different concrete monad stacks.  For example, if you are writing a state-based algorithm, don't write
```haskell
foo :: State Int Char
foo = modify (*2) >> return 'x'
```
but rather
```haskell
foo :: MonadState Int m => m Char
foo = modify (*2) >> return 'x'
```
Now, if somewhere down the line you realize you need to introduce the possibility of failure, you might switch from `State Int` to `MaybeT (State Int)`.  The type of the first version of `foo` would need to be modified to reflect this change, but the second version of `foo` can still be used as-is.

However, this sort of "capability-based" style (*e.g.* specifying that `foo` works for any monad with the "state capability") quickly runs into problems when you try to naively scale it up: for example, what if you need to maintain two independent states?  A framework for solving this and related problems is described by Schrijvers and Olivera ([Monads, zippers and views: virtualizing the monad stack, ICFP 2011](http://users.ugent.be/~tschrijv/Research/papers/icfp2011.pdf)) and is implemented in the [`Monatron` package](http://hackage.haskell.org/package/Monatron).

## Composing monads

Is the composition of two monads always a monad? As hinted previously, the answer is no.

Since `Applicative` functors are closed under composition, the problem must lie with `join`.  Indeed, suppose `m` and `n` are arbitrary monads; to make a monad out of their composition we would need to be able to implement
```haskell
join :: m (n (m (n a))) -> m (n a)
```
but it is not clear how this could be done in general.  The  `join` method for `m` is no help, because the two occurrences of `m` are not next to each other (and likewise for `n`).

However, one situation in which it can be done is if `n` *distributes* over `m`, that is, if there is a function
```haskell
distrib :: n (m a) -> m (n a)
```
satisfying certain laws. See Jones and Duponcheel ([Composing Monads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.42.2605)); see also the [section on Traversable](http://www.haskell.org/haskellwiki/#Traversable).

For a much more in-depth discussion and analysis of the failure of monads to be closed under composition, see [this question on StackOverflow](http://stackoverflow.com/questions/13034229/concrete-example-showing-that-monads-are-not-closed-under-composition-with-proo?lq=1).

### Exercises

- Implement `join :: M (N (M (N a))) -> M (N a)`, given `distrib :: N (M a) -> M (N a)` and assuming `M` and `N` are instances of `Monad`.

## Further reading

Much of the monad transformer library (originally [`mtl`](http://hackage.haskell.org/package/mtl), now split between `mtl` and [`transformers`](http://hackage.haskell.org/package/transformers)), including the `Reader`, `Writer`, `State`, and other monads, as well as the monad transformer framework itself, was inspired by Mark Jones’ classic paper [Functional Programming with Overloading and Higher-Order Polymorphism](http://web.cecs.pdx.edu/~mpj/pubs/springschool.html). It’s still very much worth a read—and highly readable—after almost fifteen years.

See [Edward Kmett's mailing list message](http://article.gmane.org/gmane.comp.lang.haskell.libraries/17139) for a description of the history and relationships among monad transformer packages (`mtl`, `transformers`, `monads-fd`, `monads-tf`).

There are two excellent references on monad transformers. Martin Grabmüller’s [Monad Transformers Step by Step](http://catamorph.de/publications/2004-10-01-monad-transformers.html) is a thorough description, with running examples, of how to use monad transformers to elegantly build up computations with various effects.  [Cale Gibbard’s article](http://cale.yi.org/index.php/How_To_Use_Monad_Transformers) on how to use monad transformers is more practical, describing how to structure code using monad transformers to make writing it as painless as possible.  Another good starting place for learning about monad transformers is a [blog post by Dan Piponi](http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html).

The `ListT` transformer from the `transformers` package comes with the caveat that `ListT m` is only a monad when `m` is *commutative*, that is, when `ma >>= \a -> mb >>= \b -> foo` is equivalent to `mb >>= \b -> ma >>= \a -> foo` (i.e. the order of `m`'s effects does not matter).  For one explanation why, see  Dan Piponi's blog post ["Why isn't `<nowiki>ListT [](http://blog.sigfpe.com/2006/11/why-isnt-listt-monad.html)</nowiki>` a monad"].  For more examples, as well as a design for a version of `ListT` which does not have this problem, see [`ListT` done right](http://www.haskell.org/haskellwiki/ListT_done_right).

There is an alternative way to compose monads, using coproducts, as described by [Lüth and Ghani](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.8.3581).  This method is interesting but has not (yet?) seen widespread use.  For a more recent alternative, see Kiselyov et al's [Extensible Effects: An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/exteff.pdf).

# MonadFix

> Note: `MonadFix` is included here for completeness (and because it is interesting) but seems not to be used much.  Skipping this section on a first read-through is perfectly OK (and perhaps even recommended).

## `do rec` notation

The `MonadFix` class describes monads which support the special fixpoint operation `mfix :: (a -> m a) -> m a`, which allows the output of monadic computations to be defined via (effectful) recursion.  This is [supported in GHC](http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#recursive-do-notation) by a special “recursive do” notation, enabled by the `-XRecursiveDo` flag.  Within a `do` block, one may have a nested `rec` block, like so:
```haskell
do { x <- foo
   ; rec { y <- baz
         ; z <- bar
         ;      bob
         }
   ; w <- frob
   }
```
Normally (if we had `do` in place of `rec` in the above example), `y` would be in scope in `bar` and `bob` but not in `baz`, and `z` would be in scope only in `bob`.  With the `rec`, however, `y` and `z` are both in scope in all three of `baz`, `bar`, and `bob`. A `rec` block is analogous to a `let` block such as
```haskell
let { y = baz
    ; z = bar
    }
in bob
```
because, in Haskell, every variable bound in a `let`-block is in scope throughout the entire block.  (From this point of view, Haskell's normal `do` blocks are analogous to Scheme's `let*` construct.)

What could such a feature be used for?  One of the motivating examples given in the original paper describing `MonadFix` (see below) is encoding circuit descriptions.  A line in a `do`-block such as

```haskell
  x <- gate y z
```

describes a gate whose input wires are labeled `y` and `z` and whose output wire is labeled `x`.  Many (most?) useful circuits, however, involve some sort of feedback loop, making them impossible to write in a normal `do`-block (since some wire would have to be mentioned as an input *before* being listed as an output).  Using a `rec` block solves this problem.

## Examples and intuition

Of course, not every monad supports such recursive binding.  However, as mentioned above, it suffices to have an implementation of `mfix :: (a -> m a) -> m a`, satisfying a few laws.  Let's try implementing `mfix` for the `Maybe` monad.  That is, we want to implement a function

```haskell
maybeFix :: (a -> Maybe a) -> Maybe a
```

Let's think for a moment about the implementation^[Actually, `fix` is implemented slightly differently for efficiency reasons; but the given definition is equivalent and simpler for the present purpose.] of the non-monadic `fix :: (a -> a) -> a`:

```haskell
fix f = f (fix f)
```

Inspired by `fix`, our first attempt at implementing `maybeFix` might be something like

```haskell
maybeFix :: (a -> Maybe a) -> Maybe a
maybeFix f = maybeFix f >>= f
```

This has the right type.  However, something seems wrong: there is nothing in particular here about `Maybe`; `maybeFix` actually has the more general type `Monad m => (a -> m a) -> m a`.  But didn't we just say that not all monads support `mfix`?

The answer is that although this implementation of `maybeFix` has the right type, it does *not* have the intended semantics.  If we think about how `(>>=)` works for the `Maybe` monad (by pattern-matching on its first argument to see whether it is `Nothing` or `Just`) we can see that this definition of `maybeFix` is completely useless: it will just recurse infinitely, trying to decide whether it is going to return `Nothing` or `Just`, without ever even so much as a glance in the direction of `f`.

The trick is to simply *assume* that `maybeFix` will return `Just`, and get on with life!

```haskell
maybeFix :: (a -> Maybe a) -> Maybe a
maybeFix f = ma
  where ma = f (fromJust ma)
```

This says that the result of `maybeFix` is `ma`, and assuming that `ma = Just x`, it is defined (recursively) to be equal to `f x`.

Why is this OK?  Isn't `fromJust` almost as bad as `unsafePerformIO`?  Well, usually, yes.  This is just about the only situation in which it is justified!  The interesting thing to note is that `maybeFix` *will never crash* -- although it may, of course, fail to terminate.  The only way we could get a crash is if we try to evaluate `fromJust ma` when we know that `ma = Nothing`.  But how could we know `ma = Nothing`?  Since `ma` is defined as `f (fromJust ma)`, it must be that this expression has already been evaluated to `Nothing` -- in which case there is no reason for us to be evaluating `fromJust ma` in the first place!

To see this from another point of view, we can consider three possibilities. First, if `f` outputs `Nothing` without looking at its argument, then `maybeFix f` clearly returns `Nothing`.  Second, if `f` always outputs `Just x`, where `x` depends on its argument, then the recursion can proceed usefully: `fromJust ma` will be able to evaluate to `x`, thus feeding `f`'s output back to it as input.  Third, if `f` tries to use its argument to decide whether to output `Just` or `Nothing`, then `maybeFix f` will not terminate: evaluating `f`'s argument requires evaluating `ma` to see whether it is `Just`, which requires evaluating `f (fromJust ma)`, which requires evaluating `ma`, ... and so on.

There are also instances of `MonadFix` for lists (which works analogously to the instance for `Maybe`), for `ST`, and for `IO`.  The [instance for `IO`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO.html#fixIO) is particularly amusing: it creates a new (empty) `MVar`, immediately reads its contents using `unsafeInterleaveIO` (which delays the actual reading lazily until the value is needed), uses the contents of the `MVar` to compute a new value, which it then writes back into the `MVar`.  It almost seems, spookily, that `mfix` is sending a value back in time to itself through the `MVar` -- though of course what is really going on is that the reading is delayed just long enough (via `unsafeInterleaveIO`) to get the process bootstrapped.

### Exercises
- Implement a `MonadFix` instance for `[]`.

## `mdo` syntax

The example at the start of this section can also be written

```haskell
mdo { x <- foo
    ; y <- baz
    ; z <- bar
    ;      bob
    ; w <- frob
    }
```

which will be translated into the original example (assuming that, say, `bar` and `bob` refer to `y`.  The difference is that `mdo` will analyze the code in order to find minimal recursive blocks, which will be placed in `rec` blocks, whereas `rec` blocks desugar directly into calls to `mfix` without any further analysis.

## Further reading

For more information (such as the precise desugaring rules for `rec` blocks), see Levent Erkök and John Launchbury's 2002 Haskell workshop paper, [A Recursive do for Haskell](http://sites.google.com/site/leventerkok/recdo.pdf?attredirects=0), or for full details, Levent Erkök’s thesis, [Value Recursion in Monadic Computations](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.15.1543&rep=rep1&type=pdf).  (Note, while reading, that `MonadFix` used to be called `MonadRec`.)  You can also read the [GHC user manual section on recursive do-notation](http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#recursive-do-notation).

# Semigroup

A semigroup is a set $S$ together with a binary operation $\oplus$ which
combines elements from $S$. The $\oplus$ operator is required to be associative
(that is, $(a \oplus b) \oplus c = a \oplus (b \oplus c)$, for any
$a,b,c$ which are elements of $S$).

For example, the natural numbers under addition form a semigroup: the sum of any two natural numbers is a natural number, and $(a+b)+c = a+(b+c)$ for any natural numbers $a$, $b$, and $c$. The integers under multiplication also form a semigroup, as do the integers (or rationals, or reals) under $\max$ or $\min$, Boolean values under conjunction and disjunction, lists under concatenation, functions from a set to itself under composition ... Semigroups show up all over the place, once you know to look for them.

## Definition

As of version 4.9 of the `base` package (which comes with GHC 8.0), semigroups are defined in the `Data.Semigroup` module.  (If you are working with a previous version of base, or want to write a library that supports previous versions of base, you can use the `semigroups` package.)

The definition of the `Semigroup` type class ([haddock](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html)) is as follows:

```haskell
class Semigroup a where
  (<>) :: a -> a -> a

  sconcat :: NonEmpty a -> a
  sconcat = sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b

  stimes :: Integral b => b -> a -> a
  stimes = ...
```

The really important method is `(<>)`, representing the associative binary operation.  The other two methods have default implementations in terms of `(<>)`, and are included in the type class in case some instances can give more efficient implementations than the default.

`sconcat` reduces a nonempty list using `(<>)`. For most instances, this is the same as `foldr1 (<>)`, but it can be constant-time for idempotent semigroups.

`stimes n` is equivalent to (but sometimes considerably more efficient than) `sconcat . replicate n`. Its default definition uses multiplication by doubling (also known as exponentiation by squaring). For many semigroups, this is an important optimization; for some, such as lists, it is terrible and must be overridden.

See the [haddock documentation](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html) for more information on `sconcat` and `stimes`.

## Laws

The only law is that `(<>)` must be associative:

```haskell
(x <> y) <> z = x <> (y <> z)
```

# Monoid

Many semigroups have a special element $e$ for which the binary operation $\oplus$ is the identity, that is, $e \oplus x = x \oplus e = x$ for every element $x$.  Such a semigroup-with-identity-element is called a *monoid*.

## Definition

The definition of the `Monoid` type class (defined in
`Data.Monoid`; [haddock](https://hackage.haskell.org/package/base/docs/Data-Monoid.html)) is:

```haskell
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
```

The `mempty` value specifies the identity element of the monoid, and `mappend`
is the binary operation.  The default definition for `mconcat`
“reduces” a list of elements by combining them all with `mappend`,
using a right fold. It is only in the `Monoid` class so that specific
instances have the option of providing an alternative, more efficient
implementation; usually, you can safely ignore `mconcat` when creating
a `Monoid` instance, since its default definition will work just fine.

The `Monoid` methods are rather unfortunately named; they are inspired
by the list instance of `Monoid`, where indeed `mempty = []` and `mappend = (++)`, but this is misleading since many
monoids have little to do with appending (see these [Comments from OCaml Hacker Brian Hurt](http://thread.gmane.org/gmane.comp.lang.haskell.cafe/50590) on the Haskell-cafe mailing list). The situation is made somewhat better by `(<>)`, which is provided as an alias for `mappend`.

Note that the `(<>)` alias for `mappend` conflicts with the `Semigroup` method of the same name.  For this reason, `Data.Semigroup` re-exports much of `Data.Monoid`; to use semigroups and monoids together, just import `Data.Semigroup`, and make sure all your types have both `Semigroup` and `Monoid` instances (and that `(<>) = mappend`).

## Laws

Of course, every `Monoid` instance should actually be a monoid in the
mathematical sense, which implies these laws:

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

## Instances

There are quite a few interesting `Monoid` instances defined in `Data.Monoid`.

- `[a]` is a `Monoid`, with `mempty = []` and `mappend = (++)`. It is not hard to check that `(x ++ y) ++ z = x ++ (y ++ z)` for any lists `x`, `y`, and `z`, and that the empty list is the identity: `[] ++ x = x ++ [] = x`.

- As noted previously, we can make a monoid out of any numeric type under either addition or multiplication.  However, since we can’t have two instances for the same type, `Data.Monoid` provides two `newtype` wrappers, `Sum` and `Product`, with appropriate `Monoid` instances.

    ```haskell
    > getSum (mconcat . map Sum $ [1..5])
    15
    > getProduct (mconcat . map Product $ [1..5])
    120
    ```

    This example code is silly, of course; we could just write
    `sum [1..5]` and `product [1..5]`.  Nevertheless, these instances are useful in more generalized settings, as we will see in the [section on `Foldable`](http://www.haskell.org/haskellwiki/Foldable).

- `Any` and `All` are `newtype` wrappers providing `Monoid` instances for `Bool` (under disjunction and conjunction, respectively).

-  There are three instances for `Maybe`: a basic instance which lifts a `Monoid` instance for `a` to an instance for `Maybe a`, and two `newtype` wrappers `First` and `Last` for which `mappend` selects the first (respectively last) non-`Nothing` item.

- `Endo a` is a newtype wrapper for functions `a -> a`, which form a monoid under composition.

- There are several ways to “lift” `Monoid` instances to instances with additional structure.  We have already seen that an instance for `a` can be lifted to an instance for `Maybe a`.  There are also tuple instances: if `a` and `b` are instances of `Monoid`, then so is `(a,b)`, using the monoid operations for `a` and `b` in the obvious pairwise manner. Finally, if `a` is a `Monoid`, then so is the function type `e -> a` for any `e`; in particular, ``g `mappend` h`` is the function which applies both `g` and `h` to its argument and then combines the results using the underlying `Monoid` instance for `a`.  This can be quite useful and elegant (see [example](http://thread.gmane.org/gmane.comp.lang.haskell.cafe/52416)).

- The type `Ordering = LT | EQ | GT` is a `Monoid`, defined in such a way that `mconcat (zipWith compare xs ys)` computes the lexicographic ordering of `xs` and `ys` (if `xs` and `ys` have the same length).  In particular, `mempty = EQ`, and `mappend` evaluates to its leftmost non-`EQ` argument (or `EQ` if both arguments are `EQ`).  This can be used together with the function instance of `Monoid` to do some clever things ([example](http://www.reddit.com/r/programming/comments/7cf4r/monoids_in_my_programming_language/c06adnx)).

- There are also `Monoid` instances for several standard data structures in the containers library  ([haddock](http://hackage.haskell.org/packages/archive/containers/0.2.0.0/doc/html/index.html)), including `Map`, `Set`, and `Sequence`.

`Monoid` is also used to enable several other type class instances.
As noted previously, we can use `Monoid` to make `((,) e)` an instance of `Applicative`:

```haskell
instance Monoid e => Applicative ((,) e) where
  pure :: Monoid e => a -> (e,a)
  pure x = (mempty, x)

  (<*>) :: Monoid e => (e,a -> b) -> (e,a) -> (e,b)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
```

`Monoid` can be similarly used to make `((,) e)` an instance of `Monad` as well; this is known as the *writer monad*.  As we’ve already seen, `Writer` and `WriterT` are a newtype wrapper and transformer for this monad, respectively.

`Monoid` also plays a key role in the `Foldable` type class (see section [Foldable](http://www.haskell.org/haskellwiki/#Foldable)).

## Further reading

Monoids got a fair bit of attention in 2009, when
[a blog post by Brian Hurt](http://blog.enfranchisedmind.com/2009/01/random-thoughts-on-haskell/)
complained about the fact that the names of many Haskell type classes
(`Monoid` in particular) are taken from abstract mathematics.  This
resulted in [a long Haskell-cafe thread](http://thread.gmane.org/gmane.comp.lang.haskell.cafe/50590)
arguing the point and discussing monoids in general.

However, this was quickly followed by several blog posts about
`Monoid` ^[May its name live forever.].  First, Dan Piponi
wrote a great introductory post, [Haskell Monoids and their Uses](http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html).  This was quickly followed by
Heinrich Apfelmus’ [Monoids and Finger Trees](http://apfelmus.nfshost.com/monoid-fingertree.html), an accessible exposition of
Hinze and Paterson’s [classic paper on 2-3 finger trees](http://www.soi.city.ac.uk/%7Eross/papers/FingerTree.html), which makes very clever
use of `Monoid` to implement an elegant and generic data structure.
Dan Piponi then wrote two fascinating articles about using `Monoids`
(and finger trees): [Fast Incremental Regular Expressions](http://blog.sigfpe.com/2009/01/fast-incremental-regular-expression.html) and [Beyond Regular Expressions](http://blog.sigfpe.com/2009/01/beyond-regular-expressions-more.html)

In a similar vein, David Place’s article on improving `Data.Map` in
order to compute incremental folds (see [the Monad Reader issue 11](http://www.haskell.org/wikiupload/6/6a/TMR-Issue11.pdf))
is also a
good example of using `Monoid` to generalize a data structure.

Some other interesting examples of `Monoid` use include [building elegant list sorting combinators](http://www.reddit.com/r/programming/comments/7cf4r/monoids_in_my_programming_language/c06adnx), [collecting unstructured information](http://byorgey.wordpress.com/2008/04/17/collecting-unstructured-information-with-the-monoid-of-partial-knowledge/), [combining probability distributions](http://izbicki.me/blog/gausian-distributions-are-monoids), and a brilliant series of posts by Chung-Chieh Shan and Dylan Thurston using `Monoid`s to [elegantly solve a difficult combinatorial puzzle](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers1/) (followed by [part 2](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers2/), [part 3](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers3/), [part 4](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers4/)).

As unlikely as it sounds, monads can actually be viewed as a sort of
monoid, with `join` playing the role of the binary operation and
`return` the role of the identity; see [Dan Piponi’s blog post](http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html).

# Failure and choice: Alternative, MonadPlus, ArrowPlus

Several classes (`Applicative`, `Monad`, `Arrow`) have "monoidal" subclasses, intended to model computations that support "failure" and "choice" (in some appropriate sense).

## Definition

The `Alternative` type class ([haddock](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#g:2))
is for `Applicative` functors which also have
a monoid structure:

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  many :: f a -> f [a]
```

The basic intuition is that `empty` represents some sort of "failure", and `(<|>)` represents a choice between alternatives.  (However, this intuition does not fully capture the nuance possible; see the section on Laws below.)  Of course, `(<|>)` should be associative and `empty` should be the identity element for it.
Instances of `Alternative` must implement `empty` and `(&lt;|&gt;)`; `some` and `many` have default implementations but are included in the class since specialized implementations may be more efficient than the default.

The default definitions of `some` and `many` are essentially given by

```haskell
some v = (:) <$> v <*> many v
many v = some v <|> pure []
```

(though for some reason, in actual fact they are not defined via mutual recursion).  The intuition is that both keep running `v`, collecting its results into a list, until it fails; `some v` requires `v` to succeed at least once, whereas `many v` does not require it to succeed at all.  That is, `many` represents 0 or more repetitions of `v`, whereas `some` represents 1 or more repetitions.  Note that `some` and `many` do not make sense for all instances of `Alternative`; they are discussed further below.

Likewise, `MonadPlus` ([haddock](https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:MonadPlus))
is for `Monad`s with a monoid structure:

```haskell
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
```

Finally, `ArrowZero` and `ArrowPlus` ([haddock](https://hackage.haskell.org/package/base/docs/Control-Arrow.html#t:ArrowZero))
represent `Arrow`s ([see below](http://www.haskell.org/haskellwiki/#Arrow)) with a
monoid structure:

```haskell
class Arrow arr => ArrowZero arr where
  zeroArrow :: b `arr` c

class ArrowZero arr => ArrowPlus arr where
  (<+>) :: (b `arr` c) -> (b `arr` c) -> (b `arr` c)
```

## Instances

Although this document typically discusses laws before presenting example instances, for `Alternative` and friends it is worth doing things the other way around, because there is some controversy over the laws and it helps to have some concrete examples in mind when discussing them.  We mostly focus on `Alternative` in this section and the next; now that `Applicative` is a superclass of `Monad`, there is little reason to use `MonadPlus` any longer, and `ArrowPlus` is rather obscure.

* `Maybe` is an instance of `Alternative`, where `empty` is `Nothing` and the choice operator `(<|>)` results in its first argument when it is `Just`, and otherwise results in its second argument.  Hence folding over a list of `Maybe` with `(<|>)` (which can be done with `asum` from `Data.Foldable`) results in the first non-`Nothing` value in the list (or `Nothing` if there are none).

* `[]` is an instance, with `empty` given by the empty list, and `(<|>)` equal to `(++)`.  It is worth pointing out that this is identical to the `Monoid` instance for `[a]`, whereas the `Alternative` and `Monoid` instances for `Maybe` are different: the `Monoid` instance for `Maybe a` requires a `Monoid` instance for `a`, and monoidally combines the contained values when presented with two `Just`s.

Let's think about the behavior of `some` and `many` for `Maybe` and `[]`.  For `Maybe`, we have `some Nothing = (:) <$> Nothing <*> many Nothing = Nothing <*> many Nothing = Nothing`.  Hence we also have `many Nothing = some Nothing <|> pure [] = Nothing <|> pure [] = pure [] = Just []`.  Boring.  But what about applying `some` and `many` to `Just`?  In fact, `some (Just a)` and `many (Just a)` are both bottom!  The problem is that since `Just a` is always "successful", the recursion will never terminate.  In theory the result "should be" the infinite list `[a,a,a,...]` but it cannot even start producing any elements of this list, because there is no way for the `(<*>)` operator to yield any output until it knows that the result of the call to `many` will be `Just`.

You can work out the behavior for `[]` yourself, but it ends up being quite similar: `some` and `many` yield boring results when applied to the empty list, and yield bottom when applied to any non-empty list.

In the end, `some` and `many` really only make sense when used with some sort of "stateful" `Applicative` instance, for which an action `v`, when run multiple times, can succeed some finite number of times and then fail.  For example, parsers have this behavior, and indeed, parsers were the original motivating example for the `some` and `many` methods; more on this below.

* Since GHC 8.0 (that is, `base-4.9`), there is an instance of `Alternative` for `IO`.  `empty` throws an I/O exception, and `(<|>)` works by first running its left-hand argument; if the left-hand argument throws an I/O exception, `(<|>)` catches the exception and then calls its second argument.  (Note that other types of exceptions are not caught.)  There are other, much better ways to handle I/O errors, but this is a quick and dirty way that may work for simple, one-off programs, such as expressions typed at the GHCi prompt.  For example, if you want to read the contents of a file but use some default contents in case the file does not exist, you can just write `readFile "somefile.txt" <|> return "default file contents"`.

* `Concurrently` from the `async` package has an `Alternative` instance, for which `c1 <|> c2` races `c1` and `c2` in parallel, and returns the result of whichever finishes first.  `empty` corresponds to the action that runs forever without returning a value.

* Practically any parser type (e.g. from `parsec`, `megaparsec`, `trifecta`, ...) has an `Alternative` instance, where `empty` is an unconditional parse failure, and `(<|>)` is left-biased choice.  That is, `p1 <|> p2` first tries parsing with `p1`, and if `p1` fails then it tries `p2` instead.

`some` and `many` work particularly well with parser types having an `Applicative` instance: if `p` is a parser, then `some p` parses one or more consecutive occurrences of `p` (i.e. it will parse as many occurrences of `p` as possible and then stop), and `many p` parses zero or more occurrences.

## Laws

Of course, instances of `Alternative` should satisfy the monoid laws

```haskell
empty <|> x = x
x <|> empty = x
(x <|> y) <|> z = x <|> (y <|> z)
```

The documentation for `some` and `many` states that they should be the "least solution" (i.e. least in the definedness partial order) to their characterizing, mutually recursive default definitions.  However, [this is  controversial](https://www.reddit.com/r/haskell/comments/2j8bvl/laws_of_some_and_many/), and probably wasn't really thought out very carefully.

Since `Alternative` is a subclass of `Applicative`, a natural question is, "how should `empty` and `(<|>)` interact with `(<*>)` and `pure`?"

First of all, everyone agrees on the *left zero* law:

```haskell
empty <*> f = empty
```

After this is where it starts to get a bit hairy though.  It turns out there are several other laws one might imagine adding, and different instances satisfy different laws.

* *Right Zero*:<p>Another obvious law would be `f <*> empty = empty`{.haskell}</p><p>This law is satisfied by most instances; however, it is not satisfied by `IO`. Once the effects in `f` have been executed, there is no way to roll them back if we later encounter an exception.</p>

* *Left Distribution*:<p>`(a <|> b) <*> c = (a <*> c) <|> (b <*> c)`{.haskell}</p><p>This distributivity law is satisfied by `[]` and `Maybe`, as you may verify.  However, it is *not* satisfied by `IO` or most parsers.  The reason is that `a` and `b` can have effects which influence execution of `c`, and the left-hand side may end up failing where the right-hand side succeeds.</p><p>For example, consider `IO`, and suppose that `a` always executes successfully, but `c` throws an I/O exception after `a` has run.  Concretely, say, `a` might ensure that a certain file does not exist (deleting it if it does exist or doing nothing if it does not), and then `c` tries to read that file.  In that case `(a <|> b) <*> c` will first delete the file, ignoring `b` since `a` is successful, and then throw an exception when `c` tries to read the file.  On the other hand, `b` might ensure that the same file in question *does* exist.  In that case `(a <*> c) <|> (b <*> c)` would succeed: after `(a <*> c)` throws an exception, it would be caught by `(<|>)`, and then `(b <*> c)` would be tried.</p><p>This law does not hold for parsers for a similar reason: `(a <|> b) <*> c` has to "commit" to parsing with `a` or `b` before running `c`, whereas `(a <*> c) <|> (b <*> c)` allows backtracking if `a <*> c` fails.  In the particular case that `a` succeeds but `c` fails after `a` but not after `b`, these may give different results.  For example, suppose `a` and `c` both expect to see two asterisks, but `b` expects to see only one.  If there are only three asterisks in the input, `b <*> c` will be successful whereas `a <*> c` will not.</p>

* *Right Distribution*:<p>`a <*> (b <|> c) = (a <*> b) <|> (a <*> c)`{.haskell}</p><p>This law is not satisfied by very many instances, but it's still worth discussing.  In particular the law is still satisfied by `Maybe`.  However, it is *not* satisfied by, for example, lists.  The problem is that the results come out in a different order.  For example, suppose `a = [(+1), (*10)]`, `b = [2]`, and `c = [3]`.  Then the left-hand side yields `[3,4,20,30]`, whereas the right-hand side is `[3,20,4,30]`.</p><p>`IO` does not satisfy it either, since, for example, `a` may succeed only the *second* time it is executed.  Parsers, on the other hand, may or may not satisfy this law, depending on how they handle backtracking.  Parsers for which `(<|>)` by itself does full backtracking will satisfy the law; but for many parser combinator libraries this is not the case, for efficiency reasons.  For example, parsec fails this law: if `a` succeeds while consuming some input, and afterwards `b` fails without consuming any input, then the left-hand side may succeed while the right-hand side fails: after `(a <*> b)` fails, the right-hand side tries to re-run `a` without backtracking over the input the original `a` consumed.</p>

* *Left Catch*:<p>`(pure a) <|> x = pure a`{.haskell}</p><p>Intuitively, this law states that `pure` should always represent a "successful" computation.  It is satisfied by `Maybe`, `IO`, and parsers.  However, it is not satisfied by lists, since lists collect *all* possible results: it corresponds to `[a] ++ x == [a]` which is obviously false.</p>

This, then, is the situation: we have a lot of instances of `Alternative` (and `MonadPlus`), with each instance satisfying some *subset* of these laws.  Moreover, it's not always the *same* subset, so there is no obvious "default" set of laws to choose.  For now at least, we just have to live with the situation.  When using a particular instance of `Alternative` or `MonadPlus`, it's worth thinking carefully about which laws it satisfies.

## Utility functions

There are a few `Alternative`-specific utility functions worth mentioning:

* `guard :: Alternative f => Bool -> f ()`{.haskell} checks the given condition, and evaluates to `pure ()` if the condition holds, and `empty` if not.  This can be used to create a conditional failure point in the middle of a computation, where the computation only proceeds if a certain condition holds.

* `optional :: Alternative f => f a -> f (Maybe a)`{.haskell} reifies potential failure into the `Maybe` type: that is, `optional x` is a computation which always succeeds, returning `Nothing` if `x` fails and `Just a` if `x` successfully results in `a`.  It is useful, for example, in the context of parsers, where it corresponds to a production which can occur zero or one times.

## Further reading

There used to be a type class called `MonadZero` containing only
`mzero`, representing monads with failure.  The `do`-notation requires
some notion of failure to deal with failing pattern matches.
Unfortunately, `MonadZero` was scrapped in favor of adding the `fail`
method to the `Monad` class. If we are lucky, someday `MonadZero` will
be restored, and `fail` will be banished to the bit bucket where it
belongs (see [MonadPlus reform proposal](http://www.haskell.org/haskellwiki/MonadPlus reform proposal)).  The idea is that any
`do`-block which uses pattern matching (and hence may fail) would require
a `MonadZero` constraint; otherwise, only a `Monad` constraint would be
required.

A great introduction to the `MonadPlus` type class, with interesting examples of its use, is Doug Auclair’s *MonadPlus: What a Super Monad!* in [the Monad.Reader issue 11](http://www.haskell.org/wikiupload/6/6a/TMR-Issue11.pdf).

Another interesting use of `MonadPlus` can be found in Christiansen et al, [All Sorts of Permutations](http://www-ps.informatik.uni-kiel.de/~sad/icfp2016-preprint.pdf), from ICFP 2016.

The [logict package](https://hackage.haskell.org/package/logict) defines a type with prominent `Alternative` and `MonadPlus` instances that can be used to efficiently enumerate possibilities subject to constraints, *i.e.* logic programming; it's like the list monad on steroids.

# Foldable

The `Foldable` class, defined in the `Data.Foldable`
module ([haddock](https://hackage.haskell.org/package/base/docs/Data-Foldable.html)), abstracts over containers which can be
“folded” into a summary value.  This allows such folding operations
to be written in a container-agnostic way.

## Definition

The definition of the `Foldable` type class is:

```haskell
class Foldable t where
  fold    :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldr'  :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (b -> a -> b) -> b -> t a -> b
  foldl'  :: (b -> a -> b) -> b -> t a -> b
  foldr1  :: (a -> a -> a) -> t a -> a
  foldl1  :: (a -> a -> a) -> t a -> a
  toList  :: t a -> [a]
  null    :: t a -> Bool
  length  :: t a -> Int
  elem    :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum     :: Num a => t a -> a
  product :: Num a => t a -> a
```

This may look complicated, but in fact, to make a `Foldable` instance
you only need to implement one method: your choice of `foldMap` or
`foldr`.  All the other methods have default implementations in terms
of these, and are included in the class in case more
efficient implementations can be provided.

## Instances and examples

The type of `foldMap` should make it clear what it is supposed to do:
given a way to convert the data in a container into a `Monoid` (a
function `a -> m`) and a container of `a`’s (`t a`), `foldMap`
provides a way to iterate over the entire contents of the container,
converting all the `a`’s to `m`’s and combining all the `m`’s with
`mappend`. The following code shows two examples: a simple
implementation of `foldMap` for lists, and a binary tree example
provided by the `Foldable` documentation.

```haskell
instance Foldable [] where
  foldMap :: Monoid m => (a -> m) -> [a] -> m
  foldMap g = mconcat . map g

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
```

The `foldr` function has a type similar to the `foldr` found in the `Prelude`, but
more general, since the `foldr` in the `Prelude` works only on lists.

The `Foldable` module also provides instances for `Maybe` and `Array`;
additionally, many of the data structures found in the standard [containers library](http://hackage.haskell.org/package/containers) (for example, `Map`, `Set`, `Tree`,
and `Sequence`) provide their own `Foldable` instances.

### Exercises

#. Implement `fold` in terms of `foldMap`.
#. What would you need in order to implement `foldMap` in terms of `fold`?
#. Implement `foldMap` in terms of `foldr`.
#. Implement `foldr` in terms of `foldMap` (hint: use the `Endo` monoid).
#. What is the type of `foldMap . foldMap`?  Or `foldMap . foldMap . foldMap`, etc.?  What do they do?

## Derived folds

Given an instance of `Foldable`, we can write generic,
container-agnostic functions such as:

```haskell
-- Compute the size of any container.
containerSize :: Foldable f => f a -> Int
containerSize = getSum . foldMap (const (Sum 1))

-- Compute a list of elements of a container satisfying a predicate.
filterF :: Foldable f => (a -> Bool) -> f a -> [a]
filterF p = foldMap (\a -> if p a then [a] else [])

-- Get a list of all the Strings in a container which include the
-- letter a.
aStrings :: Foldable f => f String -> [String]
aStrings = filterF (elem 'a')
```

The `Foldable` module also provides a large number of predefined
folds.  These used to be generalized versions of `Prelude` functions of the
same name that only worked on lists; but [as of GHC 7.10](https://wiki.haskell.org/Foldable_Traversable_In_Prelude), the generalized versions themselves are now exported from the Prelude: for example, `concat`, `concatMap`, `and`,
`or`, `any`, `all`, `sum`, `product`, `maximum`(`By`),
`minimum`(`By`), `elem`, `notElem`, and `find`.  For example, before GHC 7.10, `length` used to have type `length :: [a] -> Int`; now it has type `Foldable t => t a -> Int` (and is in fact the same as the `containerSize` function shown above).

The important function `toList` is also provided, which turns any `Foldable` structure into a list of its elements in left-right order; it works by folding with the list monoid.

There are also generic functions that work with `Applicative` or
`Monad` instances to generate some sort of computation from each
element in a container, and then perform all the side effects from
those computations, discarding the results: `traverse_`, `sequenceA_`,
and others.  The results must be discarded because the `Foldable`
class is too weak to specify what to do with them: we cannot, in
general, make an arbitrary `Applicative` or `Monad` instance into a `Monoid`, but we can make `m ()` into a `Monoid` for any such `m`.  If we do have an `Applicative` or `Monad` with a monoid
structure—that is, an `Alternative` or a `MonadPlus`—then we can
use the `asum` or `msum` functions, which can combine the results as
well.  Consult the [`Foldable` documentation](https://hackage.haskell.org/package/base/docs/Data-Foldable.html) for
more details on any of these functions.

Note that the `Foldable` operations always forget the structure of
the container being folded.  If we start with a container of type `t a` for some `Foldable t`, then `t` will never appear in the output
type of any operations defined in the `Foldable` module.  Many times
this is exactly what we want, but sometimes we would like to be able
to generically traverse a container while preserving its
structure—and this is exactly what the `Traversable` class provides,
which will be discussed in the next section.

### Exercises

#. Implement `toList :: Foldable f => f a -> [a]` in terms of either `foldr` or `foldMap`.
#. Show how one could implement the generic version of `foldr` in terms of `toList`, assuming we had only the list-specific `foldr :: (a -> b -> b) -> b -> [a] -> b`.
#. Pick some of the following functions to implement: `concat`, `concatMap`, `and`, `or`, `any`, `all`, `sum`, `product`, `maximum`(`By`), `minimum`(`By`), `elem`, `notElem`, and `find`.  Figure out how they generalize to `Foldable` and come up with elegant implementations using `fold` or `foldMap` along with appropriate `Monoid` instances.
}}

## Utility functions

* `asum :: (Alternative f, Foldable t) => t (f a) -> f a` takes a container full of computations and combines them using `(<|>)`.

* `sequenceA_ :: (Applicative f, Foldable t) => t (f a) -> f ()` takes a container full of computations and runs them in sequence, discarding the results (that is, they are used only for their effects).  Since the results are discarded, the container only needs to be `Foldable`. (Compare with `sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)`, which requires a stronger `Traversable` constraint in order to be able to reconstruct a container of results having the same shape as the original container.)

* `traverse_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()` applies the given function to each element in a foldable container and sequences the effects (but discards the results).

* `for_` is the same as `traverse_` but with its arguments flipped.  This is the moral equivalent of a "foreach" loop in an imperative language.

* For historical reasons, there are also variants of all the above with overly-restrictive `Monad`(-like) constraints: `msum` is the same as `asum` specialized to `MonadPlus`, and `sequence_`, `mapM_`, and `forM_` respectively are `Monad` specializations of `sequenceA_`, `traverse_`, and `for_`.

### Exercises

- Implement `traverse_` in terms of `sequenceA_` and vice versa. One of these will need an extra constraint. What is it?

## Foldable actually isn't

The generic term "fold" is often used to refer to the more technical concept of [catamorphism](http://www.haskell.org/haskellwiki/Catamorphisms). Intuitively, given a way to summarize "one level of structure" (where recursive subterms have already been replaced with their summaries), a catamorphism can summarize an entire recursive structure.  It is important to realize that `Foldable` does *not* correspond to catamorphisms, but to something weaker.  In particular, `Foldable` allows observing only the left-right traversal order of elements within a structure, not the actual structure itself.  Put another way, every use of `Foldable` can be expressed in terms of `toList`.  For example, `fold` itself is equivalent to `mconcat . toList`.

This is sufficient for many tasks, but not all.  For example, consider trying to compute the depth of a `Tree`: try as we might, there is no way to implement it using `Foldable`.  However, it *can* be implemented as a catamorphism.

## Further reading

The `Foldable` class had its genesis in [McBride and Paterson’s paper](http://www.soi.city.ac.uk/~ross/papers/Applicative.html)
introducing `Applicative`, although it has
been fleshed out quite a bit from the form in the paper.

An interesting use of `Foldable` (as well as `Traversable`) can be
found in Janis Voigtländer’s paper [Bidirectionalization for free!](http://doi.acm.org/10.1145/1480881.1480904).

For more on the relationship between `fold`, `foldMap`, and `foldr`, see [foldr is made of monoids](https://byorgey.wordpress.com/2012/11/05/foldr-is-made-of-monoids/).

There was [quite a bit of controversy](http://tojans.me/blog/2015/10/13/foldable-for-non-haskellers-haskells-controversial-ftp-proposal/) in the Haskell community about a [proposal to integrate `Foldable` (and `Traversable`) more tightly into the Prelude](https://wiki.haskell.org/Foldable_Traversable_In_Prelude), known as the FTP. Some of the controversy centered around `Foldable` instances such as the one for `((,) a)`, which, together with generalized types for functions such as `length :: Foldable t => t a -> Int`, allow one to derive seemingly nonsensical results such as `length (2,3) = 1`. Here is a [humorous talk](https://www.youtube.com/watch?v=87re_yIQMDw) poking fun at the situation.

# Traversable

## Definition

The `Traversable` type class, defined in the `Data.Traversable`
module ([haddock](https://hackage.haskell.org/package/base/docs/Data-Traversable.html)), is:

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM      ::       Monad m => (a -> m b) -> t a -> m (t b)
  sequence  ::       Monad m => t (m a) -> m (t a)
```

As you can see, every `Traversable` is also a `Foldable` `Functor`. To make a `Traversable` instance, it suffices to implement either `traverse` or
`sequenceA`; the other methods all have default implementations in
terms of these.  Note that `mapM` and `sequence` only exist for historical reasons; especially now that `Applicative` is a superclass of `Monad`, they are nothing more than copies of `traverse` and `sequenceA`, respectively, but with more restrictive types.

## Intuition


The key method of the `Traversable` class is `traverse`, which has the following type:
```haskell
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```
This leads us to view `Traversable` as a generalization of `Functor`.  `traverse` is an "effectful `fmap`": it allows us to map over a structure of type `t a`, applying a function to every element of type `a` in order to produce a new structure of type `t b`; but along the way the function may have some effects (captured by the applicative functor `f`).

Alternatively, we may consider the `sequenceA` function.  Consider its type:
```haskell
  sequenceA :: Applicative f => t (f a) -> f (t a)
```
This answers the fundamental question: when can we commute two
functors?  For example, can we turn a tree of lists into a list of
trees?

The ability to compose two monads depends crucially on this ability to
commute functors. Intuitively, if we want to build a composed monad
`M a = m (n a)` out of monads `m` and `n`, then to be able to
implement `join :: M (M a) -> M a`, that is,
`join :: m (n (m (n a))) -> m (n a)`, we have to be able to commute
the `n` past the `m` to get `m (m (n (n a)))`, and then we can use the
`join`s for `m` and `n` to produce something of type `m (n a)`.  See
[Mark Jones’ paper](http://web.cecs.pdx.edu/~mpj/pubs/springschool.html) for more details.

It turns out that given a `Functor` constraint on the type `t`, `traverse` and `sequenceA` are equivalent in power: either can be implemented in terms of the other.

### Exercises

#. There are at least two natural ways to turn a tree of lists into a list of trees.  What are they, and why?
#. Give a natural way to turn a list of trees into a tree of lists.
#. What is the type of `traverse . traverse`? What does it do?
#. Implement `traverse` in terms of `sequenceA`, and vice versa.

## Instances and examples

What’s an example of a `Traversable` instance?
The following code shows an example instance for the same
`Tree` type used as an example in the previous `Foldable` section.  It
is instructive to compare this instance with a `Functor` instance for
`Tree`, which is also shown.

```haskell
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Empty        = pure Empty
  traverse g (Leaf x)     = Leaf <$> g x
  traverse g (Node l x r) = Node <$> traverse g l
                                 <*> g x
                                 <*> traverse g r

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap     g Empty        = Empty
  fmap     g (Leaf x)     = Leaf $ g x
  fmap     g (Node l x r) = Node (fmap g l)
                                 (g x)
                                 (fmap g r)
```

It should be clear that the `Traversable` and `Functor` instances for
`Tree` are structurally identical; the only difference is that the `Functor`
instance involves normal function application, whereas the
applications in the `Traversable` instance take place within an
`Applicative` context, using `(<$>)` and `(<*>)`.  This same pattern will hold for any type.

Any `Traversable` functor is also `Foldable`, and a `Functor`.  We can see
this not only from the class declaration, but by the fact that we can
implement the methods of both classes given only the `Traversable`
methods.

The standard libraries provide a number of `Traversable` instances,
including instances for `[]`, `ZipList`, `Maybe`, `((,) e)`, `Sum`, `Product`, `Either e`, `Map`, `Tree`, and `Sequence`.
Notably, `Set` is not `Traversable`, although it is `Foldable`.

### Exercises

#. Implement `fmap` and `foldMap` using only the `Traversable` methods.  (Note that the `Traversable` module provides these implementations as `fmapDefault` and `foldMapDefault`.)
#. Implement `Traversable` instances for `[]`, `Maybe`, `((,) e)`, and `Either e`.
#. Explain why `Set` is `Foldable` but not `Traversable`.
#. Show that `Traversable` functors compose: that is, implement an instance for `Traversable (Compose f g)` given `Traversable` instances for `f` and `g`.

## Laws

Any instance of `Traversable` must satisfy the following two laws, where `Identity` is the identity functor (as defined in the [`Data.Functor.Identity` module](http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Data-Functor-Identity.html) from the `transformers` package), and `Compose` wraps the composition of two functors (as defined in [`Data.Functor.Compose`](http://hackage.haskell.org/packages/archive/transformers/0.3.0.0/doc/html/Data-Functor-Compose.html)):

#. `traverse Identity = Identity`
#. `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

The first law essentially says that traversals cannot make up arbitrary effects.  The second law explains how doing two traversals in sequence can be collapsed to a single traversal.

Additionally, suppose `eta` is an "`Applicative` morphism", that is,
```haskell
  eta :: forall a f g. (Applicative f, Applicative g) => f a -> g a
```
and `eta` preserves the `Applicative` operations: `eta (pure x) = pure x` and `eta (x <*> y) = eta x <*> eta y`.  Then, by parametricity, any instance of `Traversable` satisfying the above two laws will also satisfy `eta . traverse f = traverse (eta . f)`.

## Further reading

The `Traversable` class also had its genesis in [McBride and Paterson’s `Applicative` paper](http://www.soi.city.ac.uk/~ross/papers/Applicative.html),
and is described in more detail in Gibbons and Oliveira, [The Essence of the Iterator Pattern](http://www.comlab.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf),
which also contains a wealth of references to related work.

`Traversable` forms a core component of Edward Kmett's [lens library](http://hackage.haskell.org/package/lens).  Watching [Edward's talk on the subject](https://vimeo.com/56063074) is a highly recommended way to gain better insight into `Traversable`, `Foldable`, `Applicative`, and many other things besides.

For references on the `Traversable` laws, see Russell O'Connor's [mailing list post](http://article.gmane.org/gmane.comp.lang.haskell.libraries/17778) (and subsequent thread), and [this paper by Jaskelioff and Rypacek](https://arxiv.org/abs/1202.2919) for a more in-depth discussion.  Daniel Mlot also has [this very nice blog post](http://duplode.github.io/posts/traversable-a-remix.html) explaining how `Traversable` arises by considering a variant on the usual Kleisli category of a monad, which also sheds light on where the `Traversable` laws come from.

[This blog post by Will Fancher](http://elvishjerricco.github.io/2017/03/23/applicative-sorting.html) shows how to use `Traversable` along with a clever choice of `Applicative` to efficiently sort any `Traversable` container.

# Category

`Category` is a relatively recent addition to the Haskell standard libraries.  It generalizes the notion of function composition to general “morphisms”.

The definition of the `Category` type class (from `Control.Category`; [haddock](https://hackage.haskell.org/package/base/docs/Control-Category.html)) is shown below.  For ease of reading, note that I have used an infix type variable ``arr``, in parallel with the infix function type constructor `(->)`.^[GHC 7.6.1 changed its rules regarding types and type variables.  Now, any operator at the type level is treated as a type *constructor* rather than a type *variable*; prior to GHC 7.6.1 it was possible to use `(~&gt;)` instead of ``arr``.  For more information, see [the discussion on the GHC-users mailing list](http://thread.gmane.org/gmane.comp.lang.haskell.glasgow.user/21350).  For a new approach to nice arrow notation that works with GHC 7.6.1, see [this message](http://article.gmane.org/gmane.comp.lang.haskell.glasgow.user/22615) and also [this message](http://article.gmane.org/gmane.comp.lang.haskell.glasgow.user/22616) from Edward Kmett, though for simplicity I haven't adopted it here.] This syntax is not part of Haskell 2010. The second definition shown is the one used in the standard libraries. For the remainder of this document, I will use the infix type constructor ``arr`` for `Category` as well as `Arrow`.

```haskell
class Category arr where
  id  :: a `arr` a
  (.) :: (b `arr` c) -> (a `arr` b) -> (a `arr` c)

-- The same thing, with a normal (prefix) type constructor
class Category cat where
  id  :: cat a a
  (.) :: cat b c -> cat a b -> cat a c
```

Note that an instance of `Category` should be a type which takes two type arguments, that is, something of kind `* -> * -> *`. It is instructive to imagine the type variable `cat` replaced by the function constructor `(->)`: indeed, in this case we recover precisely the familiar identity function `id` and function composition operator `(.)` defined in the standard `Prelude`.

Of course, the `Category` module provides exactly such an instance of
`Category` for `(->)`.  But it also provides one other instance, shown below, which should be familiar from the previous discussion of the `Monad` laws.  `Kleisli m a b`, as defined in the `Control.Arrow` module, is just a `newtype` wrapper  around `a -> m b`.

```haskell
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id :: Kleisli m a a
  id = Kleisli return

  (.) :: Kleisli m b c -> Kleisli m a b -> Kleisli m a c
  Kleisli g . Kleisli h = Kleisli (h >=> g)
```

The only laws that `Category` instances should satisfy are that `id` should be the identity of `(.)`, and `(.)` should be associative.  This is kind of like being a monoid, except that, unlike with monoids, not any two values can be composed with `(.)`---their types have to match up.

Finally, the `Category` module exports two additional operators:
`(<<<)`, which is just a synonym for `(.)`, and `(>>>)`, which is `(.)` with its arguments reversed.  (In previous versions of the libraries, these operators were defined as part of the `Arrow` class.)

## Further reading

The name `Category` is a bit misleading, since the `Category` class cannot represent arbitrary categories, but only categories whose objects are objects of `Hask`, the category of Haskell types.  For a more general treatment of categories within Haskell, see the [category-extras package](http://hackage.haskell.org/package/category-extras).  For more about category theory in general, see the excellent [Haskell wikibook page](http://en.wikibooks.org/wiki/Haskell/Category_theory),
[Steve Awodey’s new book](http://books.google.com/books/about/Category_theory.html?id=-MCJ6x2lC7oC), Benjamin Pierce’s [Basic category theory for computer scientists](http://books.google.com/books/about/Basic_category_theory_for_computer_scien.html?id=ezdeaHfpYPwC), or [Barr and Wells category theory lecture notes](http://folli.loria.fr/cds/1999/esslli99/courses/barr-wells.html). [Benjamin Russell’s blog post](http://dekudekuplex.wordpress.com/2009/01/19/motivating-learning-category-theory-for-non-mathematicians/)
is another good source of motivation and category theory links.  You certainly don’t need to know any category theory to be a successful and productive Haskell programmer, but it does lend itself to much deeper appreciation of Haskell’s underlying theory.

# Arrow

The `Arrow` class represents another abstraction of computation, in a
similar vein to `Monad` and `Applicative`.  However, unlike `Monad`
and `Applicative`, whose types only reflect their output, the type of
an `Arrow` computation reflects both its input and output.  Arrows
generalize functions: if `arr` is an instance of `Arrow`, a value of
type `b `arr` c` can be thought of as a computation which takes values of
type `b` as input, and produces values of type `c` as output.  In the
`(->)` instance of `Arrow` this is just a pure function; in general, however,
an arrow may represent some sort of “effectful” computation.

## Definition

The definition of the `Arrow` type class, from
`Control.Arrow` ([haddock](https://hackage.haskell.org/package/base/docs/Control-Arrow.html)), is:

```haskell
class Category arr => Arrow arr where
  arr :: (b -> c) -> (b `arr` c)
  first :: (b `arr` c) -> ((b, d) `arr` (c, d))
  second :: (b `arr` c) -> ((d, b) `arr` (d, c))
  (***) :: (b `arr` c) -> (b' `arr` c') -> ((b, b') `arr` (c, c'))
  (&&&) :: (b `arr` c) -> (b `arr` c') -> (b `arr` (c, c'))
```

The first thing to note is the `Category` class constraint, which
means that we get identity arrows and arrow composition for free:
given two arrows ``g :: b `arr` c`` and ``h :: c `arr` d``, we can form their
composition ``g >>> h :: b `arr` d`` ^[In versions of the `base` package prior to version 4, there is no `Category` class, and the `Arrow` class includes the arrow composition operator `(>>>)`. It also includes `pure` as a synonym for `arr`, but this was removed since it conflicts with the `pure` from `Applicative`.].

As should be a familiar pattern by now, the only methods which must be
defined when writing a new instance of `Arrow` are `arr` and `first`;
the other methods have default definitions in terms of these, but are
included in the `Arrow` class so that they can be overridden with more
efficient implementations if desired.

## Intuition

Let’s look at each of the arrow methods in turn.  [Ross Paterson’s web page on arrows](http://www.haskell.org/arrows/) has nice diagrams which can help
build intuition.

* The `arr` function takes any function `b -> c` and turns it into a generalized arrow `b `arr` c`.  The `arr` method justifies the claim that arrows generalize functions, since it says that we can treat any function as an arrow.  It is intended that the arrow `arr g` is “pure” in the sense that it only computes `g` and has no “effects” (whatever that might mean for any particular arrow type).

* The `first` method turns any arrow from `b` to   `c` into an arrow from `(b,d)` to `(c,d)`.  The idea is that `first g` uses `g` to process the first element of a tuple, and lets the second element pass through unchanged.  For the function instance of `Arrow`, of course, `first g (x,y) = (g x, y)`.

* The `second` function is similar to `first`, but with the elements of the tuples swapped.  Indeed, it can be defined in terms of `first` using an auxiliary function `swap`, defined by `swap (x,y) = (y,x)`.

* The `(***)` operator is “parallel composition” of arrows: it takes two arrows and makes them into one arrow on tuples, which has the behavior of the first arrow on the first element of a tuple, and the behavior of the second arrow on the second element.  The mnemonic is that `g *** h` is the *product* (hence `*`) of `g` and `h`. For the function instance of `Arrow`, we define `(g *** h) (x,y) = (g x, h y)`.  The default implementation of `(***)` is in terms of `first`, `second`, and sequential arrow composition `(>>>)`.  The reader may also wish to think about how to implement `first` and `second` in terms of `(***)`.

* The `(&&&)` operator is “fanout composition” of arrows: it takes two arrows `g` and `h` and makes them into a new arrow `g &&& h` which supplies its input as the input to both `g` and `h`, returning their results as a tuple.  The mnemonic is that `g &&& h` performs both `g` *and* `h` (hence `&`) on its input.  For functions, we define `(g &&& h) x = (g x, h x)`.

## Instances

The `Arrow` library itself only provides two `Arrow` instances, both
of which we have already seen: `(->)`, the normal function
constructor, and `Kleisli m`, which makes functions of
type `a -> m b` into `Arrow`s for any `Monad m`. These instances are:

```haskell
instance Arrow (->) where
  arr :: (b -> c) -> (b -> c)
  arr g = g

  first :: (b -> c) -> ((b,d) -> (c,d))
  first g (x,y) = (g x, y)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
  arr :: (b -> c) -> Kleisli m b c
  arr f = Kleisli (return . f)

  first :: Kleisli m b c -> Kleisli m (b,d) (c,d)
  first (Kleisli f) = Kleisli (\ ~(b,d) -> do c <- f b
                                              return (c,d) )
```

## Laws



There are quite a few laws that instances of `Arrow` should
satisfy ^[See [John Hughes: Generalising monads to arrows](http://dx.doi.org/10.1016/S0167-6423(99)00023-4); [Sam Lindley, Philip Wadler, Jeremy Yallop: The arrow calculus](http://homepages.inf.ed.ac.uk/wadler/papers/arrows/arrows.pdf); [Ross Paterson: Programming with Arrows](http://www.soi.city.ac.uk/~ross/papers/fop.html).]:

```haskell
                       arr id  =  id
                  arr (h . g)  =  arr g >>> arr h
                first (arr g)  =  arr (g *** id)
              first (g >>> h)  =  first g >>> first h
   first g >>> arr (id *** h)  =  arr (id *** h) >>> first g
          first g >>> arr fst  =  arr fst >>> g
first (first g) >>> arr assoc  =  arr assoc >>> first g

assoc ((x,y),z) = (x,(y,z))
```

Note that this version of the laws is slightly different than the laws given in the
first two above references, since several of the laws have now been
subsumed by the `Category` laws (in particular, the requirements that
`id` is the identity arrow and that `(>>>)` is associative).  The laws
shown here follow those in Paterson’s Programming with Arrows, which uses the
`Category` class.

The reader is advised not to lose too much sleep over the `Arrow` laws ^[Unless category-theory-induced insomnolence is your cup of tea.], since it is not essential to understand them in order to program with arrows. There are also laws that `ArrowChoice`, `ArrowApply`, and `ArrowLoop` instances should satisfy; the interested reader should consult [Paterson: Programming with Arrows](http://www.soi.city.ac.uk/~ross/papers/fop.html).

## ArrowChoice

Computations built using the `Arrow` class, like those built using
the `Applicative` class, are rather inflexible: the structure of the computation
is fixed at the outset, and there is no ability to choose between
alternate execution paths based on intermediate results.
The `ArrowChoice` class provides exactly such an ability:

```haskell
class Arrow arr => ArrowChoice arr where
  left  :: (b `arr` c) -> (Either b d `arr` Either c d)
  right :: (b `arr` c) -> (Either d b `arr` Either d c)
  (+++) :: (b `arr` c) -> (b' `arr` c') -> (Either b b' `arr` Either c c')
  (|||) :: (b `arr` d) -> (c `arr` d) -> (Either b c `arr` d)
```

A comparison of `ArrowChoice` to `Arrow` will reveal a striking
parallel between `left`, `right`, `(+++)`, `(|||)` and `first`,
`second`, `(***)`, `(&&&)`, respectively.  Indeed, they are dual:
`first`, `second`, `(***)`, and `(&&&)` all operate on product types
(tuples), and `left`, `right`, `(+++)`, and `(|||)` are the
corresponding operations on sum types.  In general, these operations
create arrows whose inputs are tagged with `Left` or `Right`, and can
choose how to act based on these tags.

* If `g` is an arrow from `b` to `c`, then `left g` is an arrow from `Either b d` to `Either c d`.  On inputs tagged with `Left`, the `left g` arrow has the behavior of `g`; on inputs tagged with `Right`, it behaves as the identity.

* The `right` function, of course, is the mirror image of `left`. The arrow  `right g` has the behavior of `g` on inputs tagged with `Right`.

* The `(+++)` operator performs “multiplexing”: `g +++ h` behaves as `g` on inputs tagged with `Left`, and as `h` on inputs tagged with `Right`.  The tags are preserved.  The `(+++)` operator is the *sum* (hence `+`) of two arrows, just as `(***)` is the product.

* The `(|||)` operator is “merge” or “fanin”: the arrow `g ||| h` behaves as `g` on inputs tagged with `Left`, and `h` on inputs tagged with `Right`, but the tags are discarded (hence, `g` and `h` must have the same output type).  The mnemonic is that `g ||| h` performs either `g` *or* `h` on its input.

The `ArrowChoice` class allows computations to choose among a finite number of execution paths, based on intermediate results.  The possible
execution paths must be known in advance, and explicitly assembled with `(+++)` or `(|||)`.  However, sometimes more flexibility is
needed: we would like to be able to *compute* an arrow from intermediate results, and use this computed arrow to continue the computation.  This is the power given to us by `ArrowApply`.

## ArrowApply

The `ArrowApply` type class is:

```haskell
class Arrow arr => ArrowApply arr where
  app :: (b `arr` c, b) `arr` c
```

If we have computed an arrow as the output of some previous
computation, then `app` allows us to apply that arrow to an input,
producing its output as the output of `app`.  As an exercise, the
reader may wish to use `app` to implement an alternative “curried”
version, `app2 :: b `arr` ((b `arr` c) `arr` c)`.

This notion of being able to *compute* a new computation
may sound familiar:
this is exactly what the monadic bind operator `(>>=)` does.  It
should not particularly come as a surprise that `ArrowApply` and
`Monad` are exactly equivalent in expressive power.  In particular,
`Kleisli m` can be made an instance of `ArrowApply`, and any instance
of `ArrowApply` can be made a `Monad` (via the `newtype` wrapper
`ArrowMonad`).  As an exercise, the reader may wish to try
implementing these instances:

```haskell
class Arrow arr => ArrowApply arr where
  app :: (b `arr` c, b) `arr` c

instance Monad m => ArrowApply (Kleisli m) where
  app :: Kleisli m (Kleisli m b c, b) c
  app =    -- exercise

newtype ArrowApply a => ArrowMonad a b = ArrowMonad (a () b)

instance ArrowApply a => Monad (ArrowMonad a) where
  return :: b -> ArrowMonad a b
  return               =    -- exercise

  (>>=) :: ArrowMonad a a -> (a -> ArrowMonad a b) -> ArrowMonad a b
  (ArrowMonad a) >>= k =    -- exercise
```

## ArrowLoop

The `ArrowLoop` type class is:

```haskell
class Arrow a => ArrowLoop a where
  loop :: a (b, d) (c, d) -> a b c

trace :: ((b,d) -> (c,d)) -> b -> c
trace f b = let (c,d) = f (b,d) in c
```

It describes arrows that can use recursion to compute results, and is
used to desugar the `rec` construct in arrow notation (described
below).

Taken by itself, the type of the `loop` method does not seem to tell
us much.  Its intention, however, is a generalization of the `trace`
function which is also shown.  The `d` component of the first arrow’s
output is fed back in as its own input.  In other words, the arrow
`loop g` is obtained by recursively “fixing” the second component of
the input to `g`.

It can be a bit difficult to grok what the `trace` function is doing.
How can `d` appear on the left and right sides of the `let`?  Well,
this is Haskell’s laziness at work.  There is not space here for a
full explanation; the interested reader is encouraged to study the
standard `fix` function, and to read [Paterson’s arrow tutorial](http://www.soi.city.ac.uk/~ross/papers/fop.html).

## Arrow notation

Programming directly with the arrow combinators can be painful,
especially when writing complex computations which need to retain
simultaneous reference to a number of intermediate results. With
nothing but the arrow combinators, such intermediate results must be
kept in nested tuples, and it is up to the programmer to remember
which intermediate results are in which components, and to swap,
reassociate, and generally mangle tuples as necessary.  This problem
is solved by the special arrow notation supported by GHC, similar to
`do` notation for monads, that allows names to be assigned to
intermediate results while building up arrow computations.  An example
arrow implemented using arrow notation, taken from
Paterson, is:

```haskell
class ArrowLoop arr => ArrowCircuit arr where
  delay :: b -> (b `arr` b)

counter :: ArrowCircuit arr => Bool `arr` Int
counter = proc reset -> do
            rec output <- idA     -< if reset then 0 else next
                next   <- delay 0 -< output + 1
            idA -< output
```

This arrow is intended to
represent a recursively defined counter circuit with a reset line.

There is not space here for a full explanation of arrow notation; the
interested reader should consult
[Paterson’s paper introducing the notation](http://www.soi.city.ac.uk/~ross/papers/notation.html), or his later [tutorial which presents a simplified version](http://www.soi.city.ac.uk/~ross/papers/fop.html).

## Further reading

An excellent starting place for the student of arrows is the [arrows web page](http://www.haskell.org/arrows/), which contains an
introduction and many references. Some key papers on arrows include
Hughes’ original paper introducing arrows, [Generalising monads to arrows](http://dx.doi.org/10.1016/S0167-6423(99)00023-4), and [Paterson’s paper on arrow notation](http://www.soi.city.ac.uk/~ross/papers/notation.html).

Both Hughes and Paterson later wrote accessible tutorials intended for a broader
audience: [Paterson: Programming with Arrows](http://www.soi.city.ac.uk/~ross/papers/fop.html) and [Hughes: Programming with Arrows](http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf).

Although Hughes’ goal in defining the `Arrow` class was to
generalize `Monad`s, and it has been said that `Arrow` lies “between
`Applicative` and `Monad`” in power, they are not directly
comparable.  The precise relationship remained in some confusion until
[analyzed by Lindley, Wadler, and Yallop](http://homepages.inf.ed.ac.uk/wadler/papers/arrows-and-idioms/arrows-and-idioms.pdf), who
also invented a new calculus of arrows, based on the lambda calculus,
which considerably simplifies the presentation of the arrow laws
(see [The arrow calculus](http://homepages.inf.ed.ac.uk/wadler/papers/arrows/arrows.pdf)).  There is also a precise technical sense in which [`Arrow` can be seen as the intersection of `Applicative` and `Category`](http://just-bottom.blogspot.de/2010/04/programming-with-effects-story-so-far.html).

Some examples of `Arrow`s include [Yampa](https://wiki.haskell.org/Yampa), the
[Haskell XML Toolkit](http://www.fh-wedel.de/~si/HXmlToolbox/), and the functional GUI library [Grapefruit](http://www.haskell.org/haskellwiki/Grapefruit).

Some extensions to arrows have been explored; for example, the
`BiArrow`s of Alimarine et al. (["There and Back Again: Arrows for Invertible Programming"](http://wiki.clean.cs.ru.nl/download/papers/2005/alia2005-biarrowsHaskellWorkshop.pdf)), for two-way instead of one-way
computation.

The Haskell wiki has [links to many additional research papers relating to `Arrow`s](http://www.haskell.org/haskellwiki/Research papers/Monads and Arrows).

# Comonad

The final type class we will examine is `Comonad`. The `Comonad` class
is the categorical dual of `Monad`; that is, `Comonad` is like `Monad`
but with all the function arrows flipped. It is not actually in the
standard Haskell libraries, but it has seen some interesting uses
recently, so we include it here for completeness.

## Definition

The `Comonad` type class, defined in the `Control.Comonad` module of
the [comonad library](http://hackage.haskell.org/package/comonad), is:

```haskell
class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
```

As you can see, `extract` is the dual of `return`, `duplicate` is the dual of `join`, and `extend` is the dual of `(=<<)`.  The definition of `Comonad` is a bit redundant, giving the programmer the choice on whether extend or duplicate are implemented; the other operation then has a default implementation.

A prototypical example of a `Comonad` instance is:

```haskell
-- Infinite lazy streams
data Stream a = Cons a (Stream a)

-- 'duplicate' is like the list function 'tails'
-- 'extend' computes a new Stream from an old, where the element
--   at position n is computed as a function of everything from
--   position n onwards in the old Stream
instance Comonad Stream where
  extract :: Stream a -> a
  extract (Cons x _) = x

  duplicate :: Stream a -> Stream (Stream a)
  duplicate s@(Cons x xs) = Cons s (duplicate xs)

  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend g s@(Cons x xs)  = Cons (g s) (extend g xs)
                       -- = fmap g (duplicate s)
```

## Further reading

Dan Piponi explains in a blog post what [cellular automata have to do with comonads](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html).  In another blog post, Conal Elliott has examined [a comonadic formulation of functional reactive programming](http://conal.net/blog/posts/functional-interactive-behavior/).  Sterling Clover’s blog post [Comonads in everyday life](http://fmapfixreturn.wordpress.com/2008/07/09/comonads-in-everyday-life/) explains the relationship between comonads and zippers, and how comonads can be used to design a menu system for a web site.

Uustalu and Vene have a number of papers exploring ideas related to comonads and functional programming:
* [Comonadic Notions of Computation](http://dx.doi.org/10.1016/j.entcs.2008.05.029)
* [The dual of substitution is redecoration](http://www.ioc.ee/~tarmo/papers/sfp01-book.pdf) (Also available as [ps.gz](http://www.cs.ut.ee/~varmo/papers/sfp01-book.ps.gz).)
* [Recursive coalgebras from comonads](http://dx.doi.org/10.1016/j.ic.2005.08.005)
* [Recursion schemes from comonads](http://www.fing.edu.uy/~pardo/papers/njc01.ps.gz)
* [The Essence of Dataflow Programming](http://cs.ioc.ee/~tarmo/papers/essence.pdf).

Gabriel Gonzalez's [Comonads are objects](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html) points out similarities between comonads and object-oriented programming.

The [comonad-transformers](http://hackage.haskell.org/package/comonad-transformers) package contains comonad transformers.

# Acknowledgements

A special thanks to all of those who taught me about standard Haskell
type classes and helped me develop good intuition for them,
particularly Jules Bean (quicksilver), Derek Elkins (ddarius), Conal
Elliott (conal), Cale Gibbard (Cale), David House, Dan Piponi
(sigfpe), and Kevin Reid (kpreid).

I also thank the many people who provided a mountain of helpful
feedback and suggestions on a first draft of the Typeclassopedia: David Amos,
Kevin Ballard, Reid Barton, Doug Beardsley, Joachim Breitner, Andrew
Cave, David Christiansen, Gregory Collins, Mark Jason Dominus, Conal
Elliott, Yitz Gale, George Giorgidze, Steven Grady, Travis Hartwell,
Steve Hicks, Philip Hölzenspies, Edward Kmett, Eric Kow, Serge Le
Huitouze, Felipe Lessa, Stefan Ljungstrand, Eric Macaulay, Rob MacAulay, Simon Meier,
Eric Mertens, Tim Newsham, Russell O’Connor, Conrad Parker, Walt
Rorie-Baety, Colin Ross, Tom Schrijvers, Aditya Siram, C. Smith,
Martijn van Steenbergen, Joe Thornber, Jared Updike, Rob Vollmert,
Andrew Wagner, Louis Wasserman, and Ashley Yakeley, as well as a few
only known to me by their IRC nicks: b_jonas, maltem, tehgeekmeister,
and ziman.  I have undoubtedly omitted a few inadvertently, which in
no way diminishes my gratitude.

Finally, I would like to thank Wouter Swierstra for his fantastic work
editing the Monad.Reader, and my wife Joyia for her patience during
the process of writing the Typeclassopedia.

# About the author

Brent Yorgey ([blog](http://byorgey.wordpress.com/)) is (as of August 2017) an assistant professor in the department of mathematics and computer science at Hendrix College in Conway, Arkansas, USA.  He enjoys teaching, creating EDSLs, playing Bach fugues, musing upon category theory, and cooking tasty lambda-treats for the denizens of #haskell.

# Colophon

The Typeclassopedia was written by Brent Yorgey and initially published in March 2009. It was converted to wiki syntax by [Geheimdienst](http://www.haskell.org/haskellwiki/User:Geheimdienst) in November 2011, after asking Brent’s permission. The Markdown conversion was done by Erlend Hamberg and the EPUB file is generated by the excellent [pandoc](http://johnmacfarlane.net/pandoc/) tool written by John MacFarlane.
