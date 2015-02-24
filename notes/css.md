CSS
===

Some general notes regarding the structure of the CSS language.

## Grammar

A CSS rule group will be referred to as an _"expression"_. That is, an
expression is a complete set of CSS rules in the form of

```css
/*selectors*/ {
  /*body*/
}
```

where `body` contains a list of _key / value_ rules, in the form of

```css
/*selectors*/ {
  /*key*/ : /*value*/;
  /*key*/ : /*value*/;
  /*key*/ : /*value*/;
}
```

Also, an _atomic_ selector actually form a semigroup, where `<>` is
_"arbitrary nesting"_ for the rightward selector:

```haskell
`.foo` <> `#bar` = `.foo #bar`
`baz.quxx` <> `blah` = `baz.quxx blah`
```

so `<>` is just a space in CSS-speak. The selector grammar could look like this:

```haskell
classes = class : []
        | class : classes

selector = elem (Maybe id) (Maybe classes)
         | id (Maybe classes)
         | classes
```

Where `classes` is a non-empty list of class names, and our selector must
either include one of the three selector styles for a valid selector.

The semigroup forms around _nested_ selectors, which represent arbitrary
children in DOM-semantics.

```haskell
nested =     [] : selector
       | nested : selector
```

`nested` is the final form for the series of selectors, which is a snoc-list
with at least one element (and over 9000).

Now, we can formulate a definition for a CSS expression:

```haskell
expression = nested * [(key,value)]
```

> (sorry for the OCaml / Haskell notation mix)

So a single CSS expression is a fully-qualified selector `nested` applied with
it's "rule body" - the (possibly empty) list of key/value pairs.

## Summary

```haskell
import Data.List.NonEmpty


type Classes = NonEmpty String
type Id = String
type Elem = String

data Selector = SElem Elem (Maybe Id) (Maybe Classes)
              | SId Id (Maybe Classes)
              | SClasses Classes

type Nested = NonEmpty Selector

type Rule = String
type Value = String

data Expr = Expr Nested [(Rule, Value)]
```
