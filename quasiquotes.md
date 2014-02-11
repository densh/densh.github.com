---
layout: default
---

# Quasi-quote guide (WIP)

## Prerequisties

All examples in this guide are run in the repl with one extra import:

    import scala.reflect.runtime.universe._

## Intro

## Cardinality

## Interpolators

    | Used for 
----|----------------------------------------------------------------
 q  | [expressions](#exprs-summary) and [definitions](#defns-summary)
 tq | [types](#types-summary)
 pq | [patterns](#pats-summary)
 cq | [case clause](#aux-summary)
 fq | [for loop enumerator](#aux-summary)

## Lifting and Unlifting

## Referential transparency

## Syntax summary

### Abbreviation

* `tname: TermName`
* `tpname: TypeName`
* `value: Byte|Short|Int|Long|Float|Double|Boolean|String|Unit`
* `expr: Tree` that contains an expression
* `tpt: Tree` that contains a type
* `pat: Tree` that contains a pattern
* `args: List[Tree]` where each element is a parameter
* `argss: List[List[Tree]]` where each element is a parameter
* `targs: List[Tree]` where each element is a type argument
* `enums: List[Tree]` where each element is a for loop enumerator
* `early: List[Tree]` where each element is early definition
* `parents: List[Tree]` where each element is a parent
* `self: Tree` that corresponds to self type definition
* `stats: List[Tree]` where each element is either an expression or definition
* `topstats: List[Tree]` where each element is Class, Trait, Object or Package definition
* `defns: List[Tree]` where each element is Val, Var, Def or Type definition 
* `sels: List[Tree]` where each element is an import selector

### Expressions <a name="exprs-summary"> </a>

                                        | Quasi-quote                                                 | Type
----------------------------------------|-------------------------------------------------------------|-------------------------
 [Empty](#empty-expr)                   | `q""`                                                       | EmptyTree
 [Identifier](#term-ident)              | `q"$tname"` or `q"name"`                                    | Ident
 [Constant](#constant)                  | `q"$value"`                                                 | Literal
 [Application](#application)            | `q"$expr(...$argss)"`                                       | Apply
 [Type Application] (#type-application) | `q"$expr[..$targs]"`                                        | TypeApply
 [Selection](#selection)                | `q"$expr.$tname"`                                           | Select
 [This](#this-super)                    | `q"$tpname.this"`                                           | This
 [Super](#this-super)                   | `q"$tpname.super[$tpname].$tname"`                          | Tree
 [Assign](#assign-update)               | `q"$expr = $expr"`                                          | Assign, AssignOrNamedArg
 [Update](#assign-update)               | `q"$expr(..$exprs) = $expr"`                                | Tree
 [Return](#return)                      | `q"return $expr"`                                           | Return
 [Throw](#throw)                        | `q"throw $expr"`                                            | Throw
 [Ascription](#ascription)              | `q"$expr: $tpt"`                                            | Typed 
 [Tuple](#tuple-expr)                   | `q"(..$exprs)"`                                             | Tree
 [Block](#block)                        | `q"{ ..$stats }"`                                           | Block
 [If](#if)                              | `q"if ($expr) $expr else $expr"`                            | If
 [Pattern Match](#match)                | `q"$expr match { case ..$cases }"`                          | Match
 [Try](#try)                            | `q"try $expr catch { case ..$cases } finally $expr"`        | Try
 [Function](#function-expr)             | `q"(..$args) => $expr"`                                     | Function
 [Partial Function](#partial-function)  | `q"{ case ..$cases }"`                                      | Match
 [While Loop](#while)                   | `q"while ($expr) $expr"`                                    | LabelDef 
 [Do-While Loop](#while)                | `q"do $expr while ($expr)"`                                 | LabelDef
 [For Loop](#for)                       | `q"for (..$enums) $expr"`                                   | Tree
 [For-Yield Loop](#for)                 | `q"for (..$enums) yield $expr"`                             | Tree
 [New](#new)                            | `q"new { ..$early } with ..$parents { $self => ..$stats }"` | Tree

### Types <a name="types-summary"> </a>

                                       | Quasi-quote                           | Type
---------------------------------------|---------------------------------------|---------------------
 [Empty Type](#empty-type)             | `tq""`                                | TypeTree
 [Type Identifier](#type-ident)        | `tq"$tpname"` or `tq"Name"`           | Ident
 [Applied Type](#applied-type)         | `tq"$tpt[..$tpts]"`                   | AppliedTypeTree
 [Tuple Type](#tuple-type)             | `tq"(..$tpts)"`                       | Tree
 [Function Type](#function-type)       | `tq"(..$tpts) => $tpt"`               | Tree
 [Existential Type](#existential-type) | `tq"$tpt forSome { ..$defns }"`       | ExistentialTypeTree
 [Type Selection](#type-selection)     | `tq"$tpt#$tpname"`                    | SelectFromTypeTree
 [Dependent Type](#dependent-type)     | `tq"$ref.$tpname"`                    | Select
 [Refined Type](#refined-type)         | `tq"..$parents { ..$defns }"`         | CompoundTypeTree
 [Singleton Type](#singleton-type)     | `tq"$ref.type"`                       | SingletonType
 [Super Type](#super-type)             | `tq"$tpname.super[$tpname].$tpname"`  | Tree

### Patterns <a name="pats-summary"> </a>
 
                                             | Quasi-quote            | Type                    
---------------------------------------------|------------------------|-------------------
 [Wildcard Pattern](#wilcard-pattern)        | `pq"_"`                | Ident
 [Binding Pattern](#binding-pattern)         | `pq"$tname @ $pat"`    | Bind
 [Extractor Pattern](#extractor-pattern)     | `pq"$ref(..$pats)"`    | Apply, UnApply   
 [Tuple Pattern](#tuple-pattern)             | `pq"(..$pats)"`        | Apply, UnApply
 [Type Pattern](#type-pattern)               | `pq"$tname: $tpt"`     | Typed  
 [Alternative Pattern](#alternative-pattern) | `pq"$first │ ..$rest"` | Alternative       
 
### Definitions <a name="defns-summary"> </a>

                                       | Quasi-quote                                                                                                        | Type 
---------------------------------------|--------------------------------------------------------------------------------------------------------------------|-----------
 [Def](#def-definition)                | `q"$mods def $tname[..$targs](...$argss): $tpt = $expr"`                                                           | DefDef
 [Val](#val-var-definition)            | `q"$mods val $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                 | ValDef
 [Var](#val-var-definition)            | `q"$mods var $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                 | ValDef
 [Val Pattern](#val-var-definition)    | `q"$mods val $pat: $tpt = $expr"`                                                                                  | Tree
 [Var Pattern](#val-var-definition)    | `q"$mods var $pat: $tpt = $expr"`                                                                                  | Tree
 [Type](#type-definition)              | `q"$mods type $tpname[..$targs] = $tpt"`                                                                           | TypeDef
 [Class](#class-definition)            | `q"$mods class $tpname[..$targs] $ctorMods(...$argss) extends { ..$early } with ..$parents { $self => ..$stats }"` | ClassDef
 [Trait](#trait-definition)            | `q"$mods trait $tpname[..$targs] extends { ..$early } with ..$parents { $self => ..$stats }"`                      | TraitDef
 [Object](#object-definition)          | `q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }"`                                 | ModuleDef
 [Package](#package-definition)        | `q"package $ref { ..$topstats }"`                                                                                  | PackageDef
 [Package Object](#package-definition) | `q"package object $tname extends { ..$early } with ..$parents { $self => ..$stats }"`                              | PackageDef
 [Import](#import-definition)          | `q"import $ref.{..$sels}"`                                                                                         | Import

### Auxiliary <a name="aux-summary"> </a>

                                     | Quasi-quote                 | Type
-------------------------------------|-----------------------------|--------
 [Case Clause](#match)               | `cq"$pat if $expr => expr"` | CaseDef
 [Generator Enumerator](#for)        | `fq"$pat <- $expr"`         | Tree
 [Value Definition Enumerator](#for) | `fq"$pat = $expr"`          | Tree
 [Guard Enumerator](#for)            | `fq"if $expr"`              | Tree

## Syntax Details 

### Expressions

#### Empty <a name="empty-expr"> </a>

`q""` is used to indicate that some part of the tree is not provided by the user:

1. Vals, Vars and Defs without right-hand side have it set to `q""`.
2. Type definitions without bounds have them set to `q""`.
3. Try expressions without finally clause have it set to `q""`.
4. Case clauses without guards have them set to `q""`.

Default toString formats `q""` as `<empty>`.

#### Identifier <a name="term-ident"> </a>

You can also esily create a fresh identifiers with the help of `q"_"`:

    scala> val freshId = q"_"
    freshId: reflect.runtime.universe.Ident = x$1

    scala> val freshId = q"_"
    freshId: reflect.runtime.universe.Ident = x$2

#### Constant <a name="constant"> </a>

#### Application <a name="application"> </a>

#### Type Application <a name="type-application"> </a>

#### Selection <a name="selection"> </a>

#### This and Super <a name="this-super"> </a>

This and super expressions allow to select precise members within inheritance chain.

This tree supports following variations:

    scala> val q"$name.this" = q"this"
    name: reflect.runtime.universe.TypeName =

    scala> val q"$name.this" = q"foo.this"
    name: reflect.runtime.universe.TypeName = foov

So plain `q"this"` is equivalent to `q"${tpnme.EMPTY}.this"`. 

Similarly for super we have:

    scala> val q"$name.super[$qual].$field" = q"super.foo"
    name: reflect.runtime.universe.TypeName =
    qual: reflect.runtime.universe.TypeName =
    field: reflect.runtime.universe.Name = foo

    scala> val q"$name.super[$qual].$field" = q"super[T].foo"
    name: reflect.runtime.universe.TypeName =
    qual: reflect.runtime.universe.TypeName = T
    field: reflect.runtime.universe.Name = foo

    scala> val q"$name.super[$qual].$field" = q"other.super[T].foo"
    name: reflect.runtime.universe.TypeName = other
    qual: reflect.runtime.universe.TypeName = T
    field: reflect.runtime.universe.Name = foo

#### Assign and Update <a name="assign-update"> </a>

#### Return <a name="return"> </a>

Return expressions is used to perform early return from the function. 

    scala> val ret = q"return 2 + 2"
    ret: reflect.runtime.universe.Return = return 2.$plus(2)

    scala> val q"return $expr" = ret 
    expr: reflect.runtime.universe.Tree = 2.$plus(2)

#### Throw <a name="throw"> </a>

Throw expression is used to throw a throwable:

    scala> val thr = q"throw new Exception"
    thr: reflect.runtime.universe.Throw = throw new Exception()

    scala> val q"throw $expr" = thr 
    expr: reflect.runtime.universe.Tree = new Exception()

#### Ascription <a name="ascription"> </a>

#### Tuple <a name="tuple-expr"> </a>

Tuples are heteregeneous data structures with built-in user-friendly syntax. The syntax itself is just a sugar that maps onto `scala.TupleN` calls:

    scala> val tup = q"(a, b)"
    tup: reflect.runtime.universe.Tree = scala.Tuple2(a, b)

At the moment tuples are only supported up to 22 arity but this is just an implementation restriction that might be lifted in the future. To find out if given arity is supported use:

    scala> val `tuple 10 supported?` = definitions.TupleClass(10) != NoSymbol
    tuple 10 supported?: Boolean = true

    scala> val `tuple 23 supported?` = definitions.TupleClass(23) != NoSymbol
    tuple 23 supported?: Boolean = false

Despited the fact that `Tuple1` class exists there is no built-in syntax for it. Single parens around expression do not change its meaning:
 
    scala> val inparens = q"(a)"
    inparens: reflect.runtime.universe.Ident = a

It is also common to treat `Unit` as nullary tuple:
   
    scala> val elems = List.empty[Tree]
    scala> val nullary = q"(..$elems)"
    nullary: reflect.runtime.universe.Tree = ()

Quasi-quotes also support deconstruction of tuples of arbitrary arity:

    scala> val q"(..$elems)" = q"(a, b)"
    elems: List[reflect.runtime.universe.Tree] = List(a, b)   

This pattern also matches expressions as single-element tuples:

    scala> val q"(..$elems)" = q"(a)"
    elems: List[reflect.runtime.universe.Tree] = List(a)

And unit as nullary tuple:

    scala> val q"(..$elems)" = q"()"
    elems: List[reflect.runtime.universe.Tree] = List()

#### Block <a name="block"> </a>

Blocks are a fundamental primitive to express sequence of actions or bindings. `q"..."` interpolator is an equivalent of a block. It allows to express more than one expression seperated by semicolon or a newline:

    scala> val t = q"a; b; c" 
    t: reflect.runtime.universe.Tree =
    {
      a;
      b;
      c
    } 

The only difference between `q"{...}"` and `q"..."` is handling of case when just a single element is present. `q"..."` always returns an element itself while a block still remains a block if a single element is not expression:

    scala> val t = q"val x = 2"
    t: reflect.runtime.universe.ValDef = val x = 2

    scala> val t = q"{ val x = 2 }"
    t: reflect.runtime.universe.Tree =
    {
      val x = 2;
      ()
    }

Blocks can also be flattened into another blocks with `..$`:

    scala> val ab = q"a; b"
    ab: reflect.runtime.universe.Tree =
    {
      a;
      b
    }

    scala> q"..$ab; c"
    res0: reflect.runtime.universe.Tree =
    {
      a;
      b;
      c
    }

The same syntax can be used to deconstruct blocks:

    scala> val q"..$stats" = q"a; b; c"
    stats: List[reflect.runtime.universe.Tree] = List(a, b, c)

Deconstruction always returns just user-defined contents of a block:

    scala> val q"..$stats" = q"{ val x = 2 }"
    stats: List[reflect.runtime.universe.Tree] = List(val x = 2)

Due to automatic flattening of single-element blocks with expressions, expressions themselves are considered to be single-element blocks:

    scala> val q"..$stats" = q"foo"
    stats: List[reflect.runtime.universe.Tree] = List(foo)

Empty tree is considered to be a zero-element block:

    scala> val q"..$stats" = q""
    stats: List[reflect.runtime.universe.Tree] = List()

#### If <a name="if"> </a>

There are two variaties of if expressions: those with else clause and without it:

    scala> val q"if ($cond) $thenp else $elsep" = q"if (true) a else b"
    cond: reflect.runtime.universe.Tree = true
    thenp: reflect.runtime.universe.Tree = a
    elsep: reflect.runtime.universe.Tree = b

    scala> val q"if ($cond) $thenp else $elsep" = q"if (true) a"
    cond: reflect.runtime.universe.Tree = true
    thenp: reflect.runtime.universe.Tree = a
    elsep: reflect.runtime.universe.Tree = ()

No-else clause is equivalent to else clause that contains a unit literal. 

#### Pattern Match <a name="match"> </a>

Pattern matching is cornerstone feature of Scala that lets you deconstruct values into their components:
    
    q"$expr match { case ..$cases } "

Where each case is represented with a `cq"..."` quote:

    cq"$pat if $expr => $expr" 

Combination of the two forms allows to construct and deconstruct arbitrary pattern matches:

    scala> val q"$expr match { case ..$cases }" = 
               q"foo match { case _: Foo => 'foo case _ => 'notfoo }"
    expr: reflect.runtime.universe.Tree = foo
    cases: List[reflect.runtime.universe.CaseDef] = List(case (_: Foo) => scala.Symbol("foo"), case _ => scala.Symbol("notfoo"))

    scala> val cq"$pat1 => $body1" :: cq"$pat2 => $body2" :: Nil = cases
    pat1: reflect.runtime.universe.Tree = (_: Foo)
    body1: reflect.runtime.universe.Tree = scala.Symbol("foo")
    pat2: reflect.runtime.universe.Tree = _
    body2: reflect.runtime.universe.Tree = scala.Symbol("notfoo")

#### Try <a name="try"> </a>

Try expression is used to handle possible error conditions and ensure consistent state via finally. Both error handling cases and finally clause are optional.

    scala> val q"try $a catch { case ..$b } finally $c" = q"try t"
    a: reflect.runtime.universe.Tree = t
    b: List[reflect.runtime.universe.CaseDef] = List()
    c: reflect.runtime.universe.Tree = <empty>

    scala> val q"try $a catch { case ..$b } finally $c" = q"try t catch { case _: C => }"
    a: reflect.runtime.universe.Tree = t
    b: List[reflect.runtime.universe.CaseDef] = List(case (_: C) => ())
    c: reflect.runtime.universe.Tree = <empty>

    scala> val q"try $a catch { case ..$b } finally $c" = q"try t finally f"
    a: reflect.runtime.universe.Tree = t
    b: List[reflect.runtime.universe.CaseDef] = List()
    c: reflect.runtime.universe.Tree = f

Similarly to [pattern matching](#match) cases can be further deconstructed with `cq"..."`. 

#### Function <a name="function-expr"> </a>

Anonymous functions 

#### Partial Function <a name="partial-function"> </a>

#### While and Do-While Loops <a name="while"> </a>

#### For and For-Yield Loops <a name="for"> </a>

#### New <a name="new"> </a>

### Types

#### Empty Type <a name="empty-type"> </a>

#### Type Identifier <a name="type-ident"> </a>

#### Applied Type <a name="applied-type"> </a>

#### Tuple Type <a name="tuple-type"> </a>

#### Function Type <a name="function-type"> </a>

#### Existential Type <a name="existential-type"> </a>

#### Type Selection <a name="type-selection"> </a>

#### Dependent Type <a name="dependent-type"> </a>

#### Refined Type <a name="refined-type"> </a>

#### Singleton Type <a name="singleton-type"> </a>

#### Super Type <a name="super-type"> </a>

### Patterns

#### Wildcard Pattern <a name="wildcard-pattern"> </a>

#### Binding Pattern <a name="binding-pattern"> </a>

#### Extractor Pattern <a name="extractor-pattern"> </a>

#### Tuple Pattern <a name="tuple-pattern"> </a>

#### Type Pattern <a name="type-pattern"> </a>

#### Alternative Pattern <a name="alternative-pattern"> </a>

### Definitions

#### Def Definition <a name="def-definition"> </a>

#### Val and Var Definitions <a name="val-var-definition"> </a>

#### Type Definition <a name="type-definition"> </a>

#### Class Definition <a name="class-definition"> </a>

#### Trait Definition <a name="trait-definition"> </a>

#### Object Definition <a name="object-definition"> </a>

#### Package and Package Object Definitions<a name="package-definition"> </a>

#### Import Definition <a name="import-definition"> </a>
