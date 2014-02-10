---
layout: default
---

# Quasi-quotes guide

## Prerequisties

All examples in this guide are run in the repl with one extra import:

    import scala.reflect.runtime.universe._

## Intro
## Cardinality
## Interpolators: q, tq, cq, pq, fq
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

### Expressions

                                        | Quasi-quote                                                 | Type
----------------------------------------|-------------------------------------------------------------|-------------------------
 [Empty](#empty-expr)                   | `q""`                                                       | EmptyTree
 [Identifier](#ident)                   | `q"$tname"` or `q"Name"`                                    | Ident
 [Constant](#constant)                  | `q"$value"`                                                 | Literal
 [This](#this-super)                    | `q"$tpname.this"`                                           | This
 [Super](#this-super)                   | `q"$tpname.super[$tpname].$tname"`                          | Tree
 [Application](#application)            | `q"$expr(...$argss)"`                                       | Apply
 [Type Application] (#type-application) | `q"$expr[..$targs]"`                                        | TypeApply
 [Selection](#selection)                | `q"$expr.$tname"`                                           | Select
 [Assign](#assign-update)               | `q"$expr = $expr"`                                          | Assign, AssignOrNamedArg
 [Update](#assign-update)               | `q"$expr(..$exprs) = $expr"`                                | Tree
 [Return](#return)                      | `q"return $expr"`                                           | Return
 [Throw](#throw)                        | `q"throw $expr"`                                            | Throw
 [Ascription](#ascription)              | `q"$expr: $tpt"`                                            | Typed 
 [Tuple](#tuple-expr)                   | `q"(..$exprs)"`                                             | Tree
 [Function](#function-expr)             | `q"(..$args) => $expr"`                                     | Function
 [Block](#block)                        | `q"{ ..$stats }"`                                           | Block
 [If](#if)                              | `q"if ($expr) $expr else $expr"`                            | If
 [Pattern Match](#match)                | `q"$expr match { case ..$cases }"`                          | Match
 [Try](#try)                            | `q"try $expr catch { case ..$cases } finally $expr"`        | Try
 [While Loop](#while)                   | `q"while ($expr) $expr"`                                    | LabelDef 
 [Do-While Loop](#do-while)             | `q"do $expr while ($expr)"`                                 | LabelDef
 [For Loop](#for)                       | `q"for (..$enums) $expr"`                                   | Tree
 [For-Yield Loop](#for-yield)           | `q"for (..$enums) yield $expr"`                             | Tree
 [New](#new)                            | `q"new { ..$early } with ..$parents { $self => ..$stats }"` | Tree

### Types

                  | Quasi-quote                           | Type
------------------|---------------------------------------|---------------------
 Empty Type       | `tq""`                                | TypeTree
 Identifier       | `tq"$tpname"` or `tq"Name"`           | Ident
 Applied Type     | `tq"$tpt[..$tpts]"`                   | AppliedTypeTree
 Tuple Type       | `tq"(..$tpts)"`                       | Tree
 Function Type    | `tq"(..$tpts) => $tpt"`               | Tree
 Existential Type | `tq"$tpt forSome { ..$defns }"`       | ExistentialTypeTree
 Type Selection   | `tq"$tpt#$tpname"`                    | SelectFromTypeTree
 Dependent Type   | `tq"$ref.$tpname"`                    | Select
 Refined Type     | `tq"..$parents { ..$defns }"`         | CompoundTypeTree
 Singleton Type   | `tq"$ref.type"`                       | SingletonType
 Super Type       | `tq"$tpname.super[$tpname].$tpname"`  | Tree

### Patterns
 
                  | Quasi-quote           | Type                    
------------------|-----------------------|-------------------
 Wildcard Pattern | `pq"_"`               | Ident
 Binding          | `pq"$tname @ $pat"`   | Bind
 Extractor Call   | `pq"$ref(..$pats)"`   | Apply, UnApply   
 Tuple Pattern    | `pq"(..$pats)"`       | Apply, UnApply
 Type Pattern     | `pq"$tname: $tpt"`    | Typed             
 
### Definitions

                | Quasi-quote                                                                                                        | Type 
----------------|--------------------------------------------------------------------------------------------------------------------|-----------
 Def            | `q"$mods def $tname[..$targs](...$argss): $tpt = $expr"`                                                           | DefDef
 Val            | `q"$mods val $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                 | ValDef
 Var            | `q"$mods var $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                 | ValDef
 Val Pattern    | `q"$mods val $pat: $tpt = $expr"`                                                                                  | Tree
 Var Pattern    | `q"$mods var $pat: $tpt = $expr"`                                                                                  | Tree
 Type           | `q"$mods type $tpname[..$targs] = $tpt"`                                                                           | TypeDef
 Class          | `q"$mods class $tpname[..$targs] $ctorMods(...$argss) extends { ..$early } with ..$parents { $self => ..$stats }"` | ClassDef
 Trait          | `q"$mods trait $tpname[..$targs] extends { ..$early } with ..$parents { $self => ..$stats }"`                      | TraitDef
 Object         | `q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }"`                                 | ModuleDef
 Package        | `q"package $ref { ..$topstats }"`                                                                                  | PackageDef
 Package Object | `q"package object $tname extends { ..$early } with ..$parents { $self => ..$stats }"`                              | PackageDef
 Import         | `q"import $ref.{..$sels}"`                                                                                         | Import

## Syntax Details 

### Expressions

#### Empty <a name="empty-expr"> </a>

`q""` is used to indicate that some part of the syntactic expressions is not provided by the user:

1. Vals, Vars and Defs without right-hand side have it set to `q""`.
2. Type definitions without bounds have them set to `q""`.
3. Try expressions without finally clause have it set to `q""`.
4. Case clauses without guards have them set to `q""`.

#### Identifier <a name="ident"> </a>

#### Constant <a name="constant"> </a>

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

#### Application <a name="application"> </a>

#### Type Application <a name="type-application"> </a>

#### Selection <a name="selection"> </a>

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

#### Function <a name="function-expr"> </a>

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

#### Try <a name="try"> </a>

#### While Loop <a name="while"> </a>

#### Do-While Loop <a name="do-while"> </a>

#### For Loop <a name="for"> </a>

#### For-Yield Loop <a name="for-yield"> </a>

#### New <a name="new"> </a>

### Types

### Patterns

### Definitions
