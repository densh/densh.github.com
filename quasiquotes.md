---
layout: page 
---

# Quasiquote guide (WIP)

## Prerequisties <a name="prereq"> </a>

All examples in this guide are run in the repl with one extra import:

    import scala.reflect.runtime.universe._

## Terminology <a name="terminology"> </a>

* **Quasiquote** (not quasi-quote) can refer to either quasiquote library or any usage of one it's [interpolators](#interpolators). The name is not hyphenated for sake of consistency with implementations of the same concept in other languages (e.g. [Scheme and Racket](http://docs.racket-lang.org/reference/quasiquote.html), [Haskell](http://www.haskell.org/haskellwiki/Quasiquotation))
* **Tree** or **AST** (Abstract Syntax Tree) is representation of Scala program or a part of it through means of Scala reflection API's Tree type.
* **Tree construction** refers to usages of quasiquotes as expressions to represent creation of new tree values.
* **Tree deconstruction** refers to usages of quasiquotes as patterns to structurally tear trees apart.
* **Unquoting** is a way of either putting thing in or extracting things out of quasiquote. Can be performed with `$` syntax within a quasiquote.
* **Unquote splicing** (or just splicing) is another form of unquoting that flattens contents of the splicee into a tree. Can be performed with either `..$` or `...$` syntax.
* **Cardinality** is a degree of flattenning of unquotee: `cardinality($) == 0`, `cardinality(..$) == 1`, `cardinality(...$) == 2`. 
* [**Lifting**](#lifting) is a way to unquote non-tree values and transform them into trees with the help of Liftable typeclass.
* [**Unlifting**](#unlifting) is a way to unquote non-tree values out of quasiquote patterns with the help of Unliftable typeclass. 

## Intro <a name="intro"> </a> 

## Splicing and cardinality <a name="splicing"> </a>

## Interpolators <a name="interpolators"> </a>

    | Used for 
----|----------------------------------------------------------------
 q  | [expressions](#exprs-summary) and [definitions](#defns-summary)
 tq | [types](#types-summary)
 pq | [patterns](#pats-summary)
 cq | [case clause](#aux-summary)
 fq | [for loop enumerator](#aux-summary)

## Referential transparency <a name="referential-transperancy"> </a>

## Lifting <a name="lifting"> </a>

Lifting is typeclass-based approach to define representation of custom data types as Trees. Its primary use-case is support unquoting of [literal](#literal) values and a number of reflection primitives as trees:

    scala> val two = 1 + 1
    two: Int = 2

    scala> q"$two + $two"
    res10: reflect.runtime.universe.Tree = 2.$plus(2)   

This code runs successfully because quasiquote implementation provides default instance of `Liftable[Int]` that transforms integers into trees that represent literals.

To define your representation of your own data type just provide an implicit instance of `Liftable` for it:

    package points

    import scala.reflect.runtime.universe._ 

    case class Point(x: Int, y: Int)
    object Point {
      implicit val lift = Liftable[Point] { p => 
        q"_root_.points.Point(${p.x}, ${p.y})" 
      }
    }

This way whenever a value of Point type is unquoted in runtime quasiquote it will be automatically transformed
into a case class constructor call. In this example there two important points to take into account:

1. Here we only defined `Liftable` for runtime reflection. It won't be found if you try to
   use it from a macro due to the fact that each universe contains its own `Liftable` which is not
   compatible with the others. This problem is caused by path-dependant nature of current reflection
   api. (see [sharing liftable implementation between universes](#reusing-liftable-impl))

2. Due to lack of [referential transparency](#referential-transperancy), reference to point companion
   has to be fully qualified to ensure correctness in of this tree in every possible context. Another
   way to workaround reference issue is to use symbols to refer to things:

       val PointSym = symbolOf[Point].companionModule
       implicit val lift = Liftable[Point] { p =>
         q"$PointSym(${p.x}, ${p.y})"
       }

### Standard Liftables <a name="standard-liftables"> </a>

 Type                           | Value                 | Representation
--------------------------------|-----------------------|---------------
 `Byte`, `Short`, `Int`, `Long` | `0`                   | `q"0"`
 `Float`                        | `0.0`                 | `q"0.0"`
 `Double`                       | `0.0D`                | `q"0.0D"`
 `Boolean`                      | `true`, `false`       | `q"true"`, `q"false"`
 `Char`                         | `'c'`                 | `q"'c'"`
 `Unit`                         | `()`                  | `q"()"`
 `String`                       | `"string"`            | `q""" "string" """`
 `Symbol`                       | `'symbol`             | `q"'symbol"`
 `Array[T]` †                   | `Array(1, 2)`         | `q"s.Array(1, 2)"` ‡
 `Option[T]` †                  | `Some(1)`             | `q"s.Some(1)"` ‡
 `Vector[T]` †                  | `Vector(1, 2)`        | `q"s.c.i.Vector(1, 2)"` ‡
 `List[T]` †                    | `List(1, 2)`          | `q"s.c.i.List(1, 2)"` ‡
 `Map[K, V]` †                  | `Map(1 -> 2)`         | `q"s.c.i.Map((1, 2))"` ‡
 `Set[T]` †                     | `Set(1, 2)`           | `q"s.c.i.Set(1, 2)"` ‡
 `Either[L, R]` †               | `Left(1)`             | `q"s.u.Left(1)"` ‡
 `TupleN[...]` \* †             | `(1, 2)`              | `q"(1, 2)"`
 `TermName`                     | `TermName("foo")`     | `q"foo"`
 `TypeName`                     | `TypeName("foo")`     | `tq"foo"`
 `Expr`                         | `expr`                | `expr.tree`
 `Type`                         | `typeOf[Int]`         | `TypeTree(typeof[Int])`
 `TypeTag`                      | `ttag`                | `TypeTree(ttag.tpe)`
 `Constant`                     | `const`               | `Literal(const)`
 
 (\*) Liftable for tuples is defined for all `N` in `[2, 22]` range.

 (†) All type parameters have to be Liftable themselves.

 (‡) `s.` is shorthand for scala, `s.c.i.` for `scala.collection.immutable`, `s.u.` for `scala.util.`

### Reusing Liftable implementation between universes <a name="reusing-liftable-impl"> </a>

Due to path dependent nature of current reflection API it isn't trivial to share the same Liftable definition between both macro and runtime universes. A possible way to do this is to define Liftable implementations in a trait and instantiate it for each universe separately:

    import reflect.api.Universe
    import reflect.macros.blackbox.Context

    trait LiftableImpls {
      val univese: Universe
      import universe._
      
      implicit val liftPoint = Liftable[points.Point] { p =>
        q"_root_.points.Point(${p.x}, ${p.y})"
      } 
    }

    object RuntimeLiftableImpls extends { 
      val universe: reflect.runtime.universe = reflect.runtime.univese
    } with LiftableImpls 

    trait MacroLiftableImpls extends {
      val c: Context 
      val universe: c.universe = c.universe
    } with LiftableImpls
    
    // macro impls defined as a macro bundle
    class MyMacros(c: Context) with MacroLiftableImpls {
      // ... 
    }

So in practice it's much easier to just define a liftable for given universe at hand:
    
    import reflect.macros.blackbox.Context
    
    // macro impls defined as a macro bundle
    class MyMacros(c: Context) {
      import c.universe._
 
      implicit val liftPoint = Liftable[points.Point] { p =>
        q"_root_.points.Point(${p.x}, ${p.y})"
      }

      // ...
    }

## Unlifting <a name="unlifting"> </a>

### Standard Unliftables <a name="standard-unliftables"> </a>

 Type                           | Representation        | Value
--------------------------------|-----------------------|------
 `Byte`, `Short`, `Int`, `Long` | `q"0"`                | `0`
 `Float`                        | `q"0.0"`              | `0.0`
 `Double`                       | `q"0.0D"`             | `0.0D`
 `Boolean`                      | `q"true"`, `q"false"` | `true`, `false`
 `Char`                         | `q"'c'"`              | `'c'`
 `Unit`                         | `q"()"`               | `()`
 `String`                       | `q""" "string" """`   | `"string"`
 `Symbol`                       | `q"'symbol"`          | `'symbol`
 `TermName`                     | `q"foo"`, `pq"foo"`   | `TermName("foo")`
 `TypeName`                     | `tq"foo"`             | `TypeName("foo")`
 `Type`                         | `tt: TypeTree`        | `tt.tpe`
 `Constant`                     | `lit: Literal`        | `lit.value`
 `TupleN[...]` \*               | `q"(1, 2)"`           | `(1, 2)`

 (\*) Unliftable for tuples is defined for all N in [2, 22] range. All type parameters have to be Unliftable themselves.

## Syntax summary <a name="syntax-summary"> </a>

### Abbreviations <a name="abbrev"> </a>

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

                                        | Quasiquote                                                  | Type
----------------------------------------|-------------------------------------------------------------|-------------------------
 [Empty](#empty-expr)                   | `q""`                                                       | EmptyTree
 [Literal](#literal)                    | `q"$value"`                                                 | Literal
 [Identifier](#term-ref)                | `q"$tname"` or `q"name"`                                    | Ident
 [Selection](#term-ref)                 | `q"$expr.$tname"`                                           | Select
 [Application](#application)            | `q"$expr(...$argss)"`                                       | Apply
 [Type Application] (#type-application) | `q"$expr[..$targs]"`                                        | TypeApply
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

                                       | Quasiquote                           | Type
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
 
                                             | Quasiquote            | Type                    
---------------------------------------------|------------------------|-------------------
 [Wildcard Pattern](#wilcard-pattern)        | `pq"_"`                | Ident
 [Binding Pattern](#binding-pattern)         | `pq"$tname @ $pat"`    | Bind
 [Extractor Pattern](#extractor-pattern)     | `pq"$ref(..$pats)"`    | Apply, UnApply   
 [Tuple Pattern](#tuple-pattern)             | `pq"(..$pats)"`        | Apply, UnApply
 [Type Pattern](#type-pattern)               | `pq"$tname: $tpt"`     | Typed  
 [Alternative Pattern](#alternative-pattern) | `pq"$first │ ..$rest"` | Alternative       
 
### Definitions <a name="defns-summary"> </a>

                                       | Quasiquote                                                                                                        | Type 
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

                                     | Quasiquote                 | Type
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
5. Encoding of partial funtions as match expressions with `q""` scrutinee.

Default toString formats `q""` as `<empty>`.

#### Literal <a name="literal"> </a>

Scala has a number of default built-in literals:
    
    q"1", q"1L"         // integer literals
    q"1.0", q"1.0D"     // floating point literals
    q"true", q"false"   // boolean literals
    q"'c'"              // character literal
    q""" "string" """   // string literal
    q"'symbol"          // symbol literal
    q"null"             // null literal
    q"()"               // unit literal

All of those values have Literal type except symbols which have different representation:
    
    scala> q"'symbol"
    res12: reflect.runtime.universe.Tree = scala.Symbol("symbol")

Thanks to [lifting](#lifting) you can also easily create literal trees directly from values of corresponding types:

    scala> val x = 1
    scala> q"$x"
    res13: reflect.runtime.universe.Tree = 1

This would work the same way for all literal types (see [standard liftables](#standard-liftables) except `Null`. Lifting of `null` value and `Null` type isn't supported, use `q"null"` if you really mean to create null literal:

    scala> val x = null
    scala> q"$x"
    <console>:31: error: Can't splice Null, bottom type values often indicate programmer mistake
                  q"$x"
                     ^

During deconstruction you can use [unlifting](#unlifting) to extract values out of Literal trees:

    scala> val q"${x: Int}" = q"1"
    x: Int = 1

Similarly it would work with all the literal types except `Null`. (see [standard unliftables](#standard-unliftables)) 

#### Identifier and Selection <a name="term-ref"> </a>

Identifiers and member selections are two fundamental primitives that let you refer to other definitions. Combination of two of them is also known as reference (or `RefTree`).

Each term identifier is defined by its name and by the fact of being backquoted or not:

    scala> val name = TermName("Foo")
    scala> q"$name"
    res13: reflect.runtime.universe.Ident = Foo

    scala> q"`$name`"
    res14: reflect.runtime.universe.Ident = `Foo`

Although backquoted and non-backquoted identifiers may refer to the same things they are not equivalent from synactical point of view:

    scala> val q"`Foo`" = q"Foo"
    scala.MatchError: Foo (of class scala.reflect.internal.Trees$Ident)
      ... 32 elided

This is caused by the fact that backquoted identifiers have different semantics in pattern patching.

Apart from matching on identifiers with given name you can also extract their name values with the help of [unlifting](#unlifting):

    scala> val q"${name: TermName}" = q"Foo"
    name: reflect.runtime.universe.TermName = Foo

Similarly you can create and extract member selections:

    scala> val member = TermName("bar")
    member: reflect.runtime.universe.TermName = bar

    scala> q"foo.$member"
    res17: reflect.runtime.universe.Select = foo.bar

    scala> val q"foo.$name" = q"foo.bar"
    name: reflect.runtime.universe.Name = bar

    scala> val q"foo.${name: TermName}" = q"foo.bar"
    name: reflect.runtime.universe.TermName = bar

#### Application and Type Application <a name="term-application"> </a>

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

Assign and update are two related ways to explictly mutate a variable or collection:

    scala> val assign = q"x = 2"
    assign: reflect.runtime.universe.Tree = x = 2

    scala> val update = q"array(0) = 1"
    update: reflect.runtime.universe.Tree = array.update(0, 1)

As you can see update syntax is just a syntactic sugar that gets represented as update method call on given object.

Nevertheless quasiquotes let you deconstruct both of them uniformly according to their user-facing syntax:

    scala> List(assign, update).foreach { case q"$left = $right" => println(s"left = $left, right = $right") }
    left = x, right = 2
    left = array(0), right = 1

Where `array(0)` has the same AST as function application. 

#### Return <a name="return"> </a>

Return expressions is used to perform early return from a function. 

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

Ascriptions lets users to annotate type of intermidiate expression:

    scala> val ascribed = q"(1 + 1): Int"
    ascribed: reflect.runtime.universe.Typed = (1.$plus(1): Int)

    scala> val q"$expr: $tpt" = ascribed
    expr: reflect.runtime.universe.Tree = 1.$plus(1)
    tpt: reflect.runtime.universe.Tree = Int

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

Quasiquotes also support deconstruction of tuples of arbitrary arity:

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

Case clause without body is equivalent to one holding unit literal:

    scala> val cq"$pat if $expr1 => $expr2" = cq"_ =>"
    pat: reflect.runtime.universe.Tree = _
    expr1: reflect.runtime.universe.Tree = <empty>
    expr2: reflect.runtime.universe.Tree = ()

No-guard is represented with the help of [empty expression](#empty-expr).

#### Try <a name="try"> </a>

Try expression is used to handle possible error conditions and ensure consistent state via finally. Both error handling cases and finally clause are optional.

    scala> val q"try $a catch { case ..$b } finally $c" = q"try t"
    a: reflect.runtime.universe.Tree = t
    b: List[reflect.runtime.universe.CaseDef] = List()
    c: reflect.runtime.universe.Tree = <empty>

    scala> val q"try $a catch { case ..$b } finally $c" = 
               q"try t catch { case _: C => }"
    a: reflect.runtime.universe.Tree = t
    b: List[reflect.runtime.universe.CaseDef] = List(case (_: C) => ())
    c: reflect.runtime.universe.Tree = <empty>

    scala> val q"try $a catch { case ..$b } finally $c" = 
               q"try t finally f"
    a: reflect.runtime.universe.Tree = t
    b: List[reflect.runtime.universe.CaseDef] = List()
    c: reflect.runtime.universe.Tree = f

Similarly to [pattern matching](#match) cases can be further deconstructed with `cq"..."`. 

#### Function <a name="function-expr"> </a>

There are three ways to create anonymous function:

    scala> val f1 = q"_ + 1"
    anon1: reflect.runtime.universe.Function = ((x$4) => x$4.$plus(1))

    scala> val f2 = q"(a => a + 1)"
    anon2: reflect.runtime.universe.Function = ((a) => a.$plus(1))

    scala> val f3 = q"(a: Int) => a + 1"
    anon3: reflect.runtime.universe.Function = ((a: Int) => a.$plus(1))

First one uses placeholder syntax. Second one names function parameter but still relies
on type inference to infer its type. Last one explicitly defines function parameter. Due
to implementation restriction second notation can only be used in parenthesis or inside other
expression. If you leave them out you have to specify parameter types.

Parameters are represented as [Vals](#val-var-definition). If you want to programmatically create val that should have 
its type inferred you need to use [empty type](#empty-type):

    scala> val tpt = tq""
    tpt: reflect.runtime.universe.TypeTree = <type ?>

    scala> val param = q"val x: $tpt"
    param: reflect.runtime.universe.ValDef = val x

    scala> q"($param => x)"
    res30: reflect.runtime.universe.Function = ((x) => x)

All of the given forms are represented in the same way and could be uniformly matched upon:

    scala> List(f1, f2, f3).foreach { 
             case q"(..$args) => $expr" => 
               println(s"args = $args, expr = $expr") 
           }
    args = List(<synthetic> val x$5 = _), expr = x$5.$plus(1)
    args = List(val a = _), expr = a.$plus(1)
    args = List(val a: Int = _), expr = a.$plus(1)

You can also tear arguments further apart:

    scala> val q"(..$args => $_)" = f3
    args: List[reflect.runtime.universe.ValDef] = List(val a: Int = _)

    scala> val List(q"$_ val $name: $tpt") = args
    name: reflect.runtime.universe.TermName = a
    tpt: reflect.runtime.universe.Tree = Int

It's recommended to use underscore pattern in place of modifiers even if you don't plan to work 
with them as parameters may contains additional flags which might cause match errors.

#### Partial Function <a name="partial-function"> </a>

Partial functions are a neat syntax that lets you express functions with
limited domain with the help of pattern matching:

    scala> val pf = q"{ case i: Int if i > 0 => i * i }"
    pf: reflect.runtime.universe.Match =
    <empty> match {
      case (i @ (_: Int)) if i.$greater(0) => i.$times(i)
    }

Under the hood they are represented as match trees with [empty](#empty-expr) scrutinee.

    scala> val q"{ case ..$cases }" = pf
    cases: List[reflect.runtime.universe.CaseDef] = List(case (i @ (_: Int)) if i.$greater(0) => i.$times(i))

    scala> val q"$expr match { case ..$cases }" = pf
    expr: reflect.runtime.universe.Tree = <empty>
    cases: List[reflect.runtime.universe.CaseDef] = List(case (i @ (_: Int)) if i.$greater(0) => i.$times(i))

#### While and Do-While Loops <a name="while"> </a>

While and do-while loops are low-level control structures that used when performance of iteration
is critical:

    scala> val `while` = q"while(x > 0) x -= 1"
    while: reflect.runtime.universe.LabelDef =
    while$6(){
      if (x.$greater(0))
        {
          x.$minus$eq(1);
          while$6()
        }
      else
        ()
    }

    scala> val q"while($cond) $body" = `while`
    cond: reflect.runtime.universe.Tree = x.$greater(0)
    body: reflect.runtime.universe.Tree = x.$minus$eq(1)

    scala> val `do-while` = q"do x -= 1 while (x > 0)"
    do-while: reflect.runtime.universe.LabelDef =
    doWhile$2(){
      x.$minus$eq(1);
      if (x.$greater(0))
        doWhile$2()
      else
        ()
    }

    scala> val q"do $body while($cond)" = `do-while`
    body: reflect.runtime.universe.Tree = x.$minus$eq(1)
    cond: reflect.runtime.universe.Tree = x.$greater(0)

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

#### Modifiers

#### Templates

#### Def Definition <a name="def-definition"> </a>

#### Val and Var Definitions <a name="val-var-definition"> </a>

#### Type Definition <a name="type-definition"> </a>

#### Class Definition <a name="class-definition"> </a>

#### Trait Definition <a name="trait-definition"> </a>

#### Object Definition <a name="object-definition"> </a>

#### Package and Package Object Definitions<a name="package-definition"> </a>

#### Import Definition <a name="import-definition"> </a>
