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

## Referential transparency <a name="referential-transparency"> </a>

## Lifting <a name="lifting"> </a>

Lifting is typeclass-based approach to define representation of custom data types as Trees. Its primary use-case is support unquoting of [literal](#literal) values and a number of reflection primitives as trees:

    scala> val two = 1 + 1
    two: Int = 2

    scala> q"$two + $two"
    res10: reflect.runtime.universe.Tree = 2.$plus(2)   

This code runs successfully because there is default `Liftable[Int]` instance that transforms integers into trees that represent literals. `Liftable` type is just a trait with a single absract method:

    trait Liftable[T] {
      def apply(value: T): Tree
    }

To define tree representation for your own data type just provide an implicit instance of `Liftable` for it:

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

Unlifting is the reverse operation to lifting: it takes a tree and recovers value from it:

    trait Unliftable[T] {
      def unapply(tree: Tree): Option[T]
    }

Due to the fact that tree might not be a represention of our data type, the return type of unapply is `Option[T]` rather than just `T`. Such signature also makes it easy to use `Unliftable` instances as extractors.

Whenever `Unliftable` is available for given data type you can use it for pattern matching with the help of ascription syntax:

    scala> val q"${left: Int} + ${right: Int}" = q"2 + 2"
    left: Int = 2
    right: Int = 2

    scala> left + right
    res4: Int = 4

It's important to note that unlifting will not be performed at locations where `Name`, `TermName` or `Modifiers` is extracted by default:

    scala> val q"foo.${bar: Int}" = q"foo.bar"
    <console>:29: error: pattern type is incompatible with expected type;
     found   : Int
     required: reflect.runtime.universe.NameApi
           val q"foo.${bar: Int}" = q"foo.bar"
                            ^

One can also successfully combine unquote splicing and unlifting:

    scala> val q"f(..${ints: List[Int]})" = q"f(1, 2, 3)"
    ints: List[Int] = List(1, 2, 3)

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
 [Super Selection](#super-this)         | `q"$tpname.super[$tpname].$tname"`                          | Select
 [This](#super-this)                    | `q"$tpname.this"`                                           | This
 [Application](#application)            | `q"$expr(...$argss)"`                                       | Apply
 [Type Application] (#application)      | `q"$expr[..$targs]"`                                        | TypeApply
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
------------------------------------------|---------------------------------------|---------------------
 [Empty Type](#empty-type)                | `tq""`                                | TypeTree
 [Type Identifier](#type-ident)           | `tq"$tpname"` or `tq"Name"`           | Ident
 [Singleton Type](#singleton-type)        | `tq"$ref.type"`                       | SingletonType
 [Type Projection](#type-projection)      | `tq"$tpt#$tpname"`                    | SelectFromTypeTree
 [Type Selection](#type-projection)       | `tq"$ref.$tpname"`                    | Select
 [Super Type Selection](#type-projection) | `tq"$tpname.super[$tpname].$tpname"`  | Select
 [This Type Selection](#type-projection)  | `tq"this.$tpname"`                    | Select
 [Applied Type](#applied-type)            | `tq"$tpt[..$tpts]"`                   | AppliedTypeTree
 [Compound Type](#compound-type)          | `tq"..$parents { ..$defns }"`         | CompoundTypeTree
 [Existential Type](#existential-type)    | `tq"$tpt forSome { ..$defns }"`       | ExistentialTypeTree
 [Tuple Type](#tuple-type)                | `tq"(..$tpts)"`                       | Tree
 [Function Type](#function-type)          | `tq"(..$tpts) => $tpt"`               | Tree

### Patterns <a name="pats-summary"> </a>
 
                                             | Quasiquote            | Type                    
---------------------------------------------|------------------------|-------------------
 [Wildcard Pattern](#wilcard-pattern)        | `pq"_"`                | Ident
 [Binding Pattern](#binding-pattern)         | `pq"$tname @ $pat"`    | Bind
 [Extractor Pattern](#extractor-pattern)     | `pq"$ref(..$pats)"`    | Apply, UnApply   
 [Type Pattern](#type-pattern)               | `pq"_: $tpt"`          | Typed  
 [Alternative Pattern](#alternative-pattern) | `pq"$first │ ..$rest"` | Alternative       
 [Tuple Pattern](#tuple-pattern)             | `pq"(..$pats)"`        | Apply, UnApply
 
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

Identifiers and member selections are two fundamental primitives that let you refer to other definitions. Combination of two of them is also known `RefTree`.

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


#### Super and This <a name="super-this"> </a>

One can use this and super to select precise members within inheritance chain.

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

#### Application and Type Application <a name="application"> </a>

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

For and For-Yield expressions allow to write monadic style comprehensions that desugar into calls to `map`, `flatMap`, `foreach` and `withFilter` methods:

    scala> val `for-yield` = q"for (x <- xs; if x > 0; y = x * 2) yield x"
    for-yield: reflect.runtime.universe.Tree =
    xs.withFilter(((x) => x.$greater(0))).map(((x) => {
      val y = x.$times(2);
      scala.Tuple2(x, y)
    })).map(((x$3) => x$3: @scala.unchecked match {
      case scala.Tuple2((x @ _), (y @ _)) => x
    }))

Each enumerator in the comprehension can be expressed with `fq"..."` interpolator:
 
    scala> val enums = List(fq"x <- xs", fq"if x > 0", fq"y = x * 2")
    enums: List[reflect.runtime.universe.Tree] = List(`<-`((x @ _), xs), `if`(x.$greater(0)), (y @ _) = x.$times(2))

    scala> val `for-yield` = q"for (..$enums) yield y"
    for-yield: reflect.runtime.universe.Tree 

Simiarly one can deconstruct for-yield back into a list of enumerators and body:

    scala> val q"for (..$enums) yield $body" = `for-yield`
    enums: List[reflect.runtime.universe.Tree] = List(`<-`((x @ _), xs), `if`(x.$greater(0)), (y @ _) = x.$times(2))
    body: reflect.runtime.universe.Tree = x

It's important to mention that For and For-Yield do not cross-match each other:

    scala> val q"for (..$enums) $body" = `for-yield`
    scala.MatchError: ...

#### New <a name="new"> </a>

### Types

#### Empty Type <a name="empty-type"> </a>

Empty type (`tq""`) is a canonical way to say that type at given location isn't given by the user and should be inferred by the compiler:

1. [Def](#def-definition) with unknown return type
2. [Val or Var](#val-var-definition) with unknown type
3. [Anonymous function](#function-expr) with unknown argument type

#### Type Identifier <a name="type-ident"> </a>

Similarly to [term identifiers](#term-ref) one can construct a type identifier based on a name:

    scala> val name = TypeName("Foo")
    name: reflect.runtime.universe.TypeName = Foo

    scala> tq"$name"
    res25: reflect.runtime.universe.Ident = Foo

And deconstruct it back through [unlifting](#unlifting):

    scala> val tq"${name: TypeName}" = tq"Foo"
    name: reflect.runtime.universe.TypeName = Foo

#### Singleton Type <a name="singleton-type"> </a>

A singleton type is a way to express a type of a term definition that is being referenced:

    scala> val singleton = tq"foo.bar.type".sr
    singleton: String = SingletonTypeTree(Select(Ident(TermName("foo")), TermName("bar")))

    scala> val tq"$ref.type" = tq"foo.bar.type"
    ref: reflect.runtime.universe.Tree = foo.bar

#### Type Projection <a name="type-projection"> </a>

Type projection is fundamental way to select types as members of other types:

    scala> val proj = tq"Foo#Bar"
    proj: reflect.runtime.universe.SelectFromTypeTree = Foo#Bar

    scala> val tq"$foo#$bar" = proj
    foo: reflect.runtime.universe.Tree = Foo
    bar: reflect.runtime.universe.TypeName = Bar

As a convenience one can also select type members of terms:

    scala> tq"scala.Int"
    res32: reflect.runtime.universe.Select = scala.Int

But semantically such selections are just a shortcut for a combination of singleton types and type projections:

    scala> tq"scala.type#Int"
    res33: reflect.runtime.universe.SelectFromTypeTree = scala.type#Int

Lastly [similarly to expressions](#super-this) one can select members through super and this:

    scala> tq"super.Bar"
    superbar: reflect.runtime.universe.Select = super.Bar

    scala> val tq"$pre.super[$parent].$field" = superbar 
    pre: reflect.runtime.universe.TypeName =
    parent: reflect.runtime.universe.TypeName =
    field: reflect.runtime.universe.Name = Bar

    scala> val thisfoo = tq"this.Foo"
    thisfoo: reflect.runtime.universe.Select = this.Foo

    scala> val tq"this.${tpname: TypeName}" = thisfoo
    tpname: reflect.runtime.universe.TypeName = Foo

#### Applied Type <a name="applied-type"> </a>

Instantiations of parametized types can be expressed with the help of applied types (type-level equivalent of type application):

    scala> val applied = tq"Foo[A, B]"
    applied: reflect.runtime.universe.Tree = Foo[A, B]

    scala> val tq"Foo[..$targs]" = applied
    targs: List[reflect.runtime.universe.Tree] = List(A, B)

Deconstruction of non-applied types will cause `targs` begin extracted as empty list:

    scala> val tq"Foo[..$targs]" = tq"Foo"
    targs: List[reflect.runtime.universe.Tree] = List()

#### Compound Type <a name="compound-type"> </a>

Compound type lets users to express a combination of a number of types with optional refined member list:

    scala> val compound = tq"A with B with C"
    compound: reflect.runtime.universe.CompoundTypeTree = A with B with C

    scala> val tq"..$parents { }" = compound
    parents: List[reflect.runtime.universe.Tree] = List(A, B, C)
    defns: List[reflect.runtime.universe.Tree] = List()

Braces after parents are required to signal that this type is a compound type even if there are no refinements and we just want to extract a sequence of types combined with `with` keyword.

On the other side of the spectrum are pure refinements without explicit parents (a.k.a. structural types):

    scala> val structural = tq"{ val x: Int; val y: Int }"
    structural: reflect.runtime.universe.CompoundTypeTree =
    scala.AnyRef {
      val x: Int;
      val y: Int
    }

    scala> val tq"{ ..$defns }" = structural
    defns: List[reflect.runtime.universe.Tree] = List(val x: Int, val y: Int)

Here we can see that AnyRef is a parent that is inserted implicitly if we don't provide any.

#### Existential Type <a name="existential-type"> </a>

#### Tuple Type <a name="tuple-type"> </a>

[Similarly to expressions](#tuple-expr), tuple types are just a syntactic sugar over `TupleN` classes:

    scala> val tup2 = tq"(A, B)"
    tup2: reflect.runtime.universe.Tree = scala.Tuple2[A, B]

    scala> val tq"(..$tpts)" = tup2
    tpts: List[reflect.runtime.universe.Tree] = List(A, B)

Analagously `Unit` type is considered to be nullary tuple:

    scala> val tq"(..$tpts)" = tq"_root_.scala.Unit"
    tpts: List[reflect.runtime.universe.Tree] = List()

It's important to mention that pattern matching of reference to `Unit` is limited to either fully qualified path or a reference that contains symbols. (see [referential transparency](#referential-transparency))

#### Function Type <a name="function-type"> </a>

Similarly to tuples, function types are a syntactic sugar over `FunctionN` classes:

    scala> val funtype = tq"(A, B) => C"
    funtype: reflect.runtime.universe.Tree = _root_.scala.Function2[A, B, C]

    scala> val tq"..$foo => $bar" = funtype
    foo: List[reflect.runtime.universe.Tree] = List(A, B)
    bar: reflect.runtime.universe.Tree = C

### Patterns

#### Wildcard Pattern <a name="wildcard-pattern"> </a>

Wildcard pattern (`pq"_"`) is the simplest form of pattern that matches any input.

#### Binding Pattern <a name="binding-pattern"> </a>

Binding pattern is a way to name pattern or one it's part as local variable:

    scala> val bindtup = pq"foo @ (1, 2)"
    bindtup: reflect.runtime.universe.Bind = (foo @ scala.Tuple2(1, 2))

    scala> val pq"$name @ $pat" = bindtup
    name: reflect.runtime.universe.Name = foo
    pat: reflect.runtime.universe.Tree = scala.Tuple2(1, 2)

Binding without explicit pattern is equivalent to the one with wildcard pattern:

    scala> val pq"$name @ $pat" = pq"foo"
    name: reflect.runtime.universe.Name = foo
    pat: reflect.runtime.universe.Tree = _

#### Extractor Pattern <a name="extractor-pattern"> </a>

Extractors are a neat way to delegate a pattern matching to another object's unapply method:

    scala> val extractor = pq"Foo(1, 2, 3)"
    extractor: reflect.runtime.universe.Tree = Foo(1, 2, 3)

    scala> val pq"$id(..$pats)" = extractor
    id: reflect.runtime.universe.Tree = Foo
    pats: List[reflect.runtime.universe.Tree] = List(1, 2, 3)

#### Type Pattern <a name="type-pattern"> </a>

Type patterns are a way to check type of a scrutinee:

    scala> val isT = pq"_: T"
    isT: reflect.runtime.universe.Typed = (_: T)

    scala> val pq"_: $tpt" = isT
    tpt: reflect.runtime.universe.Tree = T

Combination of non-wildcard name and type pattern is represented as bind over wildcard type pattern:

    scala> val fooIsT = pq"foo: T"
    fooIsT: reflect.runtime.universe.Bind = (foo @ (_: T))

    scala> val pq"$name @ (_: $tpt)" = fooIsT
    name: reflect.runtime.universe.Name = foo
    tpt: reflect.runtime.universe.Tree = T

Another important thing to mention is a type variable patterns:

    scala> val typevar = pq"_: F[t]"
    typevar: reflect.runtime.universe.Typed = (_: F[(t @ <empty>)])

One can construct (and similarly deconstruct) such patterns by following steps:

    scala> val name = TypeName("t")
    name: reflect.runtime.universe.TypeName = t

    scala> val t = pq"$name"
    t: reflect.runtime.universe.Bind = (t @ _)

    scala> val tpt = tq"F[$t]"
    tpt: reflect.runtime.universe.Tree = F[(t @ _)]

    scala> val typevar = pq"_: $tpt"
    typevar: reflect.runtime.universe.Typed = (_: F[(t @ _)])

#### Alternative Pattern <a name="alternative-pattern"> </a>

Pattern alternatives represent a pattern that matches whenever at least one of the branches matches:

    scala> val alt = pq"Foo() | Bar() | Baz()"
    alt: reflect.runtime.universe.Alternative = (Foo()| Bar()| Baz())

    scala> val pq"$first | ..$rest" = alt
    head: reflect.runtime.universe.Tree = Foo()
    tail: List[reflect.runtime.universe.Tree] = List(Bar(), Baz())

    scala> val pq"..$init | $last" = alt
    init: List[reflect.runtime.universe.Tree] = List(Foo(), Bar())
    last: reflect.runtime.universe.Tree = Baz()

#### Tuple Pattern <a name="tuple-pattern"> </a>

Similarly to [tuple expressions](#tuple-type) and [tuple types](#tuple-type), tuple patterns are just a syntactic sugar that expands as `TupleN` extractor:

    scala> val tup2pat = pq"(a, b)"
    tup2pat: reflect.runtime.universe.Tree = scala.Tuple2((a @ _), (b @ _))

    scala> val pq"(..$pats)" = tup2pat
    pats: List[reflect.runtime.universe.Tree] = List((a @ _), (b @ _))

### Definitions

#### Modifiers <a name="modifiers"> </a>

#### Template <a name="template"> </a>

#### Def Definition <a name="def-definition"> </a>

#### Val and Var Definitions <a name="val-var-definition"> </a>

#### Type Definition <a name="type-definition"> </a>

#### Class Definition <a name="class-definition"> </a>

#### Trait Definition <a name="trait-definition"> </a>

#### Object Definition <a name="object-definition"> </a>

#### Package and Package Object Definitions<a name="package-definition"> </a>

#### Import Definition <a name="import-definition"> </a>
