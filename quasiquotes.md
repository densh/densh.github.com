---
layout: page 
title: Quasiquote guide (WIP)
---

{:#toc}
* Table of contents.
{:toc}

## Before you start {:#before-you-start} 

Before you start reading this guide it's recommended to start a Scala REPL with one extra line:

    scala> val universe = reflect.runtime.universe; import universe._

REPL is the best place to explore quasiquotes and this guide will use it extensively to demonstrate handling of trees. All of the examples will assume that import.

Additionally some examples that use `ToolBox` API might need a few more lines to get things rolling:

    scala> import reflect.runtime.currentMirror
    scala> import tools.reflect.ToolBox
    scala> val toolbox = currentMirror.mkToolBox()

Another tool you might want to be aware of is new and shiny `showCode` pretty printer (contributed by [@VladimirNik](https://github.com/VladimirNik)):

    scala> val C = q"class C"
    C: universe.ClassDef =
    class C extends scala.AnyRef {
      def <init>() = {
        super.<init>();
        ()
      }
    }

    scala> println(showCode(C))
    class C

Default pretty printer shows you contents of the tree in imaginary low-level Scala-like notation. `showCode` on the other hand will do its best to reconstruct actual source code equivalent to the given tree in proper Scala syntax.

On the other side of spectrum there is also a `showRaw` pretty printer that shows direct internal organization of the tree:

    scala> println(showRaw(q"class C"))
    ClassDef(Modifiers(), TypeName("C"), List(), Template(List(Select(Ident(scala), TypeName("AnyRef"))), noSelfType, List(DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))))))

## Intro {:#intro}

Quasiquotes are a neat notation that lets you manipulate Scala syntax trees with ease:

    scala> val tree = q"i am { a quasiquote }"
    tree: universe.Tree = i.am(a.quasiquote)

Every time you wrap a snippet of code into `q"..."` quotation it would become a tree that represents given snippet. As you might have already noticed quotation syntax is in just another usage of extensible string interpolation introduced in 2.10. Although they look like strings they operate on syntactic trees under the hood. 

The same syntax can be used to match trees as patterns:

    scala> println(tree match { case q"i am { a quasiquote }" => "it worked!" })
    it worked!

Whenever you match a tree with a quasiquote it would match whenever a *structure* of given tree is equivalent to the one you\'ve provided as a pattern. You can check for structural equality manually with the help of `equalsStructure` method:

    scala> println(q"foo + bar" equalsStructure q"foo.+(bar)")
    true

You can also put things into quasiquotation with the help of `$`:

    scala> val aquasiquote = q"a quasiquote"
    aquasiquote: universe.Select = a.quasiquote

    scala> val tree = q"i am { $aquasiquote }"
    tree: universe.Tree = i.am(a.quasiquote)

This operation is also known as unquoting. Whenever you unquote an expression of `Tree` type in a quasiquote it will *structurally substitute* that tree into that location. Most of the time such substitution between quotes is equivalent to textual substitution of the source code.

Similarly one can structurally deconstruct a tree using unquoting in pattern matching:

    scala> val q"i am $what" = q"i am { a quasiquote }"
    what: universe.Tree = a.quasiquote

## Interpolators {:#interpolators}

Scala is the language with rich syntax that differs greatly depending on the syntactical context:

    scala> val x = q"""
             val x: List[Int] = List(1, 2) match { 
               case List(a, b) => List(a + b)
             }
           """
    x: universe.ValDef =
    val x: List[Int] = List(1, 2) match {
      case List((a @ _), (b @ _)) => List(a.$plus(b))
    }

In this example we see three primary contexts being used:

1. `List(1, 2)` and `List(a + b)` are expressions
2. `List[Int]` is a type
3. `List(a, b)` is a pattern

Each of this contexts is covered by separate interpolator:

    | Used for 
----|----------------------------------------------------------------
 q  | [expressions](#exprs-summary), [definitions](#defns-summary) and [imports](#import)
 tq | [types](#types-summary)
 pq | [patterns](#pats-summary)

Syntactical similiarity between different contexts doesn\'t imply similarity between underlying trees:

    scala> println(q"List[Int]" equalsStructure tq"List[Int]")
    false

If we peek under the hood we'll see that trees are indeed different:

    scala> println(showRaw(q"List[Int]"))
    TypeApply(Ident(TermName("List")), List(Ident(TypeName("Int"))))

    scala> println(showRaw(tq"List[Int]"))
    AppliedTypeTree(Ident(TypeName("List")), List(Ident(TypeName("Int"))))

Similarly patterns and expressions are not equivalent either:

    scala> println(pq"List(a, b)" equalsStructure q"List(a, b)")
    false

So it's extremely important to use the right interpotor for the job to construct a valid syntax tree.

Additionally there are two auxilary interpolators that let you work with minor areas of scala syntax:

    | Used for
----|-------------------------------------
 cq | [case clause](#aux-summary)
 fq | [for loop enumerator](#aux-summary)

See [syntax overview](#syntax-overview) section for details.

## Splicing {:#splicing}

Unquote splicing is a way to unquote a variable number of elements:

    scala> val ab = List(q"a", q"b")
    scala> val fab = q"f(..$ab)"
    fab: universe.Tree = f(a, b)

Dots near unquotee annotate degree of flattenning and are also called splicing rank. `..$` expects argument to be an `Iterable[Tree]` and `...$` expects `Iterable[Iterable[Tree]]`. 

Splicing can be easily combined with regular unquotation:

    scala> val c = q"c"
    scala> val fabc = q"f(..$ab, $c)"
    fabc: universe.Tree = f(a, b, c)

    scala> val fcab = q"f($c, ..$ab)"
    fcab: universe.Tree = f(c, a, b)

    scala> val fabcab = q"f(..$ab, $c, ..$ab)"
    fabcab: universe.Tree = f(a, b, c, a, b)

If you want to abstract over applications even further you can use `...$`:

    scala> val argss = List(ab, List(c))
    arglists: List[List[universe.Ident]] = List(List(a, b), List(c))

    scala> val fargss = q"f(...$argss)"
    fargss: universe.Tree = f(a, b)(c)

At the moment `...$` splicing is only supported for function applications and parameter lists in def and class definitions.

Similarly to construction one can also use `..$` and `...$` to tear trees apart:

    scala> val q"f(..$args)" = q"f(a, b)"
    args: List[universe.Tree] = List(a, b)

    scala> val q"f(...$argss)" = q"f(a, b)(c)"
    argss: List[List[universe.Tree]] = List(List(a, b), List(c))

Although there are some limitations to the way to you can combine it with regular `$` variable extraction:

    case q"f($first, ..$rest)" => // ok
    case q"f(..$init, $last)"  => // ok
    case q"f(..$a, ..$b)"      => // not allowed

So in general only one `..$` is allowed per given list. Similar restrictions also apply to `...$`:

    case q"f(..$first)(...$rest)" => // ok
    case q"f(...$init)(..$first)" => // ok
    case q"f(...$a)(...$b)"       => // not allowed

In this section we only worked with function arguments but the same splicing rules are true for all syntax forms with variable amount of elements. [Syntax overview](#syntax-overview) and corresponding [syntax details](#syntax-details) sections demonstrate how you can splice into other syntactic forms.

## Referential transparency and hygiene {:#referential-transparency}

In 2.11 quasiquotes are not referentially transparent meaning that they don\'t have any knowledge of lexical context around them. For example:

    scala> import collection.mutable.Map

    scala> def typecheckType(tree: Tree): Type =
             toolbox.typecheck(tree, toolbox.TYPEmode).tpe

    scala> typecheckType(tq"Map[_, _]") =:= typeOf[Map[_, _]]
    false

    scala> typecheckType(tq"Map[_, _]") =:= typeOf[collection.immutable.Map[_, _]]
    true
      
Here we can see that plain reference to `Map` doesn\'t respect our custom import and resolves to default `collection.immutable.Map` instead. 

Similar problems can arise if references aren't fully qualified in macros. Current macro system isn't hygienic and it doesn't resolve name clashes between result of the macro and lexical scope where macro is used:

    // ---- MyMacro.scala ----
    package example

    import reflect.macros.blackbox.Context
    import language.experimental.macros

    object MyMacro {
      def wrapper(x: Int) = { println(s"wrapped x = $x"); x }
      def apply(x: Int): Int = macro impl
      def impl(c: Context)(x: c.Tree) = { import c.universe._
        q"wrapper($x)"
      }
    }

    // ---- Test.scala ----
    package example

    object Test extends App {
      def wrapper(x: Int) = x 
      MyMacro(2)
    }

If we compile both macro and it's usage we'll see that `println` will not be called when application runs. This will happen because after macro expansion `Test.scala` will look like:

    // Expanded Test.scala
    package example

    object Test extends App {
      def wrapper(x: Int) = x
      wrapper(2)
    }   

And wrapper will be resolved to `example.Test.wrapper` rather than intended `example.MyMacro.wrapper`. To avoid this kind of errors one can use two possible workarounds:

1. Fully qualify all references. i.e. we can adapt our macros' implementation to:

       def impl(c: Context)(x: c.Tree) = { import c.universe._
         q"_root_.example.MyMacro.wrapper($x)"
       }

   It's important to start with `_root_` as otherwise there will still be a chance of name collision if `example` gets redefined at use-site of the macro. 

2. Unquote symbols instead of using plain identifiers. i.e. we can resolve reference to wrapper by hand:

       def impl(c: Context)(x: c.Tree) = { import c.universe._
         val myMacro = symbolOf[MyMacro.type].asClass.module
         val wrapper = myMacro.info.member(TermName("wrapper"))
         q"$wrapper($x)"
       }

Fixing these issues is the number one priority for 2.12. (see [future prospects](#future))

## Lifting {:#lifting}

Lifting is and extensible way to unquote custom data types in quasiquotes. Its primary use-case is support unquoting of [literal](#literal) values and a number of reflection primitives as trees:

    scala> val two = 1 + 1
    two: Int = 2

    scala> val four = "$two + $two"
    four: universe.Tree = 2.$plus(2)   

This code runs successfully because `Int` is considered to be `Liftable` by default. `Liftable` type is just a trait with a single absract method that defines a mapping of given type to tree:

    trait Liftable[T] {
      def apply(value: T): Tree
    }

Whenever there is implicit value of `Liftable[T]` is available one can unquote `T` in quasiquote. This design pattern is known as a type class. You can read more about it in ["Type Classes as Objects and Implicits"](http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf).

A number of data types that are supported natively by quasiquotes will never triger usage of `Liftable` representation even if it\'s available: subtypes of `Tree`, `Symbol`, `Name`, `Modifiers` and `FlagSet`.

One can also combine lifting and unquote splicing:

    scala> val ints = List(1, 2, 3)
    scala> val f123 = q"f(..$ints)"
    f123: universe.Tree = f(1, 2, 3)

    scala> val intss = List(List(1, 2, 3), List(4, 5), List(6))
    scala> val f123456 = q"f(...$intss)"
    f123456: reflect.runtime.universe.Tree = f(1, 2, 3)(4, 5)(6)

In this case each element of the list will be lifted separately and the result will be spliced right in. 

### Bring your own {:#bring-your-own-liftable}

To define tree representation for your own data type just provide an implicit instance of `Liftable` for it:

    package points

    import scala.universe._ 

    case class Point(x: Int, y: Int)
    object Point {
      implicit val lift = Liftable[Point] { p => 
        q"_root_.points.Point(${p.x}, ${p.y})" 
      }
    }

This way whenever a value of Point type is unquoted in runtime quasiquote it will be automatically transformed
into a case class constructor call. In this example there two important points to take into account:

0. Liftable companion contains helper `apply` method to simplifies creation of `Liftable` instances.
   It takes a single type parameter `T` and a `T => Tree` function as a single value parameter and 
   returns a `Liftable[T]`.

1. Here we only defined `Liftable` for runtime reflection. It won't be found if you try to
   use it from a macro due to the fact that each universe contains its own `Liftable` which is not
   compatible with the others. This problem is caused by path-dependant nature of current reflection
   api. (see [sharing liftable implementation between universes](#reusing-liftable-impl))

2. Due to lack of [referential transparency](#referential-transparency), reference to point companion
   has to be fully qualified to ensure correctness in of this tree in every possible context. Another
   way to workaround reference issue is to use symbols to refer to things:

       val PointSym = symbolOf[Point].companionModule
       implicit val lift = Liftable[Point] { p =>
         q"$PointSym(${p.x}, ${p.y})"
       }

### Standard Liftables {:#standard-liftables}

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

### Reusing Liftable implementation between universes {:#reusing-liftable-impl}

Due to path dependent nature of current reflection API it isn't trivial to share the same Liftable definition between both macro and runtime universes. A possible way to do this is to define Liftable implementations in a trait and instantiate it for each universe separately:

    import reflect.api.Universe
    import reflect.macros.blackbox.Context

    trait LiftableImpls {
      val universe: Universe
      import universe._

      implicit val liftPoint = Liftable[points.Point] { p =>
        q"_root_.points.Point(${p.x}, ${p.y})"
      }
    }

    object RuntimeLiftableImpls extends LiftableImpls {
      val universe: universe.type = universe
    }

    trait MacroLiftableImpls extends LiftableImpls {
      val c: Context
      val universe: c.universe.type = c.universe
    }

    // macro impls defined as a bundle
    class MyMacro(val c: Context) extends MacroLiftableImpls {
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

## Unlifting {:#unlifting}

Unlifting is the reverse operation to [lifting](#lifting): it takes a tree and recovers value from it:

    trait Unliftable[T] {
      def unapply(tree: Tree): Option[T]
    }

Due to the fact that tree might not be a represention of our data type, the return type of unapply is `Option[T]` rather than just `T`. Such signature also makes it easy to use `Unliftable` instances as extractors.

Whenever implicit instance of `Unliftable` is available for given data type you can use it for pattern matching with the help of ascription syntax:

    scala> val q"${left: Int} + ${right: Int}" = q"2 + 2"
    left: Int = 2
    right: Int = 2

    scala> left + right
    res4: Int = 4

It's important to note that unlifting will not be performed at locations where `Name`, `TermName` or `Modifiers` is extracted by default:

    scala> val q"foo.${bar: Int}" = q"foo.bar"
    <console>:29: error: pattern type is incompatible with expected type;
     found   : Int
     required: universe.NameApi
           val q"foo.${bar: Int}" = q"foo.bar"
                            ^

One can also successfully combine unquote splicing and unlifting:

    scala> val q"f(..${ints: List[Int]})" = q"f(1, 2, 3)"
    ints: List[Int] = List(1, 2, 3)

    scala> val q"f(...${intss: List[List[Int]]})" = q"f(1, 2, 3)(4, 5)(6)"
    intss: List[List[Int]] = List(List(1, 2, 3), List(4, 5), List(6))

Analogously to lifting it would unlift arguments of the function elementwise and wrap the result into a list.

### Bring your own {:#bring-your-own-unliftable}

Similarly to liftables one can define your own unliftables:

    package Points
 
    import scala.universe._

    case class Point(x: Int, y: Int) 
    object Point {
      implicit val unliftPoint = Unliftable[points.Point] {
        case q"_root_.points.Point(${x: Int}, ${y: Int})" => Point(x, y)
      }
    }

Here one needs to pay attention to a few nuances:

0. Similarly to `Liftable`, `Unliftable` defines a helper `apply` function in companion
   to simplify creation of `Unliftable` instances which takes a type parameter `T` and 
   a partial function `PartialFunction[Tree, T]` and returns `Unliftable[T]`. At all 
   inputs where partial function is defined it's expected to unconditionally return
   instance of `T`.

1. We only define `Unliftable` for runtime universe, it won't be available in macros.
   (see [sharing liftable implementations](#reusing-liftable-impl))

2. Pattern used in this unliftable will only match fully qualified reference to Point that
   starts with `_root_`. It won't match other possible shapes of the reference and they have
   to be specified by hand. This problem is caused by lack of [referential transparency](#referential-transparency).

3. The pattern will also only match trees that have literal `Int` arguments. 
   It won't work for other expressions that might evaluate to `Int`.

### Standard Unliftables {:#standard-unliftables}

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

## Use cases {:#use-cases}

### AST manipulation in macros and compiler plugins {:#ast-manipulation}

Quasiquotes were designed primary as tool for ast manipulation in macros. Common workflow is to deconstruct arguments with quasiquotes patterns and construct rewritten result with another quasiquote:

    // macro that prints expression code before executing it
    object debug {
      def apply[T](x: =>T): T = macro impl
      def impl(c: Context)(x: c.Tree) = { import c.universe._
        val q"..$stats" = x
        val loggedStats = stats.flatMap { stat =>
          val msg = "executing " + showCode(stat)
          List(q"println($msg)", stat)
        }
        q"..$loggedStats"
      }
    }

    // usage
    object Test extends App {
      def faulty: Int = throw new Exception
      debug { 
        val x = 1
        val y = x + faulty
        x + y
      }
    }

    // output
    executing val x: Int = 1
    executing val y: Int = x.+(Test.this.faulty)
    java.lang.Exception
    ...

To simplify integration with macros we've also made it easier to just use trees in macro implementations instead of previous reify-centric `Expr` api:

    // 2.10
    object Macro {
      def apply(x: Int): Int = macro impl
      def impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = { import c.universe._
        c.Expr(q"$x + 1")
      }
    }
    
    // in 2.11 you can also do it like that
    object Macro {
      def apply(x: Int): Int = macro impl
      def impl(c: Context)(x: c.Tree) = { import c.universe._
        q"$x + 1"
      }
    }

You don't have to manually wrap return value of a macro into `c.Expr` or specify argument types twice any more. Return type in `impl` is optional too. (see also [quasiquotes vs reify](#quasiquotes-vs-reify))

Quasiquotes can also be used as is in compiler plugins as reflection api is strict subset of compiler's `Global` api. 

### DSLs {:#dsls}

Thanks to untyped nature of quasiquotes and rich Scala syntax one can define their own domain specific languages with the help of it:

    import reflect.runtime.universe._

    val config = Config(q"""
      endpoint {
        listen      = 80
        server_name = "domain.com"
        redirect_to = "www.domain.com"
      }
      endpoint {
        listen      = 80
        server_name = "www.domain.com"
        root        = "/home/domain.com"
      }
    """)

Where config constructor would just interprete code snippet with the help of patten matching:

    import reflect.runtime.universe._

    final case class Endpoint(props: Map[String, Any])
    final case class Config(endpoints: List[Endpoint])
    object Config {
      def incorrect(msg: String, tree: Tree): Nothing =
        throw new IllegalArgumentException(s"incorrect $msg: ${showCode(tree)}")
      implicit val unliftEndpoint = Unliftable[Endpoint] {
        case q"endpoint { ..$props }" =>
          Endpoint(props.map { 
            case q"${name: Name} = ${value: Int}" =>
              (name.toString, value)
            case q"${name: Name} = ${value: String}" =>
              (name.toString, value)
            case prop =>
              incorrect("property definition", prop)
          }.toMap)
        case endpoint =>
          incorrect("endpoint definition", endpoint)
      }
      def apply(config: Tree): Config = config match {
        case q"..${endpoints: List[Endpoint]}" => Config(endpoints) 
      }
    }

It's also possible to pass such definition as string and use `ToolBox` to parse it into a tree.    

### Just in time compilation 

Thanks to `ToolBox` api one can generate, compile and run Scala code at runtime:

    scala> val code = q"""println("compiled and run at runtime!")"""
    scala> val compiledCode = toolbox.compile(code)
    scala> val result = compiledCode()
    compiled and run at runtime!
    result: Any = ()

### Offline code generation 

Thanks to new `showRaw` pretty printer one can implement offline code generator that does AST manipulation with the help of quasiquotes and then serializes into actual source right before writing them to disk:

    object OfflineCodeGen extends App {
      def generateCode() =
        q"package mypackage { class MyClass }"
      def saveToFile(path: String, code: Tree) = {
        val writer = new java.io.PrintWriter(path)
        try writer.write(showCode(code))
        finally writer.close()
      }
      saveToFile("myfile.scala", generateCode())
    }

## Quasiquotes vs reify {:#quasiquotes-vs-reify}

## Syntax overview {:#syntax-overview}

### Abbreviations {:#abbrev}

* `defns: List[Tree]` where each element is Val, Var, Def or Type definition 
* `early: List[Tree]` where each element is early definition
* `enums: List[Tree]` where each element is a for loop enumerator
* `expr: Tree` that contains an expression
* `exprs: List[Tree]` where each element is an expression
* `exprss: List[List[Tree]]` where each element is an expression
* `name: Name`
* `params: List[Tree]` where each element is a parameter
* `paramss: List[List[Tree]]` where each element is a parameter
* `parents: List[Tree]` where each element is a parent
* `pat: Tree` that contains a pattern
* `pats: List[Tree]` where each element is a pattern
* `self: Tree` that corresponds to self type definition
* `sels: List[Tree]` where each element is an import selector
* `stats: List[Tree]` where each element is an expression, definition or import
* `tname: TermName`
* `topstats: List[Tree]` where each element is Class, Trait, Object or Package definition
* `tpname: TypeName`
* `tpt: Tree` that contains a type
* `tpts: List[Tree]` where each element is a type
* `value: T` where `T` is value type that corresponds to given literal (e.g. `Int`, `Char`, `Float` etc)

### Expressions {:#exprs-summary}

                                        | Quasiquote                                                  | Type
----------------------------------------|-------------------------------------------------------------|-------------------------
 [Empty](#empty-expr)                   | `q""`                                                       | EmptyTree
 [Literal](#literal)                    | `q"$value"`                                                 | Literal
 [Identifier](#term-ref)                | `q"$tname"` or `q"name"`                                    | Ident
 [Selection](#term-ref)                 | `q"$expr.$tname"`                                           | Select
 [Super Selection](#super-this)         | `q"$tpname.super[$tpname].$tname"`                          | Select
 [This](#super-this)                    | `q"$tpname.this"`                                           | This
 [Application](#application)            | `q"$expr(...$exprss)"`                                      | Apply
 [Type Application] (#application)      | `q"$expr[..$tpts]"`                                         | TypeApply
 [Assign](#assign-update)               | `q"$expr = $expr"`                                          | Assign, AssignOrNamedArg
 [Update](#assign-update)               | `q"$expr(..$exprs) = $expr"`                                | Tree
 [Return](#return)                      | `q"return $expr"`                                           | Return
 [Throw](#throw)                        | `q"throw $expr"`                                            | Throw
 [Ascription](#ascription)              | `q"$expr: $tpt"`                                            | Typed
 [Annotated](#annotated-expr)           | `q"$expr: @$annot"`                                         | Annotated
 [Tuple](#tuple-expr)                   | `q"(..$exprs)"`                                             | Tree
 [Block](#block)                        | `q"{ ..$stats }"`                                           | Block
 [If](#if)                              | `q"if ($expr) $expr else $expr"`                            | If
 [Pattern Match](#match)                | `q"$expr match { case ..$cases }"`                          | Match
 [Try](#try)                            | `q"try $expr catch { case ..$cases } finally $expr"`        | Try
 [Function](#function-expr)             | `q"(..$params) => $expr"`                                   | Function
 [Partial Function](#partial-function)  | `q"{ case ..$cases }"`                                      | Match
 [While Loop](#while)                   | `q"while ($expr) $expr"`                                    | LabelDef 
 [Do-While Loop](#while)                | `q"do $expr while ($expr)"`                                 | LabelDef
 [For Loop](#for)                       | `q"for (..$enums) $expr"`                                   | Tree
 [For-Yield Loop](#for)                 | `q"for (..$enums) yield $expr"`                             | Tree
 [New](#new)                            | `q"new { ..$early } with ..$parents { $self => ..$stats }"` | Tree

### Types {:#types-summary}

                                          | Quasiquote                            | Type
------------------------------------------|---------------------------------------|---------------------
 [Empty Type](#empty-type)                | `tq""`                                | TypeTree
 [Type Identifier](#type-ident)           | `tq"$tpname"` or `tq"Name"`           | Ident
 [Singleton Type](#singleton-type)        | `tq"$ref.type"`                       | SingletonType
 [Type Projection](#type-projection)      | `tq"$tpt#$tpname"`                    | SelectFromTypeTree
 [Type Selection](#type-projection)       | `tq"$ref.$tpname"`                    | Select
 [Super Type Selection](#type-projection) | `tq"$tpname.super[$tpname].$tpname"`  | Select
 [This Type Selection](#type-projection)  | `tq"this.$tpname"`                    | Select
 [Applied Type](#applied-type)            | `tq"$tpt[..$tpts]"`                   | AppliedTypeTree
 [Annotated Type](#annotated-type)        | `tq"$tpt @$annots"`                   | Annotated 
 [Compound Type](#compound-type)          | `tq"..$parents { ..$defns }"`         | CompoundTypeTree
 [Existential Type](#existential-type)    | `tq"$tpt forSome { ..$defns }"`       | ExistentialTypeTree
 [Tuple Type](#tuple-type)                | `tq"(..$tpts)"`                       | Tree
 [Function Type](#function-type)          | `tq"(..$tpts) => $tpt"`               | Tree

### Patterns {:#pats-summary}
 
                                             | Quasiquote             | Type                    
---------------------------------------------|------------------------|-------------------
 [Wildcard Pattern](#wildcard-pattern)       | `pq"_"`                | Ident
 [Binding Pattern](#binding-pattern)         | `pq"$name @ $pat"`     | Bind
 [Extractor Pattern](#extractor-pattern)     | `pq"$ref(..$pats)"`    | Apply, UnApply   
 [Type Pattern](#type-pattern)               | `pq"_: $tpt"`          | Typed  
 [Alternative Pattern](#alternative-pattern) | `pq"$first │ ..$rest"` | Alternative       
 [Tuple Pattern](#tuple-pattern)             | `pq"(..$pats)"`        | Apply, UnApply
 
### Definitions {:#defns-summary}

                                   | Quasiquote                                                                                                           | Type 
-----------------------------------|----------------------------------------------------------------------------------------------------------------------|-----------
 [Val](#val-var)                   | `q"$mods val $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                   | ValDef
 [Var](#val-var)                   | `q"$mods var $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                   | ValDef
 [Val Pattern](#pattern-def)       | `q"$mods val $pat: $tpt = $expr"`                                                                                    | Tree
 [Var Pattern](#pattern-def)       | `q"$mods var $pat: $tpt = $expr"`                                                                                    | Tree
 [Method](#method)                 | `q"$mods def $tname[..$targs](...$paramss): $tpt = $expr"`                                                           | DefDef
 [Type](#type-def)                 | `q"$mods type $tpname[..$targs] = $tpt"`                                                                             | TypeDef
 [Class](#class)                   | `q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }"` | ClassDef
 [Trait](#trait)                   | `q"$mods trait $tpname[..$targs] extends { ..$early } with ..$parents { $self => ..$stats }"`                        | TraitDef
 [Object](#object)                 | `q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }"`                                   | ModuleDef
 [Package](#package)               | `q"package $ref { ..$topstats }"`                                                                                    | PackageDef
 [Package Object](#package-object) | `q"package object $tname extends { ..$early } with ..$parents { $self => ..$stats }"`                                | PackageDef

### Auxiliary {:#aux-summary}

                                     | Quasiquote                  | Type
-------------------------------------|-----------------------------|--------
 [Import](#import)                   | `q"import $ref.{..$sels}"`  | Import
 [Case Clause](#match)               | `cq"$pat if $expr => expr"` | CaseDef
 [Generator Enumerator](#for)        | `fq"$pat <- $expr"`         | Tree
 [Value Definition Enumerator](#for) | `fq"$pat = $expr"`          | Tree
 [Guard Enumerator](#for)            | `fq"if $expr"`              | Tree

## Syntax details {:#syntax-details} 

### Expressions

#### Empty {:#empty-expr}

`q""` is used to indicate that some part of the tree is not provided by the user:

1. Vals, Vars and Defs without right-hand side have it set to `q""`.
2. Type definitions without bounds have them set to `q""`.
3. Try expressions without finally clause have it set to `q""`.
4. Case clauses without guards have them set to `q""`.

Default toString formats `q""` as `<empty>`.

#### Literal {:#literal}

Scala has a number of default built-in literals:
    
    q"1", q"1L"              // integer literals
    q"1.0f", q"1.0", q"1.0d" // floating point literals
    q"true", q"false"        // boolean literals
    q"'c'"                   // character literal
    q""" "string" """        // string literal
    q"'symbol"               // symbol literal
    q"null"                  // null literal
    q"()"                    // unit literal

All of those values have Literal type except symbols which have different representation:

    scala> val foo = q"'foo"
    foo: universe.Tree = scala.Symbol("foo")

Thanks to [lifting](#lifting) you can also easily create literal trees directly from values of corresponding types:

    scala> val x = 1
    scala> val one = q"$x"
    one: universe.Tree = 1

This would work the same way for all literal types (see [standard liftables](#standard-liftables) except `Null`. Lifting of `null` value and `Null` type isn't supported, use `q"null"` if you really mean to create null literal:

    scala> val x = null
    scala> q"$x"
    <console>:31: error: Can't unquote Null, bottom type values often indicate programmer mistake
                  q"$x"
                     ^

During deconstruction you can use [unlifting](#unlifting) to extract values out of Literal trees:

    scala> val q"${x: Int}" = q"1"
    x: Int = 1

Similarly it would work with all the literal types except `Null`. (see [standard unliftables](#standard-unliftables)) 

#### Identifier and Selection {:#term-ref}

Identifiers and member selections are two fundamental primitives that let you refer to other definitions. Combination of two of them is also known `RefTree`.

Each term identifier is defined by its name and by the fact of being backquoted or not:

    scala> val name = TermName("Foo")
    name: universe.TermName = Foo

    scala> val foo = q"$name"
    foo: universe.Ident = Foo

    scala> val backquoted = q"`$name`"
    backquoted: universe.Ident = `Foo`

Although backquoted and non-backquoted identifiers may refer to the same things they are not equivalent from synactical point of view:

    scala> val q"`Foo`" = q"Foo"
    scala.MatchError: Foo (of class scala.reflect.internal.Trees$Ident)
      ... 32 elided

This is caused by the fact that backquoted identifiers have different semantics in pattern patching.

Apart from matching on identifiers with given name you can also extract their name values with the help of [unlifting](#unlifting):

    scala> val q"${name: TermName}" = q"Foo"
    name: universe.TermName = Foo

Name ascription is important here as without it you\'ll get pattern that is equivalent to regular pattern variable binding.

Similarly you can create and extract member selections:

    scala> val member = TermName("bar")
    member: universe.TermName = bar

    scala> val q"foo.$name" = selected
    name: universe.TermName = bar

#### Super and This {:#super-this}

One can use this and super to select precise members within inheritance chain.

This tree supports following variations:

    scala> val q"$name.this" = q"this"
    name: universe.TypeName =

    scala> val q"$name.this" = q"foo.this"
    name: universe.TypeName = foov

So plain `q"this"` is equivalent to `q"${tpnme.EMPTY}.this"`. 

Similarly for super we have:

    scala> val q"$name.super[$qual].$field" = q"super.foo"
    name: universe.TypeName =
    qual: universe.TypeName =
    field: universe.Name = foo

    scala> val q"$name.super[$qual].$field" = q"super[T].foo"
    name: universe.TypeName =
    qual: universe.TypeName = T
    field: universe.Name = foo

    scala> val q"$name.super[$qual].$field" = q"other.super[T].foo"
    name: universe.TypeName = other
    qual: universe.TypeName = T
    field: universe.Name = foo

#### Application and Type Application {:#application}

#### Assign and Update {:#assign-update}

Assign and update are two related ways to explictly mutate a variable or collection:

    scala> val assign = q"x = 2"
    assign: universe.Tree = x = 2

    scala> val update = q"array(0) = 1"
    update: universe.Tree = array.update(0, 1)

As you can see update syntax is just a syntactic sugar that gets represented as update method call on given object.

Nevertheless quasiquotes let you deconstruct both of them uniformly according to their user-facing syntax:

    scala> List(assign, update).foreach {
             case q"$left = $right" =>
               println(s"left = $left, right = $right") 
           }
    left = x, right = 2
    left = array(0), right = 1

Where `array(0)` has the same AST as function application. 

On the other hand if you want to treat this two cases separately it's also possible with following more specific patterns:

    scala> List(assign, update) foreach { 
             case q"${ref: RefTree} = $expr" =>
               println(s"assign $expr to $ref")
             case q"$obj(..$args) = $expr" =>
               println(s"update $obj at $args with $expr") 
           }
    assign 2 to x
    update array at List(0) with 1


#### Return {:#return}

Return expressions is used to perform early return from a function. 

    scala> val ret = q"return 2 + 2"
    ret: universe.Return = return 2.$plus(2)

    scala> val q"return $expr" = ret 
    expr: universe.Tree = 2.$plus(2)

#### Throw {:#throw}

Throw expression is used to throw a throwable:

    scala> val thr = q"throw new Exception"
    thr: universe.Throw = throw new Exception()

    scala> val q"throw $expr" = thr 
    expr: universe.Tree = new Exception()

#### Ascription {:#ascription}

Ascriptions lets users to annotate type of intermidiate expression:

    scala> val ascribed = q"(1 + 1): Int"
    ascribed: universe.Typed = (1.$plus(1): Int)

    scala> val q"$expr: $tpt" = ascribed
    expr: universe.Tree = 1.$plus(1)
    tpt: universe.Tree = Int

#### Annotation {:#annotated-expr}

Expressions can be annotated:

    scala> val annotated = q"(1 + 1): @positive"
    annotated: reflect.runtime.universe.Annotated = 1.$plus(1): @positive

    scala> val q"$expr: @$annot" = annotated
    expr: reflect.runtime.universe.Tree = 1.$plus(1)
    annot: reflect.runtime.universe.Tree = positive

It's important to mention that such pattern won't match if we combine annotation with ascription:

    scala> val q"$expr: @$annot" = q"(1 + 1): Int @positive"
    scala.MatchError: (1.$plus(1): Int @positive) (of class scala.reflect.internal.Trees$Typed)
      ... 32 elided

In this case we need to deconstruct it as [ascription](#ascription) and then diconstruct `tpt` as [annotated type](#annotated-type).

#### Tuple {:#tuple-expr}

Tuples are heteregeneous data structures with built-in user-friendly syntax. The syntax itself is just a sugar that maps onto `scala.TupleN` calls:

    scala> val tup = q"(a, b)"
    tup: universe.Tree = scala.Tuple2(a, b)

At the moment tuples are only supported up to 22 arity but this is just an implementation restriction that might be lifted in the future. To find out if given arity is supported use:

    scala> val `tuple 10 supported?` = definitions.TupleClass(10) != NoSymbol
    tuple 10 supported?: Boolean = true

    scala> val `tuple 23 supported?` = definitions.TupleClass(23) != NoSymbol
    tuple 23 supported?: Boolean = false

Despited the fact that `Tuple1` class exists there is no built-in syntax for it. Single parens around expression do not change its meaning:
 
    scala> val inparens = q"(a)"
    inparens: universe.Ident = a

It is also common to treat `Unit` as nullary tuple:
   
    scala> val elems = List.empty[Tree]
    scala> val nullary = q"(..$elems)"
    nullary: universe.Tree = ()

Quasiquotes also support deconstruction of tuples of arbitrary arity:

    scala> val q"(..$elems)" = q"(a, b)"
    elems: List[universe.Tree] = List(a, b)   

This pattern also matches expressions as single-element tuples:

    scala> val q"(..$elems)" = q"(a)"
    elems: List[universe.Tree] = List(a)

And unit as nullary tuple:

    scala> val q"(..$elems)" = q"()"
    elems: List[universe.Tree] = List()

#### Block {:#block}

Blocks are a fundamental primitive to express sequence of actions or bindings. `q"..."` interpolator is an equivalent of a block. It allows to express more than one expression seperated by semicolon or a newline:

    scala> val t = q"a; b; c" 
    t: universe.Tree =
    {
      a;
      b;
      c
    } 

The only difference between `q"{...}"` and `q"..."` is handling of case when just a single element is present. `q"..."` always returns an element itself while a block still remains a block if a single element is not expression:

    scala> val t = q"val x = 2"
    t: universe.ValDef = val x = 2

    scala> val t = q"{ val x = 2 }"
    t: universe.Tree =
    {
      val x = 2;
      ()
    }

Blocks can also be flattened into another blocks with `..$`:

    scala> val ab = q"a; b"
    ab: universe.Tree =
    {
      a;
      b
    }

    scala> val abc = q"..$ab; c"
    abc: universe.Tree =
    {
      a;
      b;
      c
    }

The same syntax can be used to deconstruct blocks:

    scala> val q"..$stats" = q"a; b; c"
    stats: List[universe.Tree] = List(a, b, c)

Deconstruction always returns just user-defined contents of a block:

    scala> val q"..$stats" = q"{ val x = 2 }"
    stats: List[universe.Tree] = List(val x = 2)

Due to automatic flattening of single-element blocks with expressions, expressions themselves are considered to be single-element blocks:

    scala> val q"..$stats" = q"foo"
    stats: List[universe.Tree] = List(foo)

Except for empty tree which is not considered to be a block:

    scala> val q"..$stats" = q""
    scala.MatchError: <empty> (of class scala.reflect.internal.Trees$EmptyTree$)
      ... 32 elided

Zero-element block is equivalent to synthetic unit (one that was inserted by the compiler rather than written by the user): 

    scala> val q"..$stats" = q"{}"
    stats: List[universe.Tree] = List()

    scala> val syntheticUnit = q"..$stats"
    syntheticUnit: universe.Tree = ()

Such units are used in empty else branches of [ifs](#if) and empty bodies of [case clauses](#match) making it convenient to work with those cases as with zero-element blocks.

#### If {:#if}

There are two varieties of if expressions: those with else clause and without it:

    scala> val q"if ($cond) $thenp else $elsep" = q"if (true) a else b"
    cond: universe.Tree = true
    thenp: universe.Tree = a
    elsep: universe.Tree = b

    scala> val q"if ($cond) $thenp else $elsep" = q"if (true) a"
    cond: universe.Tree = true
    thenp: universe.Tree = a
    elsep: universe.Tree = ()

No-else clause is equivalent to else clause that contains a synthetic unit literal ([empty block](#block)). 

#### Pattern Match {:#match}

Pattern matching is cornerstone feature of Scala that lets you deconstruct values into their components:
    
    q"$expr match { case ..$cases } "

Where `expr` is some non-empty expression and each case is represented with a `cq"..."` quote:

    cq"$pat if $expr => $expr" 

Combination of the two forms allows to construct and deconstruct arbitrary pattern matches:

    scala> val q"$expr match { case ..$cases }" = 
               q"foo match { case _: Foo => 'foo case _ => 'notfoo }"
    expr: universe.Tree = foo
    cases: List[universe.CaseDef] = List(case (_: Foo) => scala.Symbol("foo"), case _ => scala.Symbol("notfoo"))

    scala> val cq"$pat1 => $body1" :: cq"$pat2 => $body2" :: Nil = cases
    pat1: universe.Tree = (_: Foo)
    body1: universe.Tree = scala.Symbol("foo")
    pat2: universe.Tree = _
    body2: universe.Tree = scala.Symbol("notfoo")

Case clause without body is equivalent to one holding synthetic unit literal ([empty block](#block)):

    scala> val cq"$pat if $expr1 => $expr2" = cq"_ =>"
    pat: universe.Tree = _
    expr1: universe.Tree = <empty>
    expr2: universe.Tree = ()

No-guard is represented with the help of [empty expression](#empty-expr).

#### Try {:#try}

Try expression is used to handle possible error conditions and ensure consistent state via finally. Both error handling cases and finally clause are optional.

    scala> val q"try $a catch { case ..$b } finally $c" = q"try t"
    a: universe.Tree = t
    b: List[universe.CaseDef] = List()
    c: universe.Tree = <empty>

    scala> val q"try $a catch { case ..$b } finally $c" = 
               q"try t catch { case _: C => }"
    a: universe.Tree = t
    b: List[universe.CaseDef] = List(case (_: C) => ())
    c: universe.Tree = <empty>

    scala> val q"try $a catch { case ..$b } finally $c" = 
               q"try t finally f"
    a: universe.Tree = t
    b: List[universe.CaseDef] = List()
    c: universe.Tree = f

Similarly to [pattern matching](#match) cases can be further deconstructed with `cq"..."`. No-finally clause is represented with the help of [empty expression](#empty-expr). 

#### Function {:#function-expr}

There are three ways to create anonymous function:

    scala> val f1 = q"_ + 1"
    anon1: universe.Function = ((x$4) => x$4.$plus(1))

    scala> val f2 = q"(a => a + 1)"
    anon2: universe.Function = ((a) => a.$plus(1))

    scala> val f3 = q"(a: Int) => a + 1"
    anon3: universe.Function = ((a: Int) => a.$plus(1))

First one uses placeholder syntax. Second one names function parameter but still relies
on type inference to infer its type. Last one explicitly defines function parameter. Due
to implementation restriction second notation can only be used in parenthesis or inside other
expression. If you leave them out you have to specify parameter types.

Parameters are represented as [Vals](#val-var). If you want to programmatically create val that should have 
its type inferred you need to use [empty type](#empty-type):

    scala> val tpt = tq""
    tpt: universe.TypeTree = <type ?>

    scala> val param = q"val x: $tpt"
    param: universe.ValDef = val x

    scala> val fun = q"($param => x)"
    fun: universe.Function = ((x) => x)

All of the given forms are represented in the same way and could be uniformly matched upon:

    scala> List(f1, f2, f3).foreach { 
             case q"(..$params) => $body" => 
               println(s"params = $params, body = $body") 
           }
    params = List(<synthetic> val x$5 = _), body = x$5.$plus(1)
    params = List(val a = _), body = a.$plus(1)
    params = List(val a: Int = _), body = a.$plus(1)

You can also tear arguments further apart:

    scala> val q"(..$params) => $_" = f3
    params: List[universe.ValDef] = List(val a: Int = _)

    scala> val List(q"$_ val $name: $tpt") = params
    name: universe.TermName = a
    tpt: universe.Tree = Int

It's recommended to use underscore pattern in place of [modifiers](#modifiers) even if you don't plan to work 
with them as parameters may contains additional flags which might cause match errors.

#### Partial Function {:#partial-function}

Partial functions are a neat syntax that lets you express functions with
limited domain with the help of pattern matching:

    scala> val pf = q"{ case i: Int if i > 0 => i * i }"
    pf: universe.Match =
    <empty> match {
      case (i @ (_: Int)) if i.$greater(0) => i.$times(i)
    }

    scala> val q"{ case ..$cases }" = pf
    cases: List[universe.CaseDef] = List(case (i @ (_: Int)) if i.$greater(0) => i.$times(i))

Weird default pretty printed view on the tree represents the fact that they share similar data structure as
trees for match expressions. Despite this fact they do not match one another:

  scala> val q"$expr match { case ..$cases }" = pf
  scala.MatchError: ...

#### While and Do-While Loops {:#while}

While and do-while loops are low-level control structures that used when performance of iteration
is critical:

    scala> val `while` = q"while(x > 0) x -= 1"
    while: universe.LabelDef =
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
    cond: universe.Tree = x.$greater(0)
    body: universe.Tree = x.$minus$eq(1)

    scala> val `do-while` = q"do x -= 1 while (x > 0)"
    do-while: universe.LabelDef =
    doWhile$2(){
      x.$minus$eq(1);
      if (x.$greater(0))
        doWhile$2()
      else
        ()
    }

    scala> val q"do $body while($cond)" = `do-while`
    body: universe.Tree = x.$minus$eq(1)
    cond: universe.Tree = x.$greater(0)

#### For and For-Yield Loops {:#for}

For and For-Yield expressions allow to write monadic style comprehensions that desugar into calls to `map`, `flatMap`, `foreach` and `withFilter` methods:

    scala> val `for-yield` = q"for (x <- xs; if x > 0; y = x * 2) yield x"
    for-yield: universe.Tree =
    xs.withFilter(((x) => x.$greater(0))).map(((x) => {
      val y = x.$times(2);
      scala.Tuple2(x, y)
    })).map(((x$3) => x$3: @scala.unchecked match {
      case scala.Tuple2((x @ _), (y @ _)) => x
    }))

Each enumerator in the comprehension can be expressed with `fq"..."` interpolator:
 
    scala> val enums = List(fq"x <- xs", fq"if x > 0", fq"y = x * 2")
    enums: List[universe.Tree] = List(`<-`((x @ _), xs), `if`(x.$greater(0)), (y @ _) = x.$times(2))

    scala> val `for-yield` = q"for (..$enums) yield y"
    for-yield: universe.Tree 

Simiarly one can deconstruct for-yield back into a list of enumerators and body:

    scala> val q"for (..$enums) yield $body" = `for-yield`
    enums: List[universe.Tree] = List(`<-`((x @ _), xs), `if`(x.$greater(0)), (y @ _) = x.$times(2))
    body: universe.Tree = x

It's important to mention that For and For-Yield do not cross-match each other:

    scala> val q"for (..$enums) $body" = `for-yield`
    scala.MatchError: ...

#### New {:#new}

New expression lets you construct an instance of given type possibly refining it with other types or definitions:

    scala> val q"new ..$parents { ..$body }" = q"new Foo(1) with Bar { def baz = 2 }"
    parents: List[universe.Tree] = List(Foo(1), Bar)
    body: List[universe.Tree] = List(def baz = 2)

See [templates](#templates) section for details. 

### Types

#### Empty Type {:#empty-type}

Empty type (`tq""`) is a canonical way to say that type at given location isn't given by the user and should be inferred by the compiler:

1. [Def](#method) with unknown return type
2. [Val or Var](#val-var) with unknown type
3. [Anonymous function](#function-expr) with unknown argument type

#### Type Identifier {:#type-ident}

Similarly to [term identifiers](#term-ref) one can construct a type identifier based on a name:

    scala> val name = TypeName("Foo")
    name: universe.TypeName = Foo

    scala> val foo = tq"$name"
    foo: universe.Ident = Foo

And deconstruct it back through [unlifting](#unlifting):

    scala> val tq"${name: TypeName}" = tq"Foo"
    name: universe.TypeName = Foo

It's recommended to always ascribe name as `TypeName` when you work with type identifiers. Non-ascribed pattern is equivalent to just a pattern variable binding.

#### Singleton Type {:#singleton-type}

A singleton type is a way to express a type of a term definition that is being referenced:

    scala> val singleton = tq"foo.bar.type".sr
    singleton: String = SingletonTypeTree(Select(Ident(TermName("foo")), TermName("bar")))

    scala> val tq"$ref.type" = tq"foo.bar.type"
    ref: universe.Tree = foo.bar

#### Type Projection {:#type-projection}

Type projection is a fundamental way to select types as members of other types:

    scala> val proj = tq"Foo#Bar"
    proj: universe.SelectFromTypeTree = Foo#Bar

    scala> val tq"$foo#$bar" = proj
    foo: universe.Tree = Foo
    bar: universe.TypeName = Bar

Similarly to identifiers it\'s recommended to always ascribe name as `TypeName`. Non-ascribed matching behaviour might change in the future.

As a convenience one can also select type members of terms:

    scala> val int = tq"scala.Int"
    int: universe.Select = scala.Int

    scala> val tq"scala.$name" = int
    name: universe.TypeName = Int

But semantically such selections are just a shortcut for a combination of singleton types and type projections:

    scala> val projected = tq"scala.type#Int"
    projected: universe.SelectFromTypeTree = scala.type#Int

Lastly [similarly to expressions](#super-this) one can select members through super and this:

    scala> val superbar = tq"super.Bar"
    superbar: universe.Select = super.Bar

    scala> val tq"$pre.super[$parent].$field" = superbar 
    pre: universe.TypeName =
    parent: universe.TypeName =
    field: universe.Name = Bar

    scala> val thisfoo = tq"this.Foo"
    thisfoo: universe.Select = this.Foo

    scala> val tq"this.${tpname: TypeName}" = thisfoo
    tpname: universe.TypeName = Foo

#### Applied Type {:#applied-type}

Instantiations of parametized types can be expressed with the help of applied types (type-level equivalent of type application):

    scala> val applied = tq"Foo[A, B]"
    applied: universe.Tree = Foo[A, B]

    scala> val tq"Foo[..$targs]" = applied
    targs: List[universe.Tree] = List(A, B)

Deconstruction of non-applied types will cause `targs` begin extracted as empty list:

    scala> val tq"Foo[..$targs]" = tq"Foo"
    targs: List[universe.Tree] = List()

#### Annotated Type {:#annotated-type}

Similarly to expressions types can be annotated:

    scala> val annotated = tq"T @Fooable"
    annotated: reflect.runtime.universe.Annotated = T @Fooable

    scala> val tq"$tpt @$annot" = annotated
    tpt: reflect.runtime.universe.Tree = T
    annot: reflect.runtime.universe.Tree = Fooable

#### Compound Type {:#compound-type}

Compound type lets users to express a combination of a number of types with optional refined member list:

    scala> val compound = tq"A with B with C"
    compound: universe.CompoundTypeTree = A with B with C

    scala> val tq"..$parents { }" = compound
    parents: List[universe.Tree] = List(A, B, C)
    defns: List[universe.Tree] = List()

Braces after parents are required to signal that this type is a compound type even if there are no refinements and we just want to extract a sequence of types combined with `with` keyword.

On the other side of the spectrum are pure refinements without explicit parents (a.k.a. structural types):

    scala> val structural = tq"{ val x: Int; val y: Int }"
    structural: universe.CompoundTypeTree =
    scala.AnyRef {
      val x: Int;
      val y: Int
    }

    scala> val tq"{ ..$defns }" = structural
    defns: List[universe.Tree] = List(val x: Int, val y: Int)

Here we can see that AnyRef is a parent that is inserted implicitly if we don't provide any.

#### Existential Type {:#existential-type}

#### Tuple Type {:#tuple-type}

[Similarly to expressions](#tuple-expr), tuple types are just a syntactic sugar over `TupleN` classes:

    scala> val tup2 = tq"(A, B)"
    tup2: universe.Tree = scala.Tuple2[A, B]

    scala> val tq"(..$tpts)" = tup2
    tpts: List[universe.Tree] = List(A, B)

Analagously `Unit` type is considered to be nullary tuple:

    scala> val tq"(..$tpts)" = tq"_root_.scala.Unit"
    tpts: List[universe.Tree] = List()

It's important to mention that pattern matching of reference to `Unit` is limited to either fully qualified path or a reference that contains symbols. (see [referential transparency](#referential-transparency))

#### Function Type {:#function-type}

Similarly to tuples, function types are a syntactic sugar over `FunctionN` classes:

    scala> val funtype = tq"(A, B) => C"
    funtype: universe.Tree = _root_.scala.Function2[A, B, C]

    scala> val tq"..$foo => $bar" = funtype
    foo: List[universe.Tree] = List(A, B)
    bar: universe.Tree = C

### Patterns

#### Wildcard Pattern {:#wildcard-pattern}

Wildcard pattern (`pq"_"`) is the simplest form of pattern that matches any input.

#### Binding Pattern {:#binding-pattern}

Binding pattern is a way to name pattern or one it's part as local variable:

    scala> val bindtup = pq"foo @ (1, 2)"
    bindtup: universe.Bind = (foo @ scala.Tuple2(1, 2))

    scala> val pq"$name @ $pat" = bindtup
    name: universe.Name = foo
    pat: universe.Tree = scala.Tuple2(1, 2)

Binding without explicit pattern is equivalent to the one with wildcard pattern:

    scala> val pq"$name @ $pat" = pq"foo"
    name: universe.Name = foo
    pat: universe.Tree = _

See also [type pattern](#type-pattern) for an example of type variable binding. 

#### Extractor Pattern {:#extractor-pattern}

Extractors are a neat way to delegate a pattern matching to another object's unapply method:

    scala> val extractor = pq"Foo(1, 2, 3)"
    extractor: universe.Tree = Foo(1, 2, 3)

    scala> val pq"$id(..$pats)" = extractor
    id: universe.Tree = Foo
    pats: List[universe.Tree] = List(1, 2, 3)

#### Type Pattern {:#type-pattern}

Type patterns are a way to check type of a scrutinee:

    scala> val isT = pq"_: T"
    isT: universe.Typed = (_: T)

    scala> val pq"_: $tpt" = isT
    tpt: universe.Tree = T

Combination of non-wildcard name and type pattern is represented as bind over wildcard type pattern:

    scala> val fooIsT = pq"foo: T"
    fooIsT: universe.Bind = (foo @ (_: T))

    scala> val pq"$name @ (_: $tpt)" = fooIsT
    name: universe.Name = foo
    tpt: universe.Tree = T

Another important thing to mention is a type variable patterns:

    scala> val typevar = pq"_: F[t]"
    typevar: universe.Typed = (_: F[(t @ <empty>)])

One can construct (and similarly deconstruct) such patterns by following steps:

    scala> val name = TypeName("t")
    scala> val empty = q""
    scala> val t = pq"$name @ $empty"
    scala> val tpt = tq"F[$t]"
    scala> val typevar = pq"_: $tpt"
    typevar: universe.Typed = (_: F[(t @ _)])

#### Alternative Pattern {:#alternative-pattern}

Pattern alternatives represent a pattern that matches whenever at least one of the branches matches:

    scala> val alt = pq"Foo() | Bar() | Baz()"
    alt: universe.Alternative = (Foo()| Bar()| Baz())

    scala> val pq"$first | ..$rest" = alt
    head: universe.Tree = Foo()
    tail: List[universe.Tree] = List(Bar(), Baz())

    scala> val pq"..$init | $last" = alt
    init: List[universe.Tree] = List(Foo(), Bar())
    last: universe.Tree = Baz()

#### Tuple Pattern {:#tuple-pattern}

Similarly to [tuple expressions](#tuple-type) and [tuple types](#tuple-type), tuple patterns are just a syntactic sugar that expands as `TupleN` extractor:

    scala> val tup2pat = pq"(a, b)"
    tup2pat: universe.Tree = scala.Tuple2((a @ _), (b @ _))

    scala> val pq"(..$pats)" = tup2pat
    pats: List[universe.Tree] = List((a @ _), (b @ _))

### Definitions

#### Modifiers {:#modifiers}

Every definition except imports and package objects have associtated modifiers object which contains following data:

1. `FlagSet`, a set of bits that charactarizes given definition.
2. Private within name (e.g. `foo` in `private[foo] def f`)
3. List of annotations

Quasiquotes let you easily work with those fields through native support for `Modifiers`, `FlagSet` and annotation unquoting.

    scala> val f1 = q"${Modifiers(PRIVATE | IMPLICIT)} def f"
    f1: universe.DefDef = implicit private def f: scala.Unit

    scala> val f2 = q"$PRIVATE $IMPLICIT def f"
    f2: universe.DefDef = implicit private def f: scala.Unit

    scala> val f3 = q"private implicit def f"
    f3: universe.DefDef = implicit private def f: scala.Unit

All of those quasiquotes result into equivalent trees. It's also possible to combine unquoted flags with one provided inline in the source code but unquoted one should be given earlier:

    scala> q"$PRIVATE implicit def foo"
    res10: universe.DefDef = implicit private def foo: scala.Unit

    scala> q"implicit $PRIVATE def foo"
    <console>:32: error: expected start of definition
                  q"implicit $PRIVATE def foo"
                             ^

To provide an annotation one need to unquote a new-shaped tree:

    scala> val annot = q"new foo(1)"
    Foo: universe.Tree = new Foo(1)

    scala> val f4 = q"@$annot def f"
    res9: universe.DefDef = @new foo(1) def f: scala.Unit

    scala> val f5 = q"@foo(1) def f"
    res8: universe.DefDef = @new foo(1) def f: scala.Unit

In deconstruction one can either extract `Modifiers` or annotations, but you can't extract flags separatly:

    scala> val q"$mods def f" = q"@foo implicit def f"
    mods: universe.Modifiers = Modifiers(<deferred> implicit, , Map())

    scala> val q"@..$annots implicit def f" = q"@foo @bar implicit def f"
    annots: List[universe.Tree] = List(new foo(), new bar())

Considering the fact that definitions might contain various low-level flags added to trees during typechecking it\'s recommended to always extract complete modifiers as otherwise your pattern might not be exhaustive. If you don't care about them just use a wildcard:

    scala> val q"$_ def f" = q"@foo @bar implicit def f"

#### Templates {:#templates}

Templates are a common abstraction in definition trees that is used in new expressions, classes, traits, objects, package objects. Although there is no interpolator for it at the moment we can illustrate its structure on the example of new expression (similar handling will applly to all other template-bearing trees):

    q"new { ..$early } with ..$parents { $self => ..$stats }"

So template consists of:

1. Early definitions. A list of val or type definitions. Type definitions are still allowed by they are deprecated and will be removed in the future:

        scala> val withx = q"new { val x = 1 } with RequiresX"
        withx: universe.Tree = ...

        scala> val q"new { ..$early } with RequiresX" = withx
        early: List[universe.Tree] = List(val x = 1)
 
2. List of parents. A list of type identifiers with possibly an optional arguments to the first one in the list:

        scala> val q"new $parent[..$tpts](...$exprss) with ..$rest"  = q"new Foo(1) with Bar[T]"
        parent: universe.Tree = Foo
        tpts: List[universe.Tree] = List()
        exprss: List[List[universe.Tree]] = List(List(1))
        rest: List[universe.Tree] = List(Bar[T])

        scala> val List(tq"Bar[T]") = rest

3. Self type definition. A val definition that can be used to define an alias to this and provide a self-type via tpt:

        scala> val q"new { $self => }" = q"new { self: T => }"
        self: universe.ValDef = private val self: T = _

        scala> val q"$mods val $name: $tpt" = self
        mods: universe.Modifiers = Modifiers(private, , Map())
        name: universe.TermName = self
        tpt: universe.Tree = T

4. List of body statements.

        scala> val q"new { ..$body }" = q"new { val x = 1; def y = 'y }"
        body: List[universe.Tree] = List(val x = 1, def y = scala.Symbol("y"))

#### Val and Var Definitions {:#val-var}

Vals and vars allow you to define immutable and mutable variables correspondingly. Additionally they are also used to represent [function](#function-expr), [class](#class) and [method](#method) paremeters.

Each val and var consistents of four components: modifiers, name, type tree and a right hand side:

    scala> val valx = q"val x = 2"
    valx: universe.ValDef = val x = 2

    scala> val q"$mods val $name: $tpt = $rhs" = valx
    mods: universe.Modifiers = Modifiers(, , Map())
    name: universe.TermName = x
    tpt: universe.Tree = <type ?>
    rhs: universe.Tree = 2

If type of the val isn't explicitly specified by the user an [empty type](#empty-type) is used as tpt. 

Vals and vars are disjoint (they don't match one another):

    scala> val q"$mods val $name: $tpt = $rhs" = q"var x = 2"
    scala.MatchError: var x = 2 (of class scala.reflect.internal.Trees$ValDef)
      ... 32 elided

Vars always have `MUTABLE` flag in their modifiers:

    scala> val q"$mods var $name: $tpt = $rhs" = q"var x = 2"
    mods: universe.Modifiers = Modifiers(<mutable>, , Map())
    name: universe.TermName = x
    tpt: universe.Tree = <type ?>
    rhs: universe.Tree = 2

#### Pattern Definitions {:#pattern-def}

Pattern definitions allow to use scala pattern matching capabilities to define variables. Unlike
val and var definitions, pattern definitions are not first-class and they are get represented 
through combination of regular vals and vars and pattern matching:

    scala> val patdef = q"val (x, y) = (1, 2)"
    patdef: universe.Tree =
    {
      <synthetic> <artifact> private[this] val x$2 = scala.Tuple2(1, 2): @scala.unchecked match {
        case scala.Tuple2((x @ _), (y @ _)) => scala.Tuple2(x, y)
      };
      val x = x$2._1;
      val y = x$2._2;
      ()
    }

This representation has a few side-effects on the usage of such definitions:

1. Due to the fact that single definition often gets desugared into multiple lower-level
   ones, one need to always use unquote splicing to unquote pattern definitions into other trees:

        scala> val tupsum = q"..$patdef; a + b"
        tupsum: universe.Tree =
        {
          <synthetic> <artifact> private[this] val x$3 = scala.Tuple2(1, 2): @scala.unchecked match {
            case scala.Tuple2((x @ _), (y @ _)) => scala.Tuple2(x, y)
          };
          val x = x$3._1;
          val y = x$3._2;
          a.$plus(b)
        }

   Otherwise if a regular unquoting is used, the definitions will be nested in a block that will make 
   them invisible in the scope where they are meant to be used:

        scala> val wrongtupsum = q"$patdef; a + b"
        wrongtupsum: universe.Tree =
        {
          {
            <synthetic> <artifact> private[this] val x$3 = scala.Tuple2(1, 2): @scala.unchecked match {
              case scala.Tuple2((x @ _), (y @ _)) => scala.Tuple2(x, y)
            };
            val x = x$3._1;
            val y = x$3._2;
            ()
          };
          a.$plus(b)
        }

2. One can only construct pattern definitions, not deconstruct them. 

Generic form of pattern definition consists of modifiers, pattern, ascribed type and a right hand side:

    q"$mods val $pat: $tpt = $rhs"

Simiarly one can also construct a mutable pattern definition:

    q"$mods var $pat: $tpt = $rhs"

#### Method Definition {:#method}

#### Type Definition {:#type-def}

#### Class Definition {:#class}

#### Trait Definition {:#trait}

#### Object Definition {:#object}

#### Package Definition {:#package}

Packages are a fundamental primitive to organize source code. You can express them in quasiquotes as:

    scala> val pack = q"package mycorp.myproj { class MyClass }"
    pack: universe.PackageDef =
    package mycorp.myproj {
      class MyClass extends scala.AnyRef {
        def <init>() = {
          super.<init>();
          ()
        }
      }
    }

    scala> val q"package $ref { ..$body }" = pack
    ref: universe.RefTree = mycorp.myproj
    body: List[universe.Tree] =
    List(class MyClass extends scala.AnyRef {
      def <init>() = {
        super.<init>();
        ()
      }
    })

Quasiquotes don\'t support inline package definition syntax that are usually used in the
header of the source file (but it's equivalent to the supported one in terms of ASTs).

#### Package Object Definition{:#package-object}

### Auxiliary

#### Import {:#import}

Import trees consist of reference and a list of selectors:

    scala> val q"import $ref.{..$sels}" = q"import foo.{bar, baz => boo, poision => _, _}"
    ref: universe.Tree = foo
    sels: List[universe.Tree] = List((bar @ _), $minus$greater((baz @ _), (boo @ _)), $minus$greater((poision @ _), _), _)

Selectors are extracted as pattern trees which are syntactically similar to selectors:

1. Simple identifier selectors are represented as pattern bindings: `pq"bar"`
2. Renaming selectors are represented as thin arrow patterns: `pq"baz -> boo"`
3. Unimport selectors are represented as thin arrows with wildcard right hand side: `pq"poision -> _"`
4. Wildcard selector is represented as wildcard pattern: `pq"_"`

Similarly one construct imports back from a programmatically created list of selectors:

    scala> val ref = q"a.b"
    scala> val sels = List(pq"foo -> _", pq"_")
    scala> val imp = q"import $ref.{..$sels}"
    imp: universe.Import = import a.b.{foo=>_, _}

## Terminology summary {:#terminology}

* **Quasiquote** (not quasi-quote) can refer to either quasiquote library or any usage of one it's [interpolators](#interpolators). The name is not hyphenated for sake of consistency with implementations of the same concept in other languages (e.g. [Scheme and Racket](http://docs.racket-lang.org/reference/quasiquote.html), [Haskell](http://www.haskell.org/haskellwiki/Quasiquotation))
* **Tree** or **AST** (Abstract Syntax Tree) is representation of Scala program or a part of it through means of Scala reflection API's Tree type.
* **Tree construction** refers to usages of quasiquotes as expressions to represent creation of new tree values.
* **Tree deconstruction** refers to usages of quasiquotes as patterns to structurally tear trees apart.
* **Unquoting** is a way of either putting thing in or extracting things out of quasiquote. Can be performed with `$` syntax within a quasiquote.
* **Unquote splicing** (or just splicing) is another form of unquoting that flattens contents of the unquotee into a tree. Can be performed with either `..$` or `...$` syntax.
* **Rank** is a degree of flattenning of unquotee: `rank($) == 0`, `rank(..$) == 1`, `rank(...$) == 2`. 
* [**Lifting**](#lifting) is a way to unquote non-tree values and transform them into trees with the help of Liftable typeclass.
* [**Unlifting**](#unlifting) is a way to unquote non-tree values out of quasiquote patterns with the help of Unliftable typeclass. 

## Future prospects {:#future}

* Referential transparency: [SI-7823](https://issues.scala-lang.org/browse/SI-7823)
* Alternative to Scheme's ellipsis: [SI-8164](https://issues.scala-lang.org/browse/SI-8164)
* Safety by construction 
