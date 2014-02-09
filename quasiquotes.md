---
layout: default
---

# Quasi-quotes guide

## Intro
## Cardinality
## Lifting and Unlifting
## Referential transparency
## Syntax summary

### Abbreviations

* `tname: TermName`
* `tpname: TypeName`
* `value: Byte|Short|Int|Long|Float|Double|Boolean|String|Unit`
* `expr: Tree` that contains expression
* `tpt: Tree` that contains representation of a type
* `pat: Tree` that contains pattern
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

 Tree             | Quasi-quote                                                 | Type
------------------|-------------------------------------------------------------|-------------------------
 Empty            | `q""`                                                       | EmptyTree
 Identifier       | `q"$tname"` or `q"Name"`                                    | Ident
 Constant         | `q"$value"`                                                 | Literal
 This             | `q"this"`                                                   | This
 Application      | `q"$expr(...$argss)"`                                       | Apply
 Type Application | `q"$expr[..$targs]"`                                        | TypeApply
 Selection        | `q"$expr.$tname"`                                           | Select
 Assign           | `q"$expr = $expr"`                                          | Assign, AssignOrNamedArg
 Update           | `q"$expr(..$exprs) = $expr"`                                | Tree
 Return           | `q"return $expr"`                                           | Return
 Throw            | `q"throw $expr"`                                            | Throw
 Ascription       | `q"$expr: $tpt"`                                            | Typed 
 Tuple            | `q"(..$exprs)"`                                             | Tree
 Function         | `q"(..$args) => $expr"`                                     | Function
 Block            | `q"{ ..$stats }"`                                           | Block
 If               | `q"if ($expr) $expr else $expr"`                            | If
 Pattern Match    | `q"$expr match { case ..$cases }"`                          | Match
 Try              | `q"try $expr catch { case ..$cases } finally $expr"`        | Try
 While Loop       | `q"while ($expr) $expr"`                                    | LabelDef 
 Do-While Loop    | `q"do $expr while ($expr)"`                                 | LabelDef
 For Loop         | `q"for (..$enums) $expr"`                                   | Tree
 For-Yield Loop   | `q"for (..$enums) yield $expr"`                             | Tree
 New              | `q"new { ..$early } with ..$parents { $self => ..$stats }"` | Tree

### Types

 Tree             | Quasi-quote                           | Type
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

### Patterns
 
 Tree             | Quasi-quote           | Type                    
------------------|-----------------------|-------------------
 Wildcard         | `pq"_"`               | Ident
 Binding          | `pq"$tname @ $pat"`   | Bind
 Extractor Call   | `pq"$ref(..$pats)"`   | Apply, UnApply   
 Tuple            | `pq"(..$pats)"`       | Apply, UnApply
 Type Pattern     | `pq"$tname: $tpt"`    | Typed             
 
### Definitions

 Tree           | Quasi-quote                                                                                                        | Type 
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

### Types

### Patterns

### Definitions
