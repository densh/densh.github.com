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

* `name`: variable of type `TermName` or `TypeName`
* `tname`: variable of type `TermName`
* `tpname`: variable of type `TypeName`
* `expr`: variable of type `Tree` that contains expression
* `ident`: variable of type `Tree` that contains identifier
* `tpt`: variable of type `Tree` that contains representation of a type
* `pat`: varible of type `Tree` that contains pattern
* `args`: variable of type `List[Tree]` where each element is a parameter
* `argss`: variable of type `List[List[Tree]]` where each element is a parameter
* `targs`: variable of type `List[Tree]` where each element is a type argument
* `enums`: variable of type `List[Tree]` where each element is a for loop enumerator
* `early`: variable of type `List[Tree]` where each element is early definition
* `parents`: variable of type `List[Tree]` where each element is a parent
* `self`: variable of type `Tree` that corresponds to self type definition
* `stats`: variable of type `List[Tree]` where each element is either an expression or definition
* `topstats`: variable of type `List[Tree]` where each element is top-level definition

### Expressions

 Tree             | Quasi-quote                                                 | Type
------------------|-------------------------------------------------------------|-------------------------
 Empty            | `q""`                                                       | EmptyTree
 Identifier       | `q"$tname"` or `q"Name"`                                    | Ident
 Constant         | `q"$value"`                                                 | Literal
 This             | `q"this"`                                                   | This
 Application      | `q"$expr(...$argss)"`                                       | Apply
 Type Application | `q"$expr[..$targs]"`                                        | TypeApply
 Selection        | `q"$expr.$name"`                                            | Select
 Assign           | `q"$ident = $expr"`                                         | Assign, AssignOrNamedArg
 Update           | `q"$ident(..$exprs) = $expr"`                               | Tree
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
 Applied Type     | `tq"$ident[..$tpts]"`                 | AppliedTypeTree
 Tuple Type       | `tq"(..$tpts)"`                       | Tree
 Function Type    | `tq"(..$tpts) => $tpt"`               | Tree
 Existential Type | `tq"$tpt forSome { ..$defns }"`       | ExistentialTypeTree
 Type Selection   | `tq"$tpt#$name"`                      | SelectFromTypeTree
 Dependent Type   | `tq"$ref.$name"`                      | Select
 Refined Type (!) | `tq"..$parents { ..$definitions }"`   | CompoundTypeTree
 Singleton Type   | `tq"$ref.type"`                       | SingletonType

### Patterns
 
 Tree             | Quasi-quote           | Type                    
------------------|-----------------------|-------------------
 Binding          | `pq"$name @ $pat"`    | Bind
 Extractor Call   | `pq"$ident(..$pats)"` | Apply, UnApply   
 Type Pattern     | `pq"$name: $tpt"`     | Typed             
 
### Definitions

 Tree           | Quasi-quote                                                                                                        | Type 
----------------|--------------------------------------------------------------------------------------------------------------------|-----------
 Def            | `q"$mods def $tname[..$targs](...$argss): $tpt = $expr"`                                                           | DefDef
 Val            | `q"$mods val $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                 | ValDef
 Var            | `q"$mods var $tname: $tpt = $expr"` or `q"$mods val $pat = $expr"`                                                 | ValDef
 Type           | `q"$mods type $tpname[..$targs] = $tpt"`                                                                           | TypeDef
 Class          | `q"$mods class $tpname[..$targs] $ctorMods(...$argss) extends { ..$early } with ..$parents { $self => ..$stats }"` | ClassDef
 Trait          | `q"$mods trait $tpname[..$targs] extends { ..$early } with ..$parents { $self => ..$stats }"`                      | TraitDef
 Object         | `q"$mods object $name extends { ..$early } with ..$parents { $self => ..$body }"`                                  | ModuleDef
 Package        | `q"package $ref { ..$topstats }"`                                                                                  | PackageDef
 Package Object | `q"package object $tname extends { ..$early } with ..$parents { $self => ..$stats }"`                              | PackageDef
 Import (!)     | `q"import $selector"`                                                                                              | Import

## Syntax Details 

### Expressions

### Types

### Patterns

