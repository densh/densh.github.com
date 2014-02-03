---
layout: default
---

# Quasiquotes guide

## Intro
## Cardinality
## Lifting and Unlifting
## Syntax summary
### Expressions


 Tree             | Quasi-quote                                                | Comment  
------------------|------------------------------------------------------------|---------
 If               | `q"if ($cond) $thenp else $elsep"`                         | x     
 Application      | `q"$f(...$args)"`                                          | x
 Type Application | `q"$f[..$targs]"`                                          | x
 Selection        | `q"$obj.$field"`                                           | x    
 Block            | `q"{ ..$block }"`                                          | x
 New              | `q"new { ..$early } with ..$parents { $self => ..$body }"` | x
 Assign           | `q"$lhs = $rhs"`                                           | x
 Update           | `q"$lhs(..$args) = $rhs"`                                  | x
 Pattern Match    | `q"$scrutinee match { case ..$cases }"`                    | x
 Return           | `q"return $expr"`                                          | x
 For Loop         | `q"for (..$enumerators) $body"`                            | x
 For-Yield Loop   | `q"for (..$enumerators) yield $body"`                      | x
 While Loop       | `q"while ($cond) $body"`                                   | x
 Do While Loop    | `q"do $expr while ($cond)"`                                | x
 Try              | `q"try $expr catch { case ..$cases } finally $expr"`       | x
 Ascription       | `q"$expr: $tpt"`                                           | x
 Constant         | `q"$value"` where `value: AnyVal` or `value: String`       | x
 Identifier       | `q"$name"` where `name: TermName`                          | x 

### Types

 Tree             | Quasi-quote                               | Comment
------------------|-------------------------------------------|---------
 Identifier       | tq"$name" where `name: TypeName`          | x 
 x                | x                                         | x

### Patterns
 
 Tree             | Quasi-quote                               | Comment  
------------------|-------------------------------------------|---------
 Binding          | `pq"$name @ $pat"` where `name: TermName` | x
 Extractor Call   | `pq"$extractor(..$subpatterns)"`          | x
 Type Pattern     | `pq"$name: $tpt"`                         | x
 
### Definitions

 Tree           | Quasi-quote                                                      | Comment  
----------------|------------------------------------------------------------------|---------
 Def            | `q"$mods def $name[..$targs](...$argss): $restpe = $body"`       | x 
 Val            | `q"$mods val $name: $tpt = $rhs"`                                | x
 Var            | `q"$mods var $name: $tpt = $rhs"`                                | x
 Class          | `q"$mods class $name[..$targs] $ctorMods(...$argss) extends { ..$early } with ..$parents { $self => ..$body }"` | x
 Trait          | `q"$mods trait $name[..$targs] extends { ..$early } with ..$parents { $self => ..$body }"` | x
 Object         | `q"package object $name extends { ..$early } with ..$parents { $self => ..$body }"` | x
 Package        | `q"package $name { ..$body }"`                                   | x
 Package Object | `q"package object $name extends { ..$early } with ..$parents { $self => ..$body }"` | x
 Import         | `q"import $selector"`                                            | x
 
