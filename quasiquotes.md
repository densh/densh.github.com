---
layout: default
---

# Quasiquotes guide

## Intro
## Cardinality
## Lifting and Unlifting
## Syntax summary
### Expressions


 Tree             | Quasi-quote                                                 
------------------|------------------------------------------------------------
 If               | `q"if ($cond) $thenp else $elsep"`                             
 Application      | `q"$f(...$args)"`                                          
 Type Application | `q"$f[..$targs]"`                                          
 Selection        | `q"$obj.$field"`                                              
 Block            | `q"{ ..$block }"`                                          
 New              | `q"new { ..$early } with ..$parents { $self => ..$body }"` 
 Assign           | `q"$lhs = $rhs"`                                           
 Update           | `q"$lhs(..$args) = $rhs"`                                  
 Pattern Match    | `q"$scrutinee match { case ..$cases }"`                    
 Return           | `q"return $expr"`                                          
 For Loop         | `q"for (..$enumerators) $body"`                            
 For-Yield Loop   | `q"for (..$enumerators) yield $body"`                      
 While Loop       | `q"while ($cond) $body"`                                   
 Do While Loop    | `q"do $expr while ($cond)"`                                
 Try              | `q"try $expr catch { case ..$cases } finally $expr"`       
 Ascription       | `q"$expr: $tpt"`                                           
 Constant         | `q"$value"` where `value: AnyVal` or `value: String` or values like `q"1.0"`     
 Identifier       | `q"$name"` where `name: TermName` or `q"Name"`             
 Tuple            | `q"(..$elements)"`
 Function         | `q"(..$args) => $expr"`
 Throw            | `q"throw $expr"`
 This             | `q"this"`

### Types

 Tree             | Quasi-quote                               
------------------|-------------------------------------------
 Identifier       | `tq"$name"` where `name: TypeName` or `tq"Name"`
 Applied Type     | `tq"$name[..$targs]"` 
 Tuple Type       | `tq"(..$elements)"`
 Function Type    | `tq"(..$argtpes) => $restpe"`
 Existential Type | `tq"$tpt forSome { ..$definitions }"` 
 Type Selection   | `tq"$tpt#$name"`
 Dependent Type   | `tq"$prefix.$name"`
 Refined Type (!) | `tq"..$parents { ..$definitions }"`

### Patterns
 
 Tree             | Quasi-quote                                
------------------|-------------------------------------------
 Binding          | `pq"$name @ $pat"` where `name: TermName` 
 Extractor Call   | `pq"$extractor(..$subpatterns)"`          
 Type Pattern     | `pq"$name: $tpt"`                         
 
### Definitions

 Tree           | Quasi-quote                                                        
----------------|------------------------------------------------------------------
 Def            | `q"$mods def $name[..$targs](...$argss): $restpe = $body"`
 Val            | `q"$mods val $name: $tpt = $rhs"` or `q"$mods val $pattern = $rhs"`                      
 Var            | `q"$mods var $name: $tpt = $rhs"` or `q"$mods val $pattern = $rhs"`                       
 Class          | `q"$mods class $name[..$targs] $ctorMods(...$argss) extends { ..$early } with ..$parents { $self => ..$body }"` 
 Trait          | `q"$mods trait $name[..$targs] extends { ..$early } with ..$parents { $self => ..$body }"` 
 Object         | `q"package object $name extends { ..$early } with ..$parents { $self => ..$body }"`
 Package        | `q"package $name { ..$body }"`
 Package Object | `q"package object $name extends { ..$early } with ..$parents { $self => ..$body }"`
 Import (!)     | `q"import $selector"`
 
