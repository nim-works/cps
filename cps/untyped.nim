##[
  TEMPORARY SCAFFOLDING - Every type here is technical debt.

  These types exist because we haven't fully typed the CPS AST operations.
  Each represents a semantic category where we know WHAT something is,
  but not precisely enough to use a Tier 1 type (Statement, Expression, etc.)

  To add a new type:
    1. First, check if an existing type captures your semantic
    2. If not, define a new semantic alias for Untyped
    3. Document what would be needed to eliminate it

  Progress is measured by eliminating types from this file.
]##

type
  Untyped* = distinct NormNode
    ## The base compromise type. Direct use is code smell - always prefer
    ## a semantic alias below.

  # === Semantic Aliases ===
  # Each alias documents a specific "we don't know yet" situation.

  PragmaArg* = Untyped
    ## Argument to a pragma - could be Name, TypeExpr, Literal, Call.
    ## TODO: Analyze pragma usage patterns and create specific types.

  FilterResult* = Untyped
    ## Output of generic filter/tree-walk operations.
    ## TODO: Add typed overloads for specific transformations.

  BreakTarget* = Untyped
    ## Label from a break statement - Name or Empty.
    ## TODO: Create a Label type or use Name|Empty union.

  Annotation* = Untyped
    ## CPS annotation nodes (cpsPending, cpsBreak, etc.)
    ## TODO: Create specific annotation types.

  Normalized* = Untyped
    ## Output of normalizingRewrites - preserves input structure.
    ## TODO: Add typed overloads that return same type as input.

  ContinuationBody* = Untyped
    ## Body of a continuation leg or bootstrap.
    ## TODO: Should become Statement once body handling is typed.

  HookResult* = Untyped
    ## Output of hook() calls - varies by hook type.
    ## TODO: Create per-hook return types or typed overloads.

  EnvAccess* = Untyped
    ## Access to environment fields (dot expressions into env).
    ## TODO: Should become Expression or DotExpr.

  RecoverBody* = Untyped
    ## Body of recover proc generation.
    ## TODO: Should become Statement.

  TransformResult* = Untyped
    ## Output of CPS transformation passes.
    ## TODO: Should return same type as input or Statement.
