# Candidates for NormNode → Specific Type Conversion

## Overview
This document identifies the 81 NormNode-returning functions and analyzes which could be converted to more specific AST types (Statement, Expression, Call, TypeExpr, Pragma, etc.).

## Analysis Strategy
For each candidate, determine:
1. What node kinds does it create/return?
2. What are the possible kind() values?
3. What specific type would be most appropriate?
4. Will converting break any downstream usage?

## High-Confidence Candidates (Likely Safe Conversions)

### returns.nim - Statement-Creating Functions
These all create statement-like nodes:

- **Line 23: makeReturn()** → `Statement`
  - Creates nnkReturnStmt nodes only
  - Used in statement contexts
  - Safe to convert

- **Line 37: makeReturn()** (overload) → `Statement`
  - Variant that creates return statements
  - Safe to convert

- **Line 61: terminator()** → `Statement`
  - Creates control flow nodes (loops, continues, breaks)
  - All outcomes are statements
  - Safe to convert

- **Line 90: tailCall()** → `Call`
  - Creates function call nodes in tail position
  - nnkCall or similar
  - Safe to convert

- **Line 103: jumperCall()** → `Call`
  - Creates jump call nodes
  - Always Call-like
  - Safe to convert

- **Line 5: firstReturn()** → `Statement`
  - Searches for first return statement
  - Returns Statement nodes
  - Currently blocked, but clear type

### spec.nim - Pragma & Helper Functions

- **Line 206: newCpsContinue()** → `Pragma`
  - Creates {.cpsContinue.} pragma
  - Always a pragma node
  - Safe to convert

- **Line 238: newCpsTerminate()** → `Pragma`
  - Creates {.cpsTerminate.} pragma
  - Always a pragma node
  - Safe to convert

- **Line 192: newCpsBreak()** → `Pragma`
  - Creates {.cpsBreak label.} pragma
  - Always a pragma node
  - Safe to convert

- **Line 214: breakLabel()** → `Name`
  - Extracts label from break pragma
  - Returns identifier/name
  - Safe to convert

- **Line 262: flattenStmtList()** → `Statement`
  - Flattens nested statement lists
  - Result is nnkStmtList
  - Safe to convert

- **Line 283: wrappedFinally()** → `Statement`
  - Creates try-finally structure
  - Always statement-like
  - Safe to convert

### ast.nim - Core AST Constructors

- **Line 439: newStmtList()** → `Statement`
  - Creates nnkStmtList specifically
  - Named function indicates intent
  - Safe to convert (but has issues with polymorphic add())

- **Line 313: newEmptyNormNode()** → `NormNode`
  - Empty node - keep as NormNode
  - Used as placeholder everywhere

## Medium-Confidence Candidates (Requires Testing)

### environment.nim - Environment Setup Functions

- **Line 402: createContinuation()** → `Statement`
  - Creates continuation variable declaration
  - Returns statement-like structure
  - Likely safe but needs testing

- **Line 430: createRecover()** → `Statement`
  - Creates recovery code block
  - Statement context usage
  - Likely safe but needs testing

- **Line 421: genException()** → `Statement` or `NormNode`
  - Creates exception handling code
  - Mixed returns - may need to stay NormNode
  - Test carefully

- **Line 272: initialization()** → `Statement`
  - Creates initialization statements
  - Used in var/let sections
  - Likely safe but needs testing

### transform.nim - Transformation Functions

- **Line 352: wrapContinuationWith()** → `Statement`
  - Wraps continuation setup code
  - Statement context
  - Likely safe but needs testing

- **Line 664: shimAssign()** → `Statement`
  - Creates assignment shim code
  - Statement-like structure
  - Likely safe but needs testing

- **Line 728: annotate()** → `NormNode`
  - Annotations vary by input
  - Keep as NormNode for flexibility
  - Don't convert

### exprs.nim - Expression Helpers

- **Line 7: newCpsMustLift()** → `Pragma`
  - Creates {.cpsMustLift.} pragma
  - Safe to convert

- **Line 169: assignTo()** → `Statement`
  - Creates assignment statements
  - nnkAsgn nodes typically
  - Likely safe

- **Line 394: annotate()** → `NormNode`
  - Varies by annotation type
  - Keep as NormNode
  - Don't convert

### rewrites.nim - Rewrite Functions

- **Line 52, 62: errorAst()** → `Expression` or `NormNode`
  - Creates error nodes
  - Could be Expression
  - May need testing

- **Line 160: normalizingRewrites()** → `NormNode`
  - Normalizes without changing fundamental type
  - Keep as NormNode for polymorphism
  - Don't convert

- **Line 24, 36, 42: filter()** → `NormNode`
  - Filters/transforms nodes
  - Used polymorphically
  - Keep as NormNode
  - Don't convert

- **Line 490, 501: replace()** → `NormNode`
  - Replaces nodes matching pattern
  - Keep as NormNode for polymorphism
  - Don't convert

## Low-Confidence (Keep as NormNode)

These should remain NormNode due to polymorphism or type variability:

### ast.nim
- **Line 445: newTree()** - Kind parameter determines result type
- **Line 467: wrap()** - Wrapper type varies
- **Line 225-227: copy operations** - Copy any type
- **Line 358, 365: add()** - Adds to any type
- **Line 386: getImpl()** - Implementation varies
- **Line 539: resym()** - Rewritten nodes vary
- **Line 746, 818: val()** - Value type varies
- **Line 1138: expr()** - Expression type varies
- **Line 1162: body()** - Body structure varies

### callbacks.nim
- **Line 26: cpsCallbackTypeDef()** - Type def structure

### defers.nim
- **Line 64: rewriteDefer()** - Varies by defer structure

### environment.nim
- **Line 89: maybeConvertToRoot()** - Varies
- **Line 133: objectType()** - Type extraction
- **Line 178: getResult()** - Result varies
- **Line 291, 299: addAssignment()** - Assignment varies
- **Line 354: rewriteResultReturn()** - Varies
- **Line 389: rewriteSymbolsIntoEnvDotField()** - Symbol rewrite varies
- **Line 583: rewriteVoodoo()** - Varies by voodoo call

### hooks.nim
- **Line 67: abbreviation()** - Abbreviation varies
- **Line 104, 115: hook()** - Hook construction varies
- **Line 156: initFrame()** - Frame init code varies
- **Line 163: updateLineInfoForContinuationStackFrame()** - Varies

### spec.nim
- **Line 145: filterPragma()** - Filters pragmas
- **Line 153: stripPragma()** - Strips pragmas
- **Line 351: pragmaArgument()** - Argument extraction varies
- **Line 373: bootstrapSymbol()** - Symbol varies
- **Line 597, 604: nilAsEmpty()/emptyAsNil()** - Conversion varies
- **Line 618: copyOrVoid()** - Type-dependent

### transform.nim
- **Line 182: restoreContinue()** - Restore structure
- **Line 242: mergeExceptBranches()** - Except handling varies
- **Line 650: newAnnotation()** - Annotation varies
- **Line 1169: cpsTransformProc()** - Transform result varies

## Recommended Conversion Order

### Phase 12a - Easy Wins (Very Safe)
1. `newCpsContinue()` (spec.nim:206) → Pragma
2. `newCpsTerminate()` (spec.nim:238) → Pragma
3. `newCpsBreak()` (spec.nim:192) → Pragma
4. `breakLabel()` (spec.nim:214) → Name
5. `flattenStmtList()` (spec.nim:262) → Statement

### Phase 12b - Returns Module (Safe)
6. `makeReturn()` (returns.nim:23) → Statement
7. `makeReturn()` (returns.nim:37) → Statement
8. `terminator()` (returns.nim:61) → Statement
9. `tailCall()` (returns.nim:90) → Call
10. `jumperCall()` (returns.nim:103) → Call

### Phase 12c - More Constructors (Test Each)
11. `newStmtList()` (ast.nim:439) → Statement (but test add())
12. `initialization()` (environment.nim:272) → Statement
13. `wrappedFinally()` (spec.nim:283) → Statement
14. `createContinuation()` (environment.nim:402) → Statement
15. `createRecover()` (environment.nim:430) → Statement

### Phase 12d - Transform Helpers (Careful)
16. `wrapContinuationWith()` (transform.nim:352) → Statement
17. `shimAssign()` (transform.nim:664) → Statement
18. `newCpsMustLift()` (exprs.nim:7) → Pragma
19. `assignTo()` (exprs.nim:169) → Statement

## Testing Strategy

For each conversion:
1. Change return type in function signature
2. Run smoke test: `timeout 15 balls tests/t00_smoke --define:release`
3. If pass → run full tests: `timeout 150 balls --define:release`
4. If fail → revert and document why
5. If pass → commit and move to next

## Expected Results

If all high-confidence candidates convert successfully:
- **Starting**: 73.8% adoption (254/344)
- **High-confidence**: +5 conversions → ~74.3%
- **Medium-confidence**: +10 conversions → ~76.5%
- **Target**: 75-80% adoption by end of Phase 12

## Notes

- Functions are grouped by confidence level
- Each includes reasoning for conversion or keeping as NormNode
- Test results will inform which medium-confidence ones are safe
- Keep polymorphic functions as NormNode for flexibility
- Focus on reducing NormNode returns to specific types
- Next phase will tackle function parameters
