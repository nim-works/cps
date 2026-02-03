# Functions That Cannot Be Easily Converted

These functions are either returning types that are difficult to convert, or their conversions would break downstream type system usage.

## Blocked Conversions - Phase 11

### ast.nim
1. **newStmtList** (line 439) - Returns NormNode
   - **Issue**: Converting to Statement breaks add() function
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

2. **wrap** (line 467) - Returns NormNode
   - **Issue**: NimNodeKind varies by input, can't be specific type
   - **Difficulty**: High - semantically impossible
   - **Status**: LOSER

3. **dot** (line 602) - Returns NormNode
   - **Issue**: Converting to Expression breaks downstream type checking
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

4. **dot** (line 608) - Returns NormNode
   - **Issue**: Same as line 602
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

5. **val** (line 746) - Returns NormNode
   - **Issue**: Extracts children, type varies by input
   - **Difficulty**: High - type varies
   - **Status**: LOSER

6. **val** (line 818) - Returns NormNode
   - **Issue**: Extracts children, type varies by input
   - **Difficulty**: High - type varies
   - **Status**: LOSER

7. **expr** (line 1138) - Returns NormNode
   - **Issue**: Indexes Conv node, type varies
   - **Difficulty**: High - type varies
   - **Status**: LOSER

8. **body** (line 1162) - Returns NormNode
   - **Issue**: Converting to Statement breaks downstream code
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

### callbacks.nim
1. **cpsCallbackTypeDef** (line 26) - Already returns NormNode ✓

2. **createCallback** (line 49) - Returns NimNode
   - **Issue**: Output is used in untyped macro context in `cps.nim`
   - **Problem**: Converting to NormNode causes type mismatch in `whelp` macro
   - **Difficulty**: High - would require changing macro context
   - **Status**: LOSER

3. **createCastCallback** (line 70) - Returns NimNode
   - **Issue**: Similar to createCallback, breaks macro context
   - **Difficulty**: High - would require changes in multiple files
   - **Status**: LOSER

4. **rewriteCalls** (line 185) - Returns NimNode
   - **Issue**: Internal to `when false` block, unused code
   - **Difficulty**: N/A - dead code
   - **Status**: LOSER

5. **recall** (line 188) - Returns NimNode
   - **Issue**: Internal to `when false` block, unused code
   - **Difficulty**: N/A - dead code
   - **Status**: LOSER

6. **performUntypedPass** (line 198) - Returns NimNode
   - **Issue**: Internal to `when false` block, unused code
   - **Difficulty**: N/A - dead code
   - **Status**: LOSER

### defers.nim
1. **rewriteDefer** (line 64) - Already returns NormNode ✓
   - **Issue**: Converting to Statement breaks add() function
   - **Status**: LOSER

2. **rewriter** (line 67) - Nested function
   - **Status**: LOSER (internal)

### environment.nim
1. **maybeConvertToRoot** (line 89) - Already returns NormNode ✓

2. **objectType** (line 133) - Returns NormNode
   - **Issue**: Converting to TypeExpr breaks object construction
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

3. **getResult** (line 178) - Already returns NormNode ✓

4. **initialization** (line 272) - Returns NormNode
   - **Issue**: Creates newStmtList, but Statement too specific
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

5. **addAssignment** (line 291) - Already returns NormNode ✓

6. **addAssignment** (line 299) - Already returns NormNode ✓

7. **getFieldViaLocal** (line 305) - In `when false` block
   - **Status**: LOSER (dead code)

8. **rewriteResultReturn** (line 354) - Already returns NormNode ✓

9. **rewriter** (line 356) - Nested function
   - **Status**: LOSER (internal)

10. **rewriteSymbolsIntoEnvDotField** (line 389) - Already returns NormNode ✓

11. **resultdot** (line 405) - Nested function
    - **Status**: LOSER (internal)

12. **star** (line 459) - Nested function
    - **Status**: LOSER (internal)

13. **rewriteVoodoo** (line 583) - Already returns NormNode ✓

14. **voodoo** (line 586) - Already returns NormNode ✓

### exprs.nim
1. **newCpsMustLift** (line 7) - Already returns NormNode ✓
2. **rewriteElifOf** (line 55) - Already returns NormNode ✓
3. **assignTo** (line 169) - Already returns NormNode ✓
4. **assign** (line 173) - Already returns NormNode ✓
5. **addConv** (line 326) - Already returns NormNode ✓
6. **addDiscard** (line 334) - Already returns NormNode ✓
7. **addReturn** (line 342) - Already returns NormNode ✓
8. **addRaise** (line 350) - Already returns NormNode ✓
9. **lift** (line 359) - Already returns NormNode ✓
10. **lifter** (line 361) - Already returns NormNode ✓
11. **annotate** (line 394) - Already returns NormNode ✓

### hooks.nim
1. **hook** (line 104) - Already returns NormNode ✓
2. **hook** (line 115) - Already returns NormNode ✓
3. **initFrame** (line 156) - Already returns NormNode ✓

### returns.nim
1. **firstReturn** (line 5) - Returns NormNode
   - **Issue**: Converting to Statement breaks downstream code
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER

2. **makeReturn** (line 23) - Already returns NormNode ✓
3. **makeReturn** (line 37) - Already returns NormNode ✓
4. **terminator** (line 61) - Already returns NormNode ✓
5. **tailCall** (line 90) - Already returns NormNode ✓
6. **jumperCall** (line 103) - Already returns NormNode ✓

### spec.nim
1. **bootstrapSymbol** (line 373) - Already returns NormNode ✓
2. **enbasen** (line 395) - Already returns TypeExpr ✓
3. **makeErrorShim** (line 418) - Returns NimNode
   - **Issue**: Converting breaks pragma manipulation downstream
   - **Difficulty**: High - macro context issues
   - **Status**: LOSER

4. **nilAsEmpty** (line 597) - Already returns NormNode ✓
5. **emptyAsNil** (line 604) - Already returns NormNode ✓
6. **copyOrVoid** (line 618) - Returns NormNode
   - **Issue**: Converting to TypeExpr breaks object construction
   - **Difficulty**: High - too specific a type
   - **Status**: LOSER
1. **makeErrorShim** (line 418) - Returns NimNode
   - **Issue**: Attempted conversion to ProcDef failed
   - **Problem**: Later code uses it with `addPragma` which expects NimNode for manipulation
   - **Difficulty**: High - requires significant refactoring
   - **Status**: NOT CONVERTED (attempt failed)

### environment.nim
1. **letOrVar** (line 281) - Returns NimNodeKind
   - **Issue**: Returns an enum, not a NimNode
   - **Problem**: Already correct type for its purpose
   - **Difficulty**: N/A - not actually a NimNode return
   - **Status**: CORRECT AS-IS

2. **star** (line 459) - Returns NimNode (internal to genException)
   - **Issue**: Internal nested function, complex return logic
   - **Problem**: Would require understanding broader context
   - **Difficulty**: High
   - **Status**: SKIPPED

## Why These Are Difficult

### Type System Issues
- Some functions are used in contexts where `untyped` is expected
- Converting to `NormNode` can break downstream type checking in macros
- The distinct type system is powerful but restrictive in some contexts

### Semantic Complexity
- Some functions manipulate AST in ways that require the flexibility of NimNode
- Adding pragmas, modifying procedure definitions requires raw NimNode in some cases
- Type conversions would add overhead in performance-sensitive areas

### Integration Issues
- Functions that return results assigned to `result: untyped` in macros
- Functions whose output is used as construction arguments need NimNode
- Object construction and pragma manipulation sometimes need raw node access

## Strategy for Blocked Functions

### Option 1: Accept as-is
Keep these functions returning NimNode since:
- They already work correctly
- Converting would break existing code
- The performance/type-safety trade-off favors keeping them as NimNode

### Option 2: Wrapper Functions
Create new typed functions that wrap the untyped ones:
```nim
proc createCallbackSafe*(sym: NimNode): NormNode =
  createCallback(sym).NormNode
```
This allows gradual adoption without breaking existing code.

### Option 3: Broader Refactoring
Redesign the entire callback system to work with NormNode throughout:
- Would require changes in cps.nim, transform.nim, and callbacks.nim
- High risk of regressions
- Not attempted in this pass

## Recommendation

For blocked functions, we recommend:
1. Accept them returning NimNode for now
2. Focus on converting functions that call these (upper layers)
3. When more of the codebase is typed, revisit with wrapper approach
4. Document the specific reasons for exceptions

The 7 successfully converted functions show that strategic conversion is possible, and this list documents where the boundaries are.
