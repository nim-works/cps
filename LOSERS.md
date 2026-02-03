# Functions That Cannot Be Easily Converted

These functions are either returning types that are difficult to convert, or their conversions would break downstream type system usage.

## Blocked Conversions

### callbacks.nim
1. **createCallback** (line 49) - Returns NimNode
   - **Issue**: Output is used in untyped macro context in `cps.nim`
   - **Problem**: Converting to NormNode causes type mismatch in `whelp` macro
   - **Difficulty**: Would require changing macro context handling
   - **Status**: NOT CONVERTED

2. **createCastCallback** (line 70) - Returns NimNode
   - **Issue**: Similar to createCallback, returns object construction
   - **Problem**: Type system usage downstream expects NimNode
   - **Difficulty**: High - would require changes in multiple files
   - **Status**: NOT CONVERTED

3. **rewriteCalls** (line 185) - Returns NimNode
   - **Issue**: Internal to commented-out code block
   - **Problem**: Unclear use case, wrapped in `when false`
   - **Difficulty**: Medium - low priority
   - **Status**: SKIPPED

4. **performUntypedPass** (line 198) - Returns NimNode
   - **Issue**: Internal to commented-out code block
   - **Problem**: Same as rewriteCalls
   - **Difficulty**: Medium - low priority
   - **Status**: SKIPPED

### spec.nim
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
