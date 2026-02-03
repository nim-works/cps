# Next Steps for CPS Type System Improvements

## Current Status
- **Overall Adoption**: 73.8% (254/344 functions with specific types)
- **Test Status**: All 60 tests passing (ARC + ORC)
- **Phase**: Completed Phase 11, ready for Phase 12

## Quick Reference

### Files with Lowest Adoption (Priority Targets)
1. **callbacks.nim**: 27.3% (3/11) - Consider wrapper functions
2. **spec.nim**: 32.8% (21/64) - Review type helper functions
3. **help.nim**: 50% (1/2) - Easy wins possible
4. **environment.nim**: 69% (29/42) - Some functions blocked

### Files with Highest Adoption (Well Typed)
1. **defers.nim**: 100% (4/4) ✅
2. **rewrites.nim**: 88.9% (24/27) ✅
3. **returns.nim**: 87.5% (7/8) ✅
4. **exprs.nim**: 84.6% (11/13) ✅
5. **ast.nim**: 76.4% (113/148) - Good coverage

## Documented Blockers

### Macro Context Issues (High Risk - Skip for Now)
```
- createCallback (callbacks.nim:49)
- createCastCallback (callbacks.nim:70)
- makeErrorShim (spec.nim:418)
```
These need untyped context. Would require changing cps.nim macro.

### Type Specificity Issues (Medium Risk - Consider Wrapper Functions)
```
- newStmtList (ast.nim:439)
- body (ast.nim:1162)
- firstReturn (returns.nim:5)
- objectType (environment.nim:133)
- copyOrVoid (spec.nim:618)
- wrap (ast.nim:467)
- val (ast.nim:746, 818)
- expr (ast.nim:1138)
```
These fail because converting to specific types breaks add() or other generic operations.

### Dead Code (Low Risk - Skip for Now)
```
- rewriteCalls (callbacks.nim:185) - when false block
- recall (callbacks.nim:188) - when false block
- performUntypedPass (callbacks.nim:198) - when false block
- getFieldViaLocal (environment.nim:305) - when false block
```

## Recommended Phase 12 Approach

### Priority 1: Quick Wins
1. **help.nim** - Review the 1 untyped function (50% adoption)
2. **exprs.nim** - Review the 2 untyped functions (84.6% adoption)
3. **returns.nim** - Review the 1 untyped function (87.5% adoption)

### Priority 2: Wrapper Functions
Consider creating wrapper functions for blocked functions:
```nim
# Example approach:
proc createCallbackSafe*(sym: NimNode): NormNode =
  createCallback(sym).NormNode

proc objectTypeSafe*(n: NormNode): TypeExpr =
  objectType(n).TypeExpr
```

This allows gradual adoption without breaking existing code.

### Priority 3: Lower Adoption Files
1. **callbacks.nim** (27.3%) - After wrapper strategy is clear
2. **spec.nim** (32.8%) - Has many small helpers, systematic review needed
3. **environment.nim** (69%) - Some conversions still possible

### Priority 4: Complex Refactoring (Future)
- Consider redesigning callback system to work with NormNode throughout
- May require changes in cps.nim, transform.nim, and callbacks.nim
- High risk but could unlock more conversions

## Testing Strategy

### Quick Smoke Test
```bash
timeout 15 balls tests/t00_smoke --define:release
```

### Full Test Suite
```bash
timeout 150 balls --define:release
```

### After Each Conversion
1. Run smoke test
2. If pass → full test suite
3. If fail → revert and document as LOSER

## Documentation Commands

### View Current Coverage
```bash
python3 -c "
import re
files = {...}  # Add file paths
for f in files:
    content = open(f).read()
    norm = len(re.findall(r': (?:NormNode|TypeExpr|Statement|Expression|Call|Name|ProcDef)\s*(?:=|{|,)', content))
    procs = len(re.findall(r'^(?:proc|func|template) \w+', content, re.MULTILINE))
    print(f'{f}: {norm}/{procs} ({norm/procs*100:.1f}%)')
"
```

### Find Functions Needing Review
```bash
grep -n "^proc\|^func" cps/FILENAME.nim | grep -v "NormNode\|TypeExpr\|Statement\|Expression\|bool\|string"
```

## Key Insights from Phase 11

1. **NormNode Flexibility vs. Type Safety**
   - NormNode is great for type safety but restrictive for polymorphic operations
   - Some functions need raw NimNode access for full flexibility

2. **Macro Context Matters**
   - Functions outputting to untyped contexts must remain NimNode
   - Converting breaks type inference in macro expansion

3. **Object Construction Needs Raw Nodes**
   - Some AST construction patterns need NimNode flexibility
   - Adding pragmas, modifying proc defs sometimes requires raw nodes

4. **Call Pattern Analysis Works**
   - Looking at what functions call (and get called by) reveals opportunities
   - Cascading conversions are possible but rare

## Git Workflow

### For New Conversions:
```bash
# 1. Read the function thoroughly
# 2. Edit to change return type
# 3. Run smoke test
# 4. If pass → run full tests
# 5. If pass → commit
# 6. Update TARGETS.md to mark [x]
# 7. Add to NORMALS.md
```

### For Failed Conversions:
```bash
# 1. Revert changes
# 2. Update TARGETS.md to mark [x] LOSER: <reason>
# 3. Add to LOSERS.md
# 4. Commit with explanation
```

## Resources

- **TARGETS.md** - Master tracking of all functions
- **NORMALS.md** - Successfully converted functions
- **LOSERS.md** - Documented blockers with analysis
- **PHASE11_SUMMARY.md** - Comprehensive phase analysis

## Expected Outcomes

If all Priority 1 and Priority 2 tasks complete successfully:
- **Target Adoption**: 75-80% (increase of 1-6%)
- **Remaining Blockers**: 8-10 functions that are legitimately hard
- **Good Starting Point**: For longer-term macro refactoring

## Questions to Answer

1. Can wrapper functions be used without overhead?
2. Should we redesign the callback system?
3. Is macro context a hard constraint or design choice?
4. Can we safely convert the "type variability" functions?

These answers will inform Phase 13+ work.
