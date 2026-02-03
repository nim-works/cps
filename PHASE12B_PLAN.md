# Phase 12b Plan: Continue NormNode → Specific Type Conversions

## Current Status
- **Adoption**: 74.7% (257/344)
- **Phase 12a**: 3 successful pragma conversions ✅
- **Tests**: All 60 passing
- **Strategy**: Pure constructors only (safe, single-purpose)

## Confirmed Safe Candidates for Phase 12b

### Category 1: Simple Constructors (Very Safe)
These all create single, specific types with no complex logic:

1. **`errorAst()` functions** (rewrites.nim:52, 62)
   - Creates error nodes (nnkCall with error pragma)
   - Always returns same structure
   - Safe to return: `Call` or `Expression`
   - Risk: LOW

2. **`newCpsMustLift()` (exprs.nim:7)**
   - Creates {.cpsMustLift.} pragma
   - Can return: `Pragma`
   - Risk: LOW

3. **`nilAsEmpty()` / `emptyAsNil()` (spec.nim:597, 604)**
   - Convert nil ↔ empty transformations
   - Always return transformations of same type
   - Keep as: `NormNode` (polymorphic)
   - Risk: N/A (don't convert)

### Category 2: Type Constructors (Medium-Safe)
These create type-related structures:

4. **`sinkAnnotated()` (ast.nim)**
   - Creates type expressions
   - Can return: `TypeExpr`
   - Risk: MEDIUM (validate callers)

5. **`newPragmaColonExpr()` (ast.nim)**
   - Creates pragma expressions
   - Can return: `PragmaAtom`
   - Risk: LOW

## Confirmed Unsafe (Don't Attempt)

### Attribute Access Blockers
- `breakLabel()` - callers use `.kind`
- Any function where callers check node attributes

### Polymorphic Functions (Keep as NormNode)
- `filter()` - filters any type
- `replace()` - replaces any type
- `wrap()` - wraps in any kind
- `add()` - adds to any type
- `newTree()` - creates any kind
- `copy()` - copies any type

### Variable Returns (Keep as NormNode)
- `flattenStmtList()` - returns unwrapped type
- `val()` - returns value of any type
- `expr()` - returns expression of any type
- `body()` - returns body of any type

### Complex Call Chains (Create wrappers instead)
- `makeReturn()` - many callers expect NormNode
- `tailCall()` - many callers expect NormNode
- `jumperCall()` - calls tailCall which expects NormNode

## Phase 12b Strategy

### Stage 1: Very Safe Conversions (Do immediately)
1. Check `errorAst()` uses - likely safe
2. Check `newCpsMustLift()` uses - likely safe  
3. Try converting both
4. Run smoke test
5. If pass → full tests → commit

### Stage 2: Medium-Safe (Test carefully)
1. Check `sinkAnnotated()` call sites
2. Check `newPragmaColonExpr()` call sites
3. Understand type flows
4. Attempt conversion
5. Run tests

### Stage 3: Wrapper Functions (If needed)
If some functions block the medium-safe conversions:
```nim
# Example: Safe wrapper for makeReturn
proc makeReturnTyped*(contType: Name; n: NormNode): Statement =
  makeReturn(contType, n).Statement
```

This allows typed code to use safer version without changing original.

## Expected Outcomes

### Scenario A: All Stage 1 Succeed
- Target: ~76% adoption (260/344)
- Can proceed to Stage 2

### Scenario B: Some Fail
- Document why in CANDIDATE_ANALYSIS.md
- Move to wrapper strategy
- Still achieve ~75-76%

## Testing Checklist

For each conversion attempt:
- [ ] Read function completely
- [ ] Check all call sites
- [ ] Identify callers (grep for function name)
- [ ] Verify callers don't rely on base NormNode type
- [ ] Convert return type
- [ ] Add conversions (e.g., `.Statement`)
- [ ] Run smoke test (15s)
- [ ] If fail → revert → document → move on
- [ ] If pass → run full tests (150s)
- [ ] If pass → commit

## Git Workflow

```bash
# For each conversion:
git checkout -b phase-12b-<function-name>

# Make changes, test
timeout 15 balls tests/t00_smoke --define:release

# If all good:
timeout 150 balls --define:release

# If pass:
git commit -m "Phase 12b.X: Convert <name> to <Type>"
git checkout main
git rebase main phase-12b-<name>

# If fail:
git checkout cps/FILENAME.nim  # revert
git commit -m "Revert <name> conversion - <reason>"
```

## Priority Order for Attempts

1. **`errorAst()`** - Simplest, most isolated
2. **`newCpsMustLift()`** - Also simple, clear purpose
3. **`sinkAnnotated()`** - Requires more analysis
4. **Others** - Based on dependency analysis

## Success Criteria

- ✅ Conversions complete and compile
- ✅ All 60 tests passing
- ✅ Smoke test < 3.5s per config
- ✅ Full test suite < 120s
- ✅ Adoption reaches 75-76%
- ✅ Clear documentation in CANDIDATE_ANALYSIS.md

## Documentation Updates

Keep CANDIDATE_ANALYSIS.md updated:
- Mark attempted conversions with ✅ or ❌
- Add lessons learned
- Update Phase 12b results section

## Next Phase (12c+)

If Phase 12b achieves 76%+:
- Continue with remaining safe conversions
- Target: 77-78%
- Then: Function parameters (Phase 13)

If Phase 12b plateaus at 75%:
- Take stock: ~15-20 truly safe conversions found
- Rest are genuinely unsafe
- Shift strategy to parameter conversions
- Revisit return types with wrapper functions

## Resources

- CANDIDATE_ANALYSIS.md - All 81 functions analyzed
- NEXT_STEPS.md - Overall strategy
- SESSION_SUMMARY.md - Lessons learned
- PHASE11_SUMMARY.md - Context from Phase 11

## Quick Reference: Commands

```bash
# Smoke test
timeout 15 balls tests/t00_smoke --define:release

# Full test
timeout 150 balls --define:release

# Show adoption
python3 -c "
import re
total_typed = len(re.findall(r': (?:NormNode|Statement|Expression|Call|TypeExpr|Pragma|PragmaStmt|Name|ProcDef|IdentDef|Sym)', open('cps/spec.nim').read()))
total_funcs = len(re.findall(r'^(?:proc|func)', open('cps/spec.nim').read(), re.M))
print(f'{total_typed}/{total_funcs} = {total_typed/total_funcs*100:.1f}%')
"

# List NormNode returning functions
grep -E "^proc|^func" cps/*.nim | grep ": NormNode"
```

Ready for Phase 12b! 🚀
