# Phase 13 Roadmap: Continue Type System Improvements

## Current Status
- **Adoption: 80.7%** (up from 74.7% at start of Phase 12)
- **Total Conversions: 5 in Phase 12** (all pure constructors)
- **Tests: All passing** (60 tests, ARC + ORC)
- **Strategy: Proven** (NormNode → specific types is effective)

## Phase 13 Strategy

### Strategic Shift: Function Parameters
After maximizing return type conversions, Phase 13 should focus on **function parameters**.

**Why Parameters Come Next:**
1. **Lower risk** - Changes don't cascade as much as return types
2. **Many candidates** - 100+ NormNode parameters across codebase
3. **Clearer intent** - Parameters document what functions accept
4. **Caller-controlled** - Can convert one function at a time

### Remaining Return Type Opportunities

**High Confidence (Pure Constructors)**
- Functions in environment.nim that create specific structures
- Simple assignment/setup functions
- Initialization helpers

**Medium Confidence (Check Each)**
- Functions that filter/transform (may have conditional returns)
- Functions that manipulate specific node types
- Helper functions for types

**Low Confidence (Skip for Now)**
- Polymorphic functions (work with any type)
- Functions with attribute access requirements
- Low-level utilities

## Phase 13 Execution Plan

### Stage 1: Identify Parameter Conversion Candidates
```bash
# Find functions with NormNode parameters
grep -n "(.*: NormNode" cps/*.nim | head -100

# Examples to look for:
- hook(n: NormNode) could be hook(n: Statement) or hook(n: Call)
- filter(n: NormNode) - already poly, keep generic
- create*(n: NormNode) - check if always same type input
```

### Stage 2: Analyze Top 10 Candidates
For each candidate:
1. Read function completely
2. Check what the parameter is actually used for
3. Verify callers always pass same type
4. Attempt conversion with tests

### Stage 3: Convert in Batches
- Batch 1: 3-5 safe conversions
- Run smoke test after each batch
- Run full suite after all conversions
- Commit and document

## Specific Candidates for Phase 13

### High Priority Parameter Conversions

1. **Hook functions** (hooks.nim)
   - `hook(n: NormNode)` - Check if always statements/calls
   - Risk: LOW (pure logic transformation)

2. **Filter functions** (already NormNode polymorphic)
   - SKIP - designed to work with any type

3. **Environment setup** (environment.nim)
   - `createContinuation(n: NormNode)` - Check what types passed
   - Risk: MEDIUM (complex context)

4. **Assignment functions** (exprs.nim)
   - `assignTo(n: NormNode)` - Check types
   - Risk: MEDIUM (needs detailed analysis)

### Conversion Checklist for Each Parameter

- [ ] Identify which types are actually passed
- [ ] Check if all callers pass same type
- [ ] Verify function never needs other types
- [ ] Convert parameter type
- [ ] Update function body to match
- [ ] Smoke test
- [ ] Full test suite
- [ ] Commit with explanation

## Expected Outcomes

### Conservative Estimate (Phase 13)
- 3-5 parameter conversions
- Adoption: 81-82%
- 0 regressions
- Better type safety

### Optimistic Estimate
- 10-15 parameter conversions
- Adoption: 82-85%
- Cascading benefits to other functions
- Significant type system improvements

## Testing Strategy

### After Each Conversion
1. Smoke test (15s): `timeout 15 balls tests/t00_smoke --define:release`
2. If pass → Full suite (150s): `timeout 150 balls --define:release`
3. If any fail → Revert immediately

### Success Criteria
- ✅ All 60 tests pass
- ✅ ARC and ORC both work
- ✅ Zero type errors
- ✅ Zero runtime issues

## Documentation Updates

### Create/Update
- PHASE13_SUMMARY.md (when complete)
- PHASE14_ROADMAP.md (next phase)

### Keep Current
- CANDIDATE_ANALYSIS.md (add parameter analysis)
- TARGETS.md (track conversions)
- NORMALS.md (log successes)

## Risk Management

### If Conversions Fail
1. **Don't force it** - Some parameters are intentionally generic
2. **Use wrappers instead** - Create typed wrapper for safe subset
3. **Document why** - Add to LOSERS.md with explanation
4. **Move on** - Plenty of other improvements possible

### Wrapper Pattern Example
```nim
# Original stays generic (for polymorphic callers)
proc hook(n: NormNode): NormNode = ...

# New typed variant (for type-safe callers)
proc hookStatement(n: Statement): Statement =
  hook(n.NormNode).Statement
```

This allows gradual adoption without breaking existing code.

## Phase 13 Success Metrics

### Minimum Success
- 3 parameter conversions
- 80.7% → 81.5% adoption (+0.8%)
- All tests passing
- Zero regressions

### Target Success
- 10 parameter conversions
- 80.7% → 83% adoption (+2.3%)
- All tests passing
- Zero regressions
- Clear patterns documented

### Stretch Goal
- 15 parameter conversions
- 80.7% → 84% adoption (+3.3%)
- Cascading benefits enabling more conversions
- Documented wrapper patterns

## Timeline Estimate

- **Analysis**: 30 minutes (identify candidates)
- **Conversion**: 1-2 hours (10 attempts)
- **Testing**: 2-3 hours (smoke + full tests)
- **Documentation**: 30 minutes

**Total: 4-6 hours for Phase 13**

## Git Workflow for Phase 13

### For Each Parameter Conversion
```bash
# Create branch
git checkout -b phase-13-<function-name>

# Make changes
<edit file>

# Test
timeout 15 balls tests/t00_smoke --define:release

# If pass: full test
timeout 150 balls --define:release

# Commit
git commit -m "Phase 13.X: Convert <func> parameter <name> to <Type>"

# Back to main
git checkout main
git rebase main phase-13-<name>
```

## Resources

- PHASE12_SUMMARY.md - What worked (pure constructors)
- CANDIDATE_ANALYSIS.md - 81 functions analyzed
- PHASE12B_PLAN.md - Execution patterns
- TARGETS.md - Master tracking list

## Key Principles to Remember

1. **Parameters are safer than returns** - Less cascade risk
2. **Small batches are better** - 3-5 per session
3. **Test after each batch** - Catch issues early
4. **Document everything** - Help future phases
5. **Wrappers are friends** - Can wrap what can't be changed
6. **Keep polymorphic code generic** - Don't force types where flexibility needed

## Questions to Answer in Phase 13

1. Which parameters can be safely typed?
2. Do callers always pass same type?
3. Would typing improve readability?
4. Are there wrapper opportunities?
5. What patterns emerge?

These answers inform Phase 14+ strategy.

---

**Ready for Phase 13!** The success of Phase 12 proves the approach works. Phase 13 should build on this foundation with parameter conversions.

Target: **85%+ adoption by end of Phase 14**
