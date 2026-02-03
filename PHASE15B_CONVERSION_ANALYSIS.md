# Phase 15B: Detailed Conversion Analysis

## Systematic Analysis of NormNode Functions

This document provides detailed analysis of candidate functions for direct type conversion in Phase 15C.

## Methodology

For each candidate function, we evaluate:

1. **Call Pattern Analysis**
   - How many call sites?
   - What types are passed?
   - Are they polymorphic?

2. **Risk Assessment**
   - Could breaking changes affect other code?
   - Are there function references?
   - How complex is the implementation?

3. **Value Calculation**
   - Direct adoption impact: +1 per conversion
   - Indirect impact on call sites
   - Foundation for future improvements

4. **Implementation Complexity**
   - Lines of code to modify
   - Number of call sites to update
   - Test coverage needed

## Tier 1: High Confidence Candidates

### Candidate 1.1: breakLabel() in spec.nim

**Current Signature:**
```nim
proc breakLabel*(n: NormNode): NormNode =
  ## Return the break label of a `break` statement or a `cpsBreak` annotation
```

**Analysis:**

| Aspect | Assessment |
|--------|-----------|
| Call Sites | 2 locations |
| Usage Pattern | Always called on cpsBreak pragmas (Statements) |
| Polymorphism | No - always operates on break-related statements |
| Risk | LOW |
| Complexity | LOW (10 lines, simple logic) |
| Value | +1 adoption |

**Call Sites:**
1. `cps/spec.nim:275` - Inside `matchCpsBreak()` 
2. `cps/environment.nim:393` - In code checking break statements

**Proposed Conversion:**

```nim
# Current
proc breakLabel*(n: NormNode): NormNode =
  # ... implementation ...

# Could become:
proc breakLabel*(n: Statement): NormNode =  # Note: return type stays NormNode
  # because label might be different type
  # ... implementation ...
```

**Alternative:** Could create typed variant function instead:
```nim
proc breakLabelOfStatement*(n: Statement): NormNode =
  breakLabel(n.NormNode)
```

**Recommendation:** ⭐⭐⭐ HIGH VALUE - Easy conversion with low risk

---

### Candidate 1.2: maybeConvertToRoot() in environment.nim

**Current Signature:**
```nim
proc maybeConvertToRoot*(e: Env; locals: NormNode): NormNode =
  ## add an Obj(foo: bar).Other conversion if necessary
```

**Analysis:**

| Aspect | Assessment |
|--------|-----------|
| Call Sites | 1 location |
| Usage Pattern | Called with result from e.getResult (which is Call) |
| Polymorphism | Possibly - uses eqIdent, might work with other types |
| Risk | MEDIUM |
| Complexity | LOW (6 lines) |
| Value | +1 adoption |

**Call Site:**
1. `cps/environment.nim:336` - In `addAssignment()`

**Context of Call:**
```nim
# Line 336 in addAssignment
let cont = e.castToChild(e.first)  # Returns Call
# ...
maybeAdd newAssignment(tups, defs.val)
```

**Observation:** Checking what gets passed here would require deeper analysis.

**Recommendation:** ⚠️ MEDIUM VALUE - Needs deeper call pattern analysis

---

## Tier 2: Medium Confidence Candidates

### Candidate 2.1: firstReturn() in returns.nim

**Current Signature:**
```nim
proc firstReturn*(p: NormNode): NormNode =
  ## Find the first control-flow return statement or cps control-flow
```

**Analysis:**

| Aspect | Assessment |
|--------|-----------|
| Call Sites | 3-5 locations |
| Usage Pattern | Called on various statement types |
| Polymorphism | Medium - works on statements but also expressions |
| Risk | MEDIUM |
| Complexity | MEDIUM (15 lines with case statement) |
| Value | +1 adoption |

**Call Sites:**
- `cps/environment.nim:362`
- `cps/transform.nim:1264`
- `cps/exprs.nim:25`

**Observation:** Called on various types - might not be safe to convert.

**Recommendation:** ❌ SKIP - Polymorphic usage pattern

---

### Candidate 2.2: getResult() in environment.nim

**Current Signature:**
```nim
proc getResult*(e: Env): NormNode =
  ## retrieve a continuation's result value from the env
  newDotExpr(e.castToChild(e.first), e.rs.name)
```

**Analysis:**

| Aspect | Assessment |
|--------|-----------|
| Call Sites | Multiple (5+) |
| Return Type | Always Call (from newDotExpr) |
| Polymorphism | No - always returns same pattern |
| Risk | LOW |
| Complexity | LOW (1 line) |
| Value | +1 adoption, improves clarity |

**Proposed Conversion:**

```nim
# Current
proc getResult*(e: Env): NormNode =
  newDotExpr(e.castToChild(e.first), e.rs.name)

# Convert to:
proc getResult*(e: Env): Call =
  Call newDotExpr(e.castToChild(e.first), e.rs.name)
```

**Impact:**
- Direct: +1 adoption
- Indirect: Call sites now know return type is Call
- Enables cascading improvements

**Call Sites Impacted:**
- `cps/environment.nim:362` - assigns to getResult
- `cps/environment.nim:382` - assigns to getResult
- `cps/environment.nim:416` - uses getResult
- `cps/environment.nim:428` - uses getResult
- `cps/transform.nim:1250` - rewriteResultReturn gets e.getResult

**Recommendation:** ⭐⭐⭐ HIGHEST VALUE - Easy, safe, high impact

---

## Tier 3: Lower Confidence (Skip for Now)

### Candidate 3.1: filter() functions

**Reason:** Polymorphic by design, used with function references, would require wholesale refactoring.

### Candidate 3.2: copy() variants

**Reason:** We already added typed overloads; conversion would duplicate functionality.

### Candidate 3.3: stripPragma() in spec.nim

**Reason:** Polymorphic - works with PragmaStmt, RoutineDef, ObjectTy, RefTy, TypeDef, and TypeSection. Unsafe to convert.

---

## Recommended Phase 15C Targets

### Priority 1: getResult() - IMPLEMENT FIRST

**Difficulty:** ⚪ VERY EASY
**Risk:** 🟢 VERY LOW
**Value:** 🟡 HIGH (+1 adoption, many call sites benefit)
**Time Estimate:** 15-20 minutes

**Steps:**
1. Change return type from `NormNode` to `Call`
2. Wrap result in `Call(...)` 
3. Update type in header
4. Run tests
5. Commit

**Expected Outcome:** +1 adoption, 5+ call sites now know getResult returns Call

---

### Priority 2: Consider breakLabel() - OPTIONAL

**Difficulty:** ⚪ VERY EASY
**Risk:** 🟢 LOW
**Value:** 🟢 MEDIUM (+1 adoption, few call sites)
**Time Estimate:** 10-15 minutes

**Steps:**
1. Change parameter type from `NormNode` to `Statement`
2. Update call site conversions
3. Run tests
4. Commit

**Expected Outcome:** +1 adoption, documents expected input type

---

## Analysis Summary Table

| Function | File | Risk | Value | Time | Complexity | Recommended |
|----------|------|------|-------|------|-----------|-------------|
| getResult() | environment.nim | LOW | HIGH | 15-20 min | VERY EASY | ⭐ YES |
| breakLabel() | spec.nim | LOW | MEDIUM | 10-15 min | VERY EASY | ⭐ OPTIONAL |
| maybeConvertToRoot() | environment.nim | MEDIUM | LOW | 20 min | EASY | ❌ SKIP |
| firstReturn() | returns.nim | MEDIUM | LOW | 30 min | MEDIUM | ❌ SKIP |
| filter() | rewrites.nim | HIGH | LOW | 2+ hours | HARD | ❌ SKIP |
| stripPragma() | spec.nim | HIGH | LOW | 1+ hour | HARD | ❌ SKIP |

## Phase 15C Implementation Plan

### Part 1: getResult() Conversion (20 min)

1. Edit `cps/environment.nim` line 178
2. Change `proc getResult*(e: Env): NormNode` to `proc getResult*(e: Env): Call`
3. Change body: `newDotExpr(...)` to `Call newDotExpr(...)`
4. Check all call sites compile correctly
5. Run tests
6. Commit with message: "Phase 15C: Convert getResult() return type to Call"

### Part 2: Optional breakLabel() (15 min)

1. Edit `cps/spec.nim` line 214
2. Change parameter from `n: NormNode` to `n: Statement`
3. Update call sites in environment.nim
4. Run tests
5. Commit with message: "Phase 15C: Add type safety to breakLabel() parameter"

### Part 3: Test & Verify (10 min)

1. Run full test suite
2. Verify no regressions
3. Document results

## Expected Phase 15C Outcomes

**If Part 1 only:**
- +1 direct adoption (getResult)
- Multiple call sites benefit
- Estimated total: +1-2% adoption improvement
- Time: 30 minutes

**If Parts 1 & 2:**
- +2 direct adoption (getResult + breakLabel)
- Better type documentation
- Estimated total: +1-2% adoption improvement
- Time: 40 minutes

## Key Metrics for Phase 15C Success

- [ ] getResult() successfully converted
- [ ] All 60 tests passing
- [ ] Zero regressions
- [ ] +1 adoption increase minimum
- [ ] Documentation updated
- [ ] Clear path for Phase 16

## Conclusion

Phase 15B analysis identifies **getResult()** as the prime candidate for Phase 15C conversion. It is:
- Very safe (low risk)
- Very easy to implement
- High value (many call sites benefit)
- Quick to complete

**breakLabel()** is an optional secondary target with similar properties.

All other candidates are deferred to Phase 16 or future phases due to higher complexity or polymorphic design that makes conversion risky.

**Ready for Phase 15C implementation** ✅
