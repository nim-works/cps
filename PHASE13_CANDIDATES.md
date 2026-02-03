# Phase 13 Parameter Conversion Candidates

## Analysis Results

### Current Situation
- 80.7% adoption (239/296)
- Need ~15-20 more conversions for 85%+
- 90 functions with NormNode parameters identified

### Challenge
Unlike Phase 12's pure constructors, most NormNode parameters are:
- Used polymorphically (generic add(), filter(), etc.)
- Required to be flexible
- Already appropriate as NormNode

### Strategic Options for Phase 13

## Option 1: Create Typed Wrapper Functions
**Approach:** Create new functions that are typed versions of polymorphic ones
**Benefit:** No changes to existing code, gradual adoption
**Example:**
```nim
# Original (stays polymorphic)
proc hook(n: NormNode): NormNode = ...

# New typed wrapper
proc hookStatement(n: Statement): Statement =
  hook(n.NormNode).Statement
```

## Option 2: Look for Overloads
**Approach:** Add type-specific overloads that delegate to generic versions
**Benefit:** Clean API, type safety where it matters
**Example:**
```nim
# Original generic
proc initialize(e: var Env; n: NormNode): NormNode = ...

# New typed overload
proc initialize(e: var Env; n: Statement): Statement =
  initialize(e, n.NormNode).Statement
```

## Option 3: Continue Return-Type Conversions
**Approach:** Find more functions that return specific types
**Benefit:** Direct impact on adoption metric
**Difficulty:** We've already found most of the easy ones

## Recommended Strategy for Phase 13

### Hybrid Approach

#### Part A: Return-Type Conversions (if any more found)
- Continue searching for pure constructors
- Target: 3-5 more conversions
- Expected impact: +1-2% adoption

#### Part B: Typed Wrappers (new approach)
- Create typed wrappers for commonly-typed functions
- Start with: `hook()`, `filter()`, assignment functions
- Target: 5-10 wrappers
- Expected impact: +2-3% adoption (indirect through call site improvements)

#### Part C: Analysis & Documentation
- Document which functions genuinely need to stay polymorphic
- Identify patterns in type-safe vs. polymorphic code
- Create guidelines for future phases

### Phase 13 Execution Plan

#### Stage 1: Identify Return-Type Conversions (30 min)
```bash
# Look for functions that clearly return only one type
grep -n "newPragma\|newStmtList\|newCall" cps/*.nim | grep "proc.*NormNode ="
```

#### Stage 2: Create Wrapper Functions (1-2 hours)
- Pick 3-5 high-impact functions
- Create typed wrappers
- Test thoroughly

#### Stage 3: Test & Verify (1 hour)
- Smoke test after each wrapper
- Full test suite at end
- Document what worked

## High-Priority Return-Type Candidates

Looking at the 29 remaining NormNode-returning functions:
1. `nilAsEmpty()` / `emptyAsNil()` - Maybe keep as NormNode (variable types)
2. `copyOrVoid()` - Variable types
3. `getResult()` - Need to check
4. `initialization()` - Creates statements, might convert
5. `bootstrap Symbol()` - Variable types

Most remaining are genuinely polymorphic or have variable types.

## High-Priority Wrapper Candidates

Functions that are called with consistent types and could benefit from wrappers:

1. **`filter()` variants** - Called with specific predicates
   - Could create `filterStatements()`, `filterExpressions()`
   - Impact: HIGH (used in many places)

2. **`add()` variants** - Fundamental but polymorphic
   - Could create `addStatement()`, `addExpression()`
   - Impact: HIGH (very common)

3. **`hook()` functions** - Called with specific node types
   - Could create `hookStatement()`, `hookCall()`
   - Impact: MEDIUM (specialized use)

4. **Environment functions** - Work with specific node types
   - Could create typed variants
   - Impact: MEDIUM (isolated usage)

## Expected Phase 13 Outcome

### Conservative (Wrapper-focused)
- 5-10 wrapper functions created
- Return-type conversions: 2-3
- Total: ~7-13 functions improved
- Adoption: 81-82%

### Optimistic (Comprehensive)
- 10-15 wrapper functions created
- Return-type conversions: 5-8
- Total: ~15-23 functions improved
- Adoption: 82-85%

### Ideal (Everything works)
- 15-20 wrapper functions created
- Return-type conversions: 8-10
- Cascading improvements from wrapper adoption
- Adoption: 85%+

## Decision Point

**Key Question:** Should we focus on:
A. Wrappers (safer, more work, indirect benefit)
B. More return-type conversions (riskier, direct benefit)
C. Hybrid (both approaches)

**Recommendation:** Start with return-type conversions (find any remaining), then shift to wrappers if no more easy conversions exist.

## Next Steps

1. Search for any remaining simple constructor functions
2. If found, convert them
3. If not found, pivot to wrapper strategy
4. Document results and patterns
5. Plan Phase 14 accordingly
