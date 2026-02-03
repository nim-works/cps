# Phase 1 Implementation Guide: Module Migration to ast.nim

**Phase**: Phase 1 (Weeks 2-4, ~160 hours)  
**Goal**: Achieve >85% ast.nim adoption across all modules  
**Status**: Ready for implementation  

## Overview

Phase 1 focuses on achieving comprehensive ast.nim adoption across the codebase. This guide provides patterns, strategies, and specific implementation notes for each module.

## Core Principle

Replace raw `NimNode` usage with strongly-typed distinct types from `ast.nim`. The only exceptions should be:
1. At genAst boundaries (where untyped macros interact with runtime code)
2. In rewrites.nim (which normalizes to NormNode)
3. In critical performance paths (rare and documented)

## Implementation Strategy

### Step-by-Step Process

1. **Audit the module** - List all NimNode parameters and returns
2. **Identify the type** - Map NimNode to appropriate ast.nim type
3. **Create converter** - If needed, add `asXXX` function to narrow type
4. **Update signatures** - Change function signatures to use strong types
5. **Update call sites** - Propagate changes through dependent code
6. **Test** - Run tests frequently to catch regressions
7. **Commit** - Create atomic, testable commits

### Type Mapping Patterns

| NimNode context | Recommended Type | Pattern |
|-----------------|------------------|---------|
| Any proc definition | `ProcDef` | Use `normalizeProcDef()` if not already normalized |
| Any call | `Call` | Use `normalizeCall()` |
| Type expression | `TypeExpr` | Validate with `expectKind(n, TypeExprKinds)` |
| Variable definition | `IdentDef` or `VarLet` | Narrow from parent context |
| Generic node | `Statement \| Expression` | For unspecialized handling |
| Name-like (ident/sym) | `Name` | Covers both identifier and symbol nodes |
| Any pragma | `Pragma` | Opaque sum of pragma variants |

### Common Patterns to Replace

**Pattern 1: Generic parameter narrowing**
```nim
# Before (accepts any NimNode)
proc handleDef(n: NimNode): NormNode =
  if n.kind == nnkIdentDefs:
    result = processIdentDef(n)
  else:
    error("Expected IdentDef", n)

# After (type enforces correctness)
proc handleDef(n: IdentDef): NormNode =
  result = processIdentDef(n)
  # Type system guarantees we have nnkIdentDefs
```

**Pattern 2: Return type refinement**
```nim
# Before (caller must check result)
proc extractType(n: NimNode): NimNode =
  # Extract and return type expression
  # Caller doesn't know what kind it is

# After (type documents return value)
proc extractType(n: IdentDef): TypeExpr =
  # Caller knows this is a valid type expression
```

**Pattern 3: Eliminating redundant checks**
```nim
# Before (must validate at runtime)
proc transform(n: NimNode): NimNode =
  expectKind(n, nnkProcDef)
  # ... now safe to assume nnkProcDef

# After (type enforces at compile time)
proc transform(n: ProcDef): NormNode =
  # Type system guarantees nnkProcDef
  # No need for runtime checks
```

## Per-Module Implementation Guide

### Module 1: environment.nim (591 lines, ~79% adoption)

**Status**: Foundation layer - affects many downstream modules  
**Effort**: ~40 hours  
**Priority**: CRITICAL (do first)

**Key functions to update**:
- `bindEnv()` - signature uses NimNode, should use RoutineDef
- `symEnv()` - returns NimNode, should return Name or IdentDef
- `getLocal()` - returns NimNode, should return more specific type
- `pushLocal()` - takes NimNode, should be IdentDef
- All symbol binding functions - propagate type changes

**Key changes**:
1. Review all function signatures (start ~line 50)
2. Identify return types and parameter types
3. Create `asName()` and `asIdentDef()` converters if needed
4. Update transform.nim call sites (~50 affected)
5. Run tests after each major change (t70_locals.nim tests this)

**Risk**: HIGH - Many dependent modules  
**Mitigation**: Change one function at a time, test frequently

**Expected adoption after**: 85-90%

---

### Module 2: transform.nim (1,331 lines, ~89% adoption)

**Status**: Largest module, already high adoption  
**Effort**: ~35 hours  
**Priority**: HIGH (do after environment.nim)

**Key functions to update**:
- `transform()` - main transformation function
- `cpsTransform()` - entry point
- `expectCall()` - pattern matching functions
- Various helper functions handling specific AST nodes

**Key changes**:
1. Audit all remaining 39 NimNode references
2. Most should map to existing types (Statement, Expression, Call, etc.)
3. Add type narrowing where generic handling is needed
4. Update return types to be more specific
5. Use `Statement` and `Expression` types where appropriate

**Risk**: MEDIUM - Large module, but largely isolated  
**Mitigation**: Work section by section, test frequently

**Expected adoption after**: 95%+

---

### Module 3: hooks.nim (166 lines, ~70% adoption)

**Status**: Hook system, moderate complexity  
**Effort**: ~15 hours  
**Priority**: MEDIUM

**Key functions to update**:
- `cpsMagic()` - type its parameters properly
- Hook processing functions - use ProcDef instead of NimNode
- Hook matching functions - use Call for matching calls

**Key changes**:
1. Update hook-related function signatures
2. Use ProcDef and Call types
3. Simplify hook matching code with stronger types
4. Review hook creation functions (~line 80+)

**Risk**: LOW - Well-isolated module  
**Mitigation**: Hook tests (t50_hooks.nim) provide good coverage

**Expected adoption after**: 90%+

---

### Module 4: exprs.nim (797 lines, ~93% adoption)

**Status**: Already high adoption, nearly complete  
**Effort**: ~10 hours  
**Priority**: MEDIUM (quick win)

**Key functions to update**:
- Final 8 NimNode references
- Most are likely in expression flattening code
- Use Expression type for generic expression handling

**Key changes**:
1. Locate remaining 8 NimNode references
2. Most should be Expression or specific expression types
3. Consolidate patterns where multiple functions handle similar cases
4. Add documentation about expression flattening invariants

**Risk**: LOW - Nearly complete  
**Mitigation**: Excellent test coverage (t90_exprs*.nim)

**Expected adoption after**: 98%+

---

### Module 5: callbacks.nim (198 lines, ~72% adoption)

**Status**: Moderate, lower adoption  
**Effort**: ~15 hours  
**Priority**: MEDIUM

**Key functions to update**:
- Callback processing functions
- Callback creation functions
- Use Call for callback handling
- Use ProcDef for callback function definitions

**Key changes**:
1. Update callback function signatures
2. Use Call for matching callback calls
3. Use ProcDef for callback definitions
4. Review transformation logic for callbacks

**Risk**: MEDIUM - Callbacks are moderately complex  
**Mitigation**: Tests in t85_callbacks.nim

**Expected adoption after**: 85%+

---

### Module 6: defers.nim (106 lines, ~60% adoption)

**Status**: Smallest, lowest adoption  
**Effort**: ~10 hours  
**Priority**: LOW (do last, smallest)

**Key functions to update**:
- Defer handling functions
- All function signatures
- Use RoutineDef for routine handling
- Use Statement for defer statements

**Key changes**:
1. Update all function signatures
2. Use strong types throughout
3. Simplify defer handling with better types
4. Document defer transformation invariants

**Risk**: LOW - Smallest module, isolated  
**Mitigation**: Should have good test coverage

**Expected adoption after**: 95%+

---

### Module 7: returns.nim (114 lines, ~88% adoption)

**Status**: High adoption, nearly complete  
**Effort**: ~8 hours  
**Priority**: LOW (quick, already mostly done)

**Key functions to update**:
- Return handling functions
- Type return values properly
- Use Expression for return expressions

**Key changes**:
1. Final pass on remaining NimNode references
2. Ensure all return handling uses strong types
3. Document return transformation invariants

**Risk**: LOW - Very high adoption already  
**Mitigation**: Tests cover return handling

**Expected adoption after**: 98%+

---

## Cross-Cutting Concerns

### Pattern 1: Converter Functions

Add to ast.nim when needed:
```nim
proc asStatement*(n: NormNode): Statement =
  ## Narrow a NormNode to a statement
  result = n.Statement

proc asExpression*(n: NormNode): Expression =
  ## Narrow a NormNode to an expression
  result = n.Expression
```

### Pattern 2: Dual-Mode Functions

For functions that need both old and new style:
```nim
# Old style (for compatibility during migration)
proc transform*(n: NimNode): NimNode = ...

# New style (preferred)
proc transform*(n: Statement): Statement = ...

# Both can coexist briefly during migration
```

### Pattern 3: Eliminating .NimNode Conversions

Before:
```nim
let n: TypeExpr = ...
let raw = n.NimNode  # Lost type information!
```

After:
```nim
let n: TypeExpr = ...
# Work with n directly, no conversion needed
```

## Testing Strategy

### Test Frequency
- Run smoke tests after each function update
- Run module-specific tests after each module
- Full test suite before committing

### Test Commands
```bash
# Quick feedback
balls tests/t00_smoke.nim --define:debug

# Module-specific
balls tests/t70_locals.nim --define:debug       # environment.nim
balls tests/t50_hooks.nim --define:debug         # hooks.nim
balls tests/t90_exprs0.nim --define:debug        # exprs.nim
balls tests/t85_callbacks.nim --define:debug     # callbacks.nim

# Full suite (slow but thorough)
balls --define:debug
```

### Success Criteria
- ✅ All tests pass (green status)
- ✅ No new test failures
- ✅ Code compiles cleanly
- ✅ Adoption percentage increases toward >85%

## Code Review Checkpoints

Request code review at these points:

1. **After environment.nim** (highest risk)
   - Validates architecture for other modules
   - Most dependent code

2. **After transform.nim** (if changes are substantial)
   - Largest refactoring
   - Most complex transformations

3. **After final module** (before Phase 2)
   - Verify overall adoption metrics
   - Check for any missed patterns

## Common Pitfalls and How to Avoid Them

### Pitfall 1: Over-generalizing types
**Problem**: Using `Statement` for everything  
**Solution**: Use specific types (IdentDef, TypeExpr, Call, etc.) when possible

### Pitfall 2: Forgetting call site updates
**Problem**: Changing a function signature but not its callers  
**Solution**: Use compiler to find all call sites, update systematically

### Pitfall 3: Breaking tests
**Problem**: Changing signatures without running tests  
**Solution**: Run tests after EVERY significant change, not just at the end

### Pitfall 4: Losing type information
**Problem**: Calling `.NimNode` to extract raw node  
**Solution**: Work with strong types directly, only convert at boundaries

### Pitfall 5: Inconsistent type usage
**Problem**: Some functions use strong types, others use NimNode  
**Solution**: Be consistent within a module, convert systematically

## Metrics Tracking

### Track These Metrics
For each module:
- NimNode reference count (should decrease)
- ast.nim adoption percentage (should increase)
- Lines of code (may decrease due to eliminated checks)
- Compilation time (may improve due to better type narrowing)

### Comparison Points
- **Before**: From METRICS_BASELINE.md
- **After Phase 1**: Should show clear improvements
- **Phase 2 target**: >90% adoption, faster compilation, clearer intent

## Deliverables

After Phase 1 completion, we should have:

1. ✅ All modules with >85% ast.nim adoption (target >90%)
2. ✅ Zero raw `.NimNode` conversions except at critical boundaries
3. ✅ Updated METRICS_BASELINE.md showing improvements
4. ✅ All tests passing continuously
5. ✅ Code review approval from architects
6. ✅ Comprehensive knowledge of codebase for Phase 2 redesign

## Timeline Estimate

| Module | Hours | Complexity | Order |
|--------|-------|-----------|-------|
| environment.nim | 40 | HIGH | 1 |
| transform.nim | 35 | HIGH | 2 |
| hooks.nim | 15 | MEDIUM | 3 |
| exprs.nim | 10 | LOW | 4 |
| callbacks.nim | 15 | MEDIUM | 5 |
| defers.nim | 10 | LOW | 6 |
| returns.nim | 8 | LOW | 7 |
| **TOTAL** | **133** | | |
| Buffer (15%) | 20 | | |
| **WITH BUFFER** | **153** | | |

**Expected completion**: Week 3-4 of Phase 1 (~40 hours/week)

## Next Steps

1. ✅ **Phase 0**: Complete (you are here)
2. ⏳ **Phase 1 Start**: environment.nim module migration
3. ⏳ **Phase 1 Progression**: Follow module order above
4. ⏳ **Phase 1 Complete**: All modules >85% adoption
5. ⏳ **Code Review**: Verify quality and completeness
6. ⏳ **Phase 2**: Complete redesign with prepared knowledge

---

## Questions & Support

For questions during implementation:
1. Check patterns in this guide
2. Look at similar code already converted
3. Run tests frequently to validate changes
4. Request code review early for risky changes

Remember: **Tests must always pass!** This is non-negotiable for code quality.
