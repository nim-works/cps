# CPS Metrics Baseline - Phase 0 (2026-02-03)

## Summary
**Date**: 2026-02-03  
**Phase**: Phase 0 - Foundation  
**Status**: Completed ast.nim foundation (rename + type additions)  

## Codebase Size

### Core CPS Modules
| Module | Lines | Role |
|--------|-------|------|
| ast.nim | 1,257 | Type definitions and AST utilities |
| transform.nim | 1,331 | Main CPS transformation logic |
| exprs.nim | 797 | Expression flattening |
| spec.nim | 623 | CPS specification |
| environment.nim | 591 | Symbol environment management |
| rewrites.nim | 565 | AST rewriting utilities |
| callbacks.nim | 198 | Callback handling |
| hooks.nim | 166 | Hook system |
| returns.nim | 114 | Return value handling |
| defers.nim | 106 | Defer statement handling |
| help.nim | 75 | Help utilities |
| cubby.nim | 60 | Temp storage utilities |
| **TOTAL** | **5,883** | |

### Test Suite
- **Test Files**: 40 (including skipped variants)
- **Test Lines**: 6,878
- **Test-to-Code Ratio**: 1.17x (tests are larger than code!)
- **Key Test Files**:
  - t00_smoke.nim - smoke tests
  - t10_loops_skipped.nim - loop constructs
  - t50_hooks.nim - hook system
  - t70_locals.nim - local variables
  - t80_try1/t80_try2.nim - exception handling
  - t90_exprs*.nim - expression flattening (5 files)

## ast.nim Adoption Baseline

Current usage of ast.nim distinct types vs raw NimNode:

| Module | NimNode refs | NormNode-types refs | Adoption % |
|--------|--------------|---------------------|-----------|
| transform.nim | 39 | 304 | **~89%** |
| environment.nim | 38 | 146 | **~79%** |
| callbacks.nim | 26 | 67 | **~72%** |
| exprs.nim | 8 | 110 | **~93%** |
| hooks.nim | 14 | 33 | **~70%** |
| returns.nim | 5 | 37 | **~88%** |
| defers.nim | 8 | 12 | **~60%** |
| **Average** | | | **~79%** |

> **Note**: Percentages estimated from search counts. Actual adoption depends on context and call sites.

## Key Metrics Established

### Lines of Code Distribution
- **Top 3 modules**: transform.nim (1,331), ast.nim (1,257), exprs.nim (797)
- **Bottom 3 modules**: cubby.nim (60), help.nim (75), defers.nim (106)
- **Ratio (largest/smallest)**: 22:1 (shows significant variation in module complexity)

### Type System Baseline
- **Total distinct types in ast.nim**: 48+ (Name, Ident, Sym, DotExpr, TypeExpr, TypeExprObj, TypeExprRef, IdentDef, RoutineDef, ProcDef, FormalParams, RoutineParam, Call, CallKind, Conv, Pragma, PragmaStmt, PragmaBlock, PragmaExpr, PragmaAtom, TypeSection, TypeDef, **Statement (NEW)**, **Expression (NEW)**, VarLet, VarLetTuple, VarLetIdentDef, LetSection, VarSection, VarIdentDef, LetIdentDef, DefVarLet, IdentDefVarLet, TupleDefVarLet, IdentDefLet, IdentDefVar, TypeExprLike, ...)

### Test Coverage Baseline
- All smoke tests pass ✅
- Loop tests pass ✅
- Hook tests pass ✅
- 40 test files total
- No known failing tests in Phase 0

## Compilation Time Baseline

**Test mode** (t00_smoke + t10_loops_skipped + t50_hooks):
- Arc GC: ~2.8-4.2s per test
- Orc GC: ~3.1-4.2s per test
- Total parallel: ~5-6s for all three

> Note: Times are for `--define:debug` mode. Danger/optimized builds will be slower.

## Phase 0 Completion Checklist

✅ Renamed normalizedast.nim → ast.nim  
✅ Updated all imports (13 files)  
✅ Added Statement and Expression types  
✅ All tests pass  
✅ Established metrics baseline (this document)  
⏳ Create Phase 1 implementation guide (next)  

## Next Steps (Phase 1)

**Goal**: Achieve >85% ast.nim adoption across all modules  
**Priority order**:
1. **environment.nim** (591 lines) - foundation layer, high reuse
2. **transform.nim** (1,331 lines) - largest module, main transformation logic
3. **hooks.nim** (166 lines) - hook system, moderate complexity
4. **exprs.nim** (797 lines) - expression handling, large
5. **callbacks.nim** (198 lines) - callback handling, moderate
6. **defers.nim** (106 lines) - defer handling, small
7. **returns.nim** (114 lines) - return handling, small

**Expected improvements**:
- Adoption: 79% → >85% (target minimum)
- Type safety: Reduce raw NimNode usage to critical genAst boundaries only
- Code clarity: Stronger typing makes intent clearer
- Maintainability: Centralized AST handling in ast.nim

---

## Reference: Distinct Types in ast.nim

**Summary types** (opaque sums):
- `Name` = Ident | Sym
- `TypeExpr` = various type nodes
- `Pragma` = PragmaStmt | PragmaBlock | PragmaExpr
- `VarLet` = var or let section
- `Call` = any call node variant
- **NEW**: `Statement` = any statement node
- **NEW**: `Expression` = any expression node

**Specialized types** (narrow to single kind):
- `ProcDef` - normalized proc definition
- `FormalParams` - formal parameters
- `IdentDef` - single identifier definition
- `TypeSection` - type section
- `TypeDef` - type definition
- And many more...

> Total: 48+ distinct types providing semantic grammar on top of Nim's syntactic AST.
