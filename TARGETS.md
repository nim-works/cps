# Typed AST Return Type Conversion Targets

This file tracks all functions returning NimNode or NormNode that should be converted to specific AST types.

## Status
- **Total Functions**: 160
- **NimNode returns**: 52 (candidates for NormNode/typed conversion)
- **NormNode returns**: 108 (candidates for specific type conversion)

## Conversion Strategy

### Priority 1: Easy NimNode → NormNode conversions
These are utility functions that should safely convert to NormNode:
- environment.nim: letOrVar, getFieldViaLocal
- environment.nim: star
- callbacks.nim: createCallback, createCastCallback, rewriteCalls, recall, performUntypedPass
- hooks.nim: makeLineInfo (template - harder)
- ast.nim: getPragmaName, kind (multiple)
- rewrites.nim: filter (NimNode version)

### Priority 2: NormNode → Specific Type conversions
These should convert to more specific AST types like Statement, Expression, Call, etc:

#### Statement-returning functions (nnkStmtList, nnkStmtListExpr, etc.)
- returns.nim: makeReturn, terminator, tailCall, jumperCall
- defers.nim: rewriteDefer
- exprs.nim: addConv, addDiscard, addReturn, addRaise

#### Call-returning functions (function calls)
- rewrites.nim: childCallToRecoverResult, resym
- environment.nim: rewriteResultReturn

#### Expression-returning functions (nnkCall, nnkIdent, etc.)
- ast.nim: findChild, getImpl
- exprs.nim: assign, assignTo

#### ProcDef-returning functions
- callbacks.nim: cpsCallbackTypeDef (needs investigation)

## Completed Conversions ✅

## Failed Conversions (LOSERS) ❌

- **newStmtList** (ast.nim:439) - Too specific as Statement, breaks add() function
- **copyOrVoid** (spec.nim:618) - Converting to TypeExpr breaks downstream usage

## NormNode Conversions (NORMALS) 🔵

(See NORMALS.md for phase 10 - 7 functions already converted)

## Functions to Review

### ast.nim
- [x] **copy** (line 225) - Already returns NormNode ✓
- [x] **copyNimNode** (line 226) - Already returns NormNode ✓
- [x] **copyNimTree** (line 227) - Already returns NormNode ✓
- [x] **newEmptyNormNode** (line 313) - Already returns NormNode ✓
- [x] **kind** (line 355) - Returns NimNodeKind, correct ✓
- [x] **add** (line 358) - Already returns NormNode ✓
- [x] **add** (line 365) - Already returns NormNode ✓
- [x] **findChild** (line 370) - Already returns NormNode ✓
- [x] **getImpl** (line 386) - Already returns NormNode ✓
- [x] **getPragmaName** (line 419) - LOSER: Deprecated function
- [x] **resym** (line 539) - Already returns NormNode ✓
- [x] **kind** (line 829) - Returns NimNodeKind, correct ✓
- [x] **expr** (line 1138) - LOSER: Indexes Conv node, type varies
- [x] **body** (line 1162) - LOSER: Statement too specific

### callbacks.nim
- [x] **cpsCallbackTypeDef** (line 26) - Already returns NormNode ✓
- [x] **createCallback** (line 49) - LOSER: Breaks macro context (whelp)
- [x] **createCastCallback** (line 70) - LOSER: Breaks macro context (whelp)
- [x] **rewriteCalls** (line 185) - LOSER: In `when false` block, unused
- [x] **recall** (line 188) - LOSER: In `when false` block, unused
- [x] **performUntypedPass** (line 198) - LOSER: In `when false` block, unused

### defers.nim
- [x] **rewriteDefer** (line 64) - LOSER: NormNode is correct, Statement breaks
- [x] **rewriter** (line 67) - LOSER: Internal nested function

### environment.nim
- [x] **maybeConvertToRoot** (line 89) - Already returns NormNode ✓
- [x] **objectType** (line 133) - LOSER: TypeExpr too specific
- [x] **letOrVar** (line 281) - Returns NimNodeKind, correct ✓
- [x] **rewriteSymbolsIntoEnvDotField** (line 389) - Already returns NormNode ✓
- [x] **createContinuation** (line 402) - Already returns NormNode ✓
- [x] **resultdot** (line 405) - LOSER: Nested function
- [x] **genException** (line 421) - Already returns NormNode ✓
- [x] **createRecover** (line 430) - Already returns NormNode ✓
- [x] **star** (line 459) - LOSER: Nested function
- [x] **rewriteVoodoo** (line 583) - Already returns NormNode ✓
- [x] **voodoo** (line 586) - Nested function, already returns NormNode ✓

### exprs.nim
- [x] **newCpsMustLift** (line 7) - Already returns NormNode ✓
- [x] **rewriteElifOf** (line 55) - Already returns NormNode ✓
- [x] **assignTo** (line 169) - Already returns NormNode ✓
- [x] **assign** (line 173) - Already returns NormNode ✓
- [x] **addConv** (line 326) - Already returns NormNode ✓
- [x] **addDiscard** (line 334) - Already returns NormNode ✓
- [x] **addReturn** (line 342) - Already returns NormNode ✓
- [x] **addRaise** (line 350) - Already returns NormNode ✓
- [x] **lift** (line 359) - Already returns NormNode ✓
- [x] **lifter** (line 361) - Already returns NormNode ✓
- [x] **annotate** (line 394) - Already returns NormNode ✓

### hooks.nim
- [x] **makeLineInfo** (line 54) - Template returning NimNode, correct ✓
- [x] **abbreviation** (line 67) - Already returns NormNode ✓
- [x] **entrace** (line 90) - Template already returns NormNode ✓
- [x] **hook** (line 104) - Already returns NormNode ✓
- [x] **hook** (line 115) - Already returns NormNode ✓
- [x] **initFrame** (line 156) - Already returns NormNode ✓
- [x] **updateLineInfoForContinuationStackFrame** (line 163) - Already returns Statement ✓

### returns.nim
- [x] **firstReturn** (line 5) - LOSER: Statement too specific
- [x] **makeReturn** (line 23) - Already returns NormNode ✓
- [x] **makeReturn** (line 37) - Already returns NormNode ✓
- [x] **terminator** (line 61) - Already returns NormNode ✓
- [x] **tailCall** (line 90) - Already returns NormNode ✓
- [x] **jumperCall** (line 103) - Already returns NormNode ✓

### rewrites.nim
- [x] **filter** (line 24) - Phase 10.2: Converted NimNode → NormNode ✓
- [x] **filter** (line 34) - Already returns NormNode ✓
- [x] **filter** (line 40) - Already returns NormNode ✓
- [x] **errorAst** (line 50) - Already returns NormNode ✓
- [x] **errorAst** (line 60) - Already returns NormNode ✓
- [x] **desym** (line 65) - Phase 10.1: Converted NimNode → NormNode ✓
- [x] **desym** (line 71) - Already returns NormNode ✓
- [x] **childCallToRecoverResult** (line 74) - Phase 10.5: Converted NimNode → NormNode ✓
- [x] **childCallToRecoverResult** (line 87) - Already returns NormNode ✓
- [x] **resym** (line 90) - Phase 10.6: Converted NimNode → NormNode ✓
- [x] **resym** (line 108) - Already returns NormNode ✓
- [x] **replacedSymsWithIdents** (line 111) - Phase 10.7: Converted NimNode → NormNode ✓
- [x] **replacedSymsWithIdents** (line 120) - Already returns NormNode ✓
- [ ] **isCallback** (line 123) - Returns bool, skip
- [x] **normalizingRewrites** (line 156) - Already returns NormNode ✓
- [x] **replace** (line 490) - Already returns NormNode ✓
- [x] **replace** (line 501) - Already returns NormNode ✓
- [x] **replace** (line 510) - Template already returns NimNode ✓
- [x] **replace** (line 515) - Template already returns NormNode ✓
- [x] **multiReplace** (line 520) - Already returns NormNode ✓
- [x] **multiReplace** (line 535) - Already returns NormNode ✓
- [x] **addInitializationToDefault** (line 565) - Phase 11.1: Converted NimNode → NormNode ✓

### spec.nim
- [x] **nilAsEmpty** (line 597) - Already returns NormNode ✓
- [x] **emptyAsNil** (line 604) - Already returns NormNode ✓
- [x] **copyOrVoid** (line 618) - LOSER: TypeExpr too specific
- [x] **bootstrapSymbol** (line 373) - Already returns NormNode ✓
- [x] **enbasen** (line 395) - Already returns TypeExpr ✓
- [x] **makeErrorShim** (line 418) - LOSER: Macro context issues
- [ ] **hash** (line 178) - Returns Hash, skip

### transform.nim
- (Many complex transformation functions that need detailed analysis)

## Next Step
Start with Priority 1 conversions, then move to Priority 2 after getting some wins.
