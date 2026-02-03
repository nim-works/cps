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

## NormNode Conversions (NORMALS) 🔵

## Functions to Review

### ast.nim
- [ ] **copy** (line 225) - Creates normalized copy, should return NormNode ✓
- [ ] **copyNimNode** (line 226) - Borrows from copy, returns NormNode ✓
- [ ] **copyNimTree** (line 227) - Full tree copy, returns NormNode ✓
- [ ] **newEmptyNormNode** (line 313) - Creates empty node, returns NormNode ✓
- [ ] **kind** (line 355) - Getter function, might be ok as NimNode (borrowed)
- [ ] **add** (line 358) - Adds children, returns NormNode ✓
- [ ] **add** (line 365) - Adds NormNode children, returns NormNode ✓
- [ ] **findChild** (line 370) - Template search, could return Expression or Statement
- [ ] **getImpl** (line 386) - Gets implementation, could be ProcDef or Expression
- [ ] **getPragmaName** (line 419) - Gets pragma, could be Name or Expression
- [ ] **newStmtList** (line 439) - Creates stmt list, should return Statement
- [ ] **newTree** (line 445) - Creates tree, varies by kind
- [ ] **wrap** (line 467) - Wraps expression, could be Expression
- [ ] **resym** (line 539) - Rewrites symbols, should return Statement or Expression
- [ ] **dot** (line 602) - Creates dot expr, should return Expression
- [ ] **dot** (line 608) - Creates dot expr, should return Expression
- [ ] **val** (line 746) - Extracts value, could be Expression or TypeExpr
- [ ] **val** (line 818) - Extracts value, could be Expression or TypeExpr
- [ ] **kind** (line 829) - Getter, borrowed from NimNode
- [ ] **expr** (line 1138) - Creates expression, should return Expression
- [ ] **body** (line 1162) - Returns body, should return Statement

### callbacks.nim
- [ ] **cpsCallbackTypeDef** (line 26) - Creates type def, should return TypeExpr or Call
- [ ] **createCallback** (line 49) - Creates callback, returns Call (construction)
- [ ] **createCastCallback** (line 70) - Creates cast callback, returns Call
- [ ] **rewriteCalls** (line 185) - Should return NormNode or Statement
- [ ] **recall** (line 188) - Should return NormNode or Expression
- [ ] **performUntypedPass** (line 198) - Should return ProcDef

### defers.nim
- [ ] **rewriteDefer** (line 64) - Should return Statement
- [ ] **rewriter** (line 67) - Should return Statement or NormNode

### environment.nim
- [ ] **maybeConvertToRoot** (line 89) - Should return Expression or TypeExpr
- [ ] **objectType** (line 133) - Should return TypeExpr
- [ ] **getResult** (line 178) - Should return Expression
- [ ] **initialization** (line 272) - Should return Expression
- [ ] **letOrVar** (line 281) - Should return Statement (var/let section)
- [ ] **addAssignment** (line 291) - Should return Statement
- [ ] **addAssignment** (line 299) - Should return Statement
- [ ] **getFieldViaLocal** (line 305) - Should return Expression or Name
- [ ] **rewriteResultReturn** (line 354) - Should return Statement
- [ ] **rewriter** (line 356) - Should return Statement or NormNode
- [ ] **rewriteSymbolsIntoEnvDotField** (line 389) - Should return NormNode or Statement
- [ ] **createContinuation** (line 402) - Should return Statement ✓ (already is)
- [ ] **resultdot** (line 405) - Should return Expression
- [ ] **genException** (line 421) - Should return Statement or NormNode
- [ ] **createRecover** (line 430) - Should return Statement or NormNode
- [ ] **star** (line 459) - Should return NimNode (utility)
- [ ] **rewriteVoodoo** (line 583) - Should return Statement or NormNode
- [ ] **voodoo** (line 586) - Should return Statement or NormNode

### exprs.nim
- [ ] **newCpsMustLift** (line 7) - Should return Statement
- [ ] **rewriteElifOf** (line 55) - Should return Statement
- [ ] **assignTo** (line 169) - Should return Statement or Expression
- [ ] **assign** (line 173) - Should return Statement
- [ ] **addConv** (line 326) - Should return Statement
- [ ] **addDiscard** (line 334) - Should return Statement
- [ ] **addReturn** (line 342) - Should return Statement
- [ ] **addRaise** (line 350) - Should return Statement
- [ ] **lift** (line 359) - Should return Statement or NormNode
- [ ] **lifter** (line 361) - Should return Statement or NormNode
- [ ] **annotate** (line 394) - Should return Expression or NormNode

### hooks.nim
- [ ] **makeLineInfo** (line 54) - Template, returns NimNode (tricky)
- [ ] **abbreviation** (line 67) - Should return Name or Expression ✓ (already is NormNode)
- [ ] **entrace** (line 90) - Template, should return NormNode (already is)
- [ ] **entrace** (line 95) - Template, should return NormNode (already is)
- [ ] **hook** (line 104) - Should return Statement or NormNode
- [ ] **hook** (line 115) - Should return Statement or NormNode
- [ ] **initFrame** (line 156) - Should return Statement or NormNode
- [ ] **updateLineInfoForContinuationStackFrame** (line 163) - Should return Statement ✓ (already is)

### returns.nim
- [ ] **firstReturn** (line 5) - Should return Statement or Expression
- [ ] **makeReturn** (line 23) - Should return Statement
- [ ] **makeReturn** (line 37) - Should return Statement
- [ ] **terminator** (line 61) - Should return Statement
- [ ] **tailCall** (line 90) - Should return Statement
- [ ] **jumperCall** (line 103) - Should return Statement

### rewrites.nim
- [ ] **filter** (line 24) - Should return NormNode (utility)
- [ ] **filter** (line 34) - Already returns NormNode ✓
- [ ] **filter** (line 40) - Already returns NormNode ✓
- [ ] **errorAst** (line 50) - Already returns NormNode ✓
- [ ] **errorAst** (line 60) - Already returns NormNode ✓
- [ ] **desym** (line 65) - Should return NormNode or Name
- [ ] **desym** (line 71) - Already returns NormNode ✓
- [ ] **childCallToRecoverResult** (line 74) - Should return Expression or NormNode
- [ ] **childCallToRecoverResult** (line 87) - Already returns NormNode ✓
- [ ] **resym** (line 90) - Should return NormNode or Statement
- [ ] **resym** (line 108) - Already returns NormNode ✓
- [ ] **replacedSymsWithIdents** (line 111) - Should return NormNode
- [ ] **replacedSymsWithIdents** (line 120) - Already returns NormNode ✓
- [ ] **isCallback** (line 123) - Returns bool, skip
- [ ] **normalizingRewrites** (line 156) - Already returns NormNode ✓
- [ ] **workaroundRewrites** (line 386) - Already returns NormNode ✓
- [ ] **workaroundRewrites** (line 483) - Already returns NormNode ✓
- [ ] **replace** (line 486) - Should return NormNode
- [ ] **replace** (line 497) - Already returns NormNode ✓
- [ ] **multiReplace** (line 516) - Should return NormNode
- [ ] **multiReplace** (line 531) - Already returns NormNode ✓
- [ ] **multiReplace** (line 546) - Already returns NormNode ✓
- [ ] **addInitializationToDefault** (line 561) - Should return NormNode ✓

### spec.nim
- [ ] **nilAsEmpty** (line 597) - Should return Expression or NormNode
- [ ] **emptyAsNil** (line 604) - Should return Expression or NormNode
- [ ] **copyOrVoid** (line 618) - Should return Expression or TypeExpr
- [ ] **bootstrapSymbol** (line 373) - Should return Name or Expression
- [ ] **enbasen** (line 395) - Should return TypeExpr
- [ ] **makeErrorShim** (line 418) - Should return ProcDef
- [ ] **hash** (line 178) - Returns Hash, skip

### transform.nim
- (Many complex transformation functions that need detailed analysis)

## Next Step
Start with Priority 1 conversions, then move to Priority 2 after getting some wins.
