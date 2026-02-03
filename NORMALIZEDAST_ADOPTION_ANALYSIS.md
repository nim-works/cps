# Complete Adoption Analysis: normalizedast.nim to ast.nim

**Document Date:** February 2, 2026  
**Analysis Focus:** Complete adoption of NormNode-based AST abstraction across CPS

---

## Executive Summary

**Current Adoption:** ~45-50% of AST handling uses NormNode/specialized types  
**Estimated Effort to Complete:** 3-4 weeks (40-50 developer hours)  
**Renaming to ast.nim:** **RECOMMENDED** - Would signal architectural primacy

The normalizedast module provides excellent semantic abstraction but adoption is **incomplete and inconsistent**. Critical modules like `environment.nim` and `transform.nim` return bare `NimNode` from public APIs, creating an impedance mismatch. The module's design is sound, but the boundary is porous.

---

## 1. CURRENT STATE - What normalizedast Does Well

### 1.1 Strong Abstraction Design

**File:** `/home/adavidoff/git/cps/cps/normalizedast.nim` (1,250 lines)

#### Semantic Typing System
The module provides **distinct types** that encode semantic meaning into type system:

```nim
type
  Name* = distinct NormNode        # nnkIdent | nnkSym
  Ident* = distinct Name           # specifically nnkIdent
  Sym* = distinct Name             # specifically nnkSym
  
  TypeExpr* = distinct NormNode    # type expressions (can be empty)
  TypeExprObj* = distinct TypeExpr # specifically nnkObjectTy
  TypeExprRef* = distinct TypeExpr # specifically nnkRefTy
  
  IdentDef* = distinct NormNode    # normalized nnkIdentDefs (always 3 children)
  RoutineParam* = distinct IdentDef # routine parameter variant
  
  Call* = distinct NormNode        # opaque over CallNodes
  Conv* = distinct NormNode        # nnkConv conversion
  
  VarLet* = distinct NormNode      # nnkLetSection | nnkVarSection
  VarLetIdentDef* = distinct VarLet # with single IdentDef
  VarLetTuple* = distinct VarLet   # with VarTuple
  
  ProcDef* = distinct RoutineDef   # normalized proc definition
```

**Design Philosophy (lines 20-71):**
- Codifies **pre/post-conditions** of CPS code
- Maintains **normalization invariants** from rewrites module
- Separates **syntax grammar** (NimNode) from **semantic grammar** (distinct types)
- Uses opaque types to work around Nim's lack of sum types

#### Invariant Maintenance Strategies

1. **asXXX conversions** (lines 286-302) - validate and coerce or error
   - `asName(NormNode): Name` - ensures nnkIdent or nnkSym
   - `asIdentDefs(NimNode): IdentDef` - validates 3-child structure
   - `asVarLet(NimNode): VarLet` - ensures normalized var/let
   - `asProcDef(NormNode): ProcDef` - ensures nnkProcDef

2. **Dedicated constructors** - build only valid states
   - `newIdentDef(n: Name, t: TypeExprLike, val: NormNode): IdentDef`
   - `newProcDef(name: Name, retType: TypeExpr, ...): ProcDef`
   - `newVarSection(name: Name, typ: TypeExprLike, val): VarSection`

3. **Type-restricted operations** - dispatch via type system
   - `name*(n: IdentDefLike): Name` - extract name from various contexts
   - `typ*(n: DefLike): TypeExpr` - unified type accessor
   - `val*(n: VarLetLike): NormNode` - unified value accessor

### 1.2 Comprehensive Type Ecosystem

**Coverage by Category:**

| Category | Types | Example Functions |
|----------|-------|-------------------|
| **Names** | Name, Ident, Sym | `genSymVar`, `desym`, `asName` |
| **Types** | TypeExpr, TypeExprObj, TypeExprRef | `newRefType`, `sinkAnnotated` |
| **Defs** | IdentDef, RoutineParam, FormalParams | `newIdentDef`, `firstCallParam` |
| **Var/Let** | VarLet, VarLetIdentDef, VarLetTuple, LetSection, VarSection | `asVarLetTuple`, `clone`, `inferTypFromImpl` |
| **Routines** | RoutineDef, ProcDef | `newProcDef`, `body=`, `callingParams` |
| **Calls** | Call, CallKind | `newCall`, `hasImpl`, `prependArg` |
| **Pragmas** | Pragma, PragmaStmt, PragmaBlock, PragmaExpr, PragmaAtom | `newPragmaStmt`, `hasPragma`, `addPragma` |
| **Type Sections** | TypeSection, TypeDef | `asTypeSection`, `asTypeDef` |

### 1.3 Well-Adopted Modules

**Strong normalizedast usage:**

1. **defers.nim** (8 imports, consistent)
   - Works almost entirely with NormNode
   - Uses `findTree`, `filterRecursive` patterns
   - Demonstrates idiomatic usage

2. **returns.nim** (10 imports)
   - Heavy use of type abstractions
   - `firstReturn` works with NormNode
   - Follows the semantic grammar pattern

3. **hooks.nim** (11 imports)
   - `introduce` works with NormNode and Hook enum
   - Pragma-heavy, uses `hasPragma` extensively
   - Good integration

4. **exprs.nim** (25 imports)
   - Sophisticated expression filtering
   - Uses TypeExpr, NormNode, VarLet abstractions
   - Shows how complex AST work should look

5. **spec.nim** (32 imports)
   - Moderate usage of types
   - Has both NimNode and NormNode in APIs
   - Shows mixed pattern

**Export coverage:**
```nim
# From normalizedast.nim:
export NormNode, NormalCallNodes  # (line 18)
```

### 1.4 Design Strengths

| Strength | Impact | Example |
|----------|--------|---------|
| **Syntax → Semantics** | Encodes meaning in types, catches misuse at compile time | Can't pass nnkProcDef to code expecting Call |
| **Opaque Sum Types** | Handles Nim's lack of tagged unions elegantly | Name abstracts over nnkIdent and nnkSym |
| **Conversion Gating** | Makes invariant violations impossible | Must call `asIdentDefs` which validates 3-child structure |
| **Unified Accessors** | Works across type variants via typeclasses | `name()`, `typ()`, `val()` work on IdentDefLike |
| **Borrowing** | Reduces type coercion spam | `.borrow.` on copy, copyNimNode, copyNimTree |
| **Downgrades** | Allows "widening" to less specific types safely | RoutineParam → IdentDef → NormNode |

---

## 2. INCOMPLETE ADOPTION - Where It Falls Short

### 2.1 Fragmented Adoption by Module

**Adoption Scorecard:**

```
Module              NimNode Uses  NormNode Uses  Adoption %  Type
─────────────────────────────────────────────────────────────────
normalizedast       ~30           119            100%         Pure NormNode
rewrites            ~75           33             30%          Mixed
environment         ~31           20             40%          CRITICAL
transform           ~31           68             70%          HIGH
exprs                ~8            25             75%          GOOD
spec                ~23            32             60%          MIXED
callbacks            ~15            2              12%         POOR
defers                ~8            8             50%          WEAK
hooks                ~11           11             50%          MIXED
returns              ~5            10             70%          GOOD
```

**Key Insight:** Three modules account for ~50% of raw NimNode usage:
- `rewrites.nim` (75 uses) - foundational, can't fully adopt
- `environment.nim` (31 uses) - **CRITICAL GAP**
- `transform.nim` (31 uses) - **CRITICAL GAP**

### 2.2 Explicit Failures: Direct NimNode Returns

#### Problem 1: environment.nim Returns Bare NimNode

**Location:** `/home/adavidoff/git/cps/cps/environment.nim` (591 lines)

Functions returning raw `NimNode` from public API:

```nim
# Line 133
proc objectType(e: Env): NimNode =
  ## SHOULD BE: TypeExpr or TypeSection or dedicated type

# Line 168  
proc makeType*(e: Env): NimNode =
  ## SHOULD BE: TypeSection or similar
  
# Line 272
proc initialization(e: Env; field: Name, section: VarLetIdentDef): NimNode =
  ## SHOULD BE: This returns initializer code - what type?

# Line 291
proc addAssignment(e: var Env; d: IdentDef): NimNode =
  ## SHOULD BE: Could be Call | statement

# Line 299
proc addAssignment(e: var Env; section: VarLetIdentDef): NimNode =
  ## SHOULD BE: Same issue

# Line 402
proc createContinuation*(e: Env; name: Name; goto: NimNode): NimNode =
  ## NOTE: Takes NimNode as input, returns NimNode
  
# Line 420
proc genException*(e: var Env): NimNode =
  ## What does this return semantically?

# Line 429
proc createRecover*(env: Env, exported = false): NimNode =
  ## Likely a statement or proc def
```

**Impact:**
- Callers in `transform.nim` must work with untyped NimNode
- No compile-time checking of result usage
- 20+ call sites must handle `NimNode` results

**Example misuse at line 445:**
```nim
genProcName(procedure env, "recover", info=env.store.NormNode).NimNode
# ⬆️ CONVERSION SPAM: NormNode → NimNode → NormNode
```

#### Problem 2: transform.nim Construction Pattern

**Location:** `/home/adavidoff/git/cps/cps/transform.nim` (1,331 lines)

Multiple NimNode construction sites that bypass NormNode abstractions:

```nim
# Line 26 - genAstOpt usage
genAstOpt({}, contParam = contParam.NimNode):
  # Working with raw NimNode from inside genAst block
  ...

# Line 78-92 - childCallName analyzes NimNode directly
proc childCallName(n: NimNode): string =
  case n.kind
  of NormalCallNodes:
    "child " & childCallName(n[0]) & "()"
  # Should use Call abstraction

# Line 703-706 - Complex construction
genAstOpt({}, assign = assign.NimNode, tail = tail.NimNode,
          child = child.NimNode, ctype = ctype.NimNode,
          root = env.root.NimNode, identity = env.identity.NimNode,
          dealloc = Dealloc.sym.NimNode):
  # Mixing NormNode types in genAst breaks abstraction
```

**Pattern:** Code uses `genAst`/`genAstOpt` which requires bare `NimNode`, forcing conversions at boundary.

#### Problem 3: Minor Modules Avoid normalizedast

**callbacks.nim** (198 lines):
```nim
import cps/normalizedast except newTree, newStmtList
# ⬆️ Imports BUT excludes key constructors - suggests mismatch
# Only 2 NormNode uses despite heavy AST work
```

**hooks.nim** (166 lines):
```nim
# Line 96-98 - Direct NimNode manipulation
let fun = newLit(nameForNode n.NimNode)
let call = newCall(Trace.sym, event, c.NimNode, ...)
# Should use Call constructor
```

### 2.3 Boundary Friction Points

#### Friction 1: .NimNode Conversion Spam

Count of `.NimNode` extractions requiring explicit conversion:

```
File                    .NimNode Count   Impact
────────────────────────────────────────────────
environment.nim              17          High (internal APIs)
transform.nim               17          High (heavy lifting)
normalizedast.nim           87          Expected (bridges)
callbacks.nim                3          Low
hooks.nim                    3          Low
rewrites.nim                 2          Expected
```

**Example chain at environment.nim:446:**
```nim
genProcName(procedure env, "recover", info=env.store.NormNode).NimNode
           ↓ returns Name
                                              ↓ extract NimNode to use in genAst
                                                                  ↓ result is NimNode
# This could be: genProcName(...) : Name, used directly
```

#### Friction 2: Type Mismatches Requiring Workarounds

**transform.nim lines 703-706:**
```nim
# Can't pass NormNode directly to genAstOpt - must extract NimNode
genAstOpt({}, 
  assign = assign.NimNode,  # ← forced conversion
  tail = tail.NimNode,      # ← forced conversion
  ...
):
  # Inside genAst, work with raw NimNode again
```

**rewrites.nim lines 21-22:**
```nim
converter normNodeToNimNode(n: NormNode): NimNode =
  n.NimNode
# Module-scope to prevent leakage but still needed
```

#### Friction 3: Helper Functions Missing

Functions that should exist but don't:

```nim
# NOT IN normalizedast.nim:

# Convert NormNode to specific types when semantic intent is clear
proc asCall*(n: NormNode): Call  # EXISTS but line 1077
proc asCallKind*(n: NormNode): CallKind  # EXISTS but line 1076

# But these patterns aren't available:
proc newStatement*(body: NormNode): NormNode  # varies widely
proc newTypeSection*(defs: varargs[TypeDef]): TypeSection
  # ↑ environment.nim writes inline type sections

proc newCall*(f: NormNode, args: seq[NormNode]): Call
  # ↑ can't use varargs with NormNode due to converter issues
```

### 2.4 Architectural Gaps

#### Gap 1: Type Construction Coverage

Some important AST nodes don't have dedicated types:

| Missing Type | Used For | Current Pattern |
|--------------|----------|-----------------|
| Statement | code blocks | Returns bare NormNode |
| TypeSection | type definitions | `makeType` returns NimNode |
| FieldDef | object fields | Uses IdentDef (confusing) |
| ExceptionDef | exception handling | Uses bare NormNode |
| GenericParam | generic parameters | Uses IdentDef |
| Callable | callbacks | No validation |

**Example - Type sections:**
```nim
# environment.nim line 168
proc makeType*(e: Env): NimNode =
  # Returns a NimNode that contains nnkTypeSection
  # Should be:
  proc makeType*(e: Env): TypeSection =
```

#### Gap 2: Mixed Entry Points

Some functions accept both NimNode and NormNode, creating ambiguity:

```nim
# normalizedast.nim line 507
proc asName*(n: string; info: NormNode = NilNormNode): Name =

# But these also exist:
func asName*(n: NimNode): Name =     # line 489
func asNameAllowEmpty*(n: NimNode): Name =  # line 492

# Result: Callers can use either, breaking consistency
```

**Principle violation:** If something should always be normalized, accept only NormNode.

#### Gap 3: Return Type Underspecification

Functions that return NormNode but semantics are unclear:

```nim
proc initialization(e: Env; field: Name, section: VarLetIdentDef): NimNode =
  ## Is this: an assignment? a statement? an expression?
  ## Should be: NormNode | Call | specific type
  
proc addAssignment(e: var Env; d: IdentDef): NormNode =
  ## Same issue - what invariants does result satisfy?
```

---

## 3. BARRIERS TO COMPLETION

### 3.1 Top 5 Barriers (Priority Order)

#### Barrier 1: genAst/genAstOpt Boundary (CRITICAL)

**Problem:** Nim's `genAst` macro requires bare `NimNode` arguments
**Severity:** HIGH - affects transform.nim heavily
**Workaround:** Force use of `.NimNode` conversion

```nim
# transform.nim line 26
genAstOpt({}, contParam = contParam.NimNode):  # Can't pass NormNode
  # ... use raw NimNode inside
```

**Solution Options:**
1. **Wrapper layer:** Create `genNormAst` that accepts/returns NormNode
2. **Local conversion:** Accept as is, but require explicit `.NimNode` at call sites
3. **Type erasure:** Implement custom quasi-quoting

**Estimated Impact:** Would unblock ~200 lines of code

#### Barrier 2: environment.nim API Redesign (CRITICAL)

**Problem:** Multiple functions return bare NimNode without semantic type
**Severity:** HIGH - 15+ public functions affected
**Current:** Callers depend on NimNode for flexibility

Functions to redesign:
- `objectType(e: Env): NimNode` → TypeExpr? TypeSection?
- `makeType(e: Env): NimNode` → TypeSection
- `initialization(...)` → Call? (need new type)
- `addAssignment(...)` → Call? (need new type)
- `createContinuation(...)` → Call?
- `genException(...)` → Call?
- `createRecover(...)` → ProcDef

**Solution:** 
1. Define missing semantic types (Statement? Expression?)
2. Ensure functions return specific types
3. Update all ~50 call sites in transform.nim

**Estimated Effort:** 8-10 hours

#### Barrier 3: rewrites.nim's Dual Role (MODERATE)

**Problem:** rewrites.nim is both consumer and producer of NimNode
**Current State:**
```nim
type
  NormNode* = distinct NimNode  # BASE DEFINITION (line 13)
  NodeFilter* = proc(n: NimNode): NimNode
  NormalizingFilter* = proc(n: NimNode): NormNode

proc normalizingRewrites*(n: NimNode): NormNode
```

**Issue:** Must work with both normalized and non-normalized AST
- Input: raw NimNode (from compiler)
- Output: NormNode (normalized)
- Internal: Often raw NimNode during transformation

**Solution:** Accept as foundational constraint - rewrites.nim stays mixed

**Estimated Impact:** Can't fix without rewriting core transformation logic

#### Barrier 4: Conversion Function Naming (LOW)

**Problem:** Multiple `asName`, `asIdentDefs` overloads cause confusion

```nim
func asName*(n: NimNode): Name =           # line 489
func asNameAllowEmpty*(n: NimNode): Name = # line 492
proc asName*(n: string; info: NormNode): Name =  # line 507
proc asName*(n: TypeExpr): Name =          # line 566

# Which version is called?
let x = asName(something)  # Ambiguous!
```

**Solution:** Establish clear naming convention:
- `asXXX(NimNode)` → coerce, error if wrong kind
- `asXXXAllowEmpty(NimNode)` → allows nnkEmpty
- `asXXX(String)` → construct (different name?)

**Estimated Impact:** Documentation + name review (~2 hours)

#### Barrier 5: Incomplete Type Coverage (MODERATE)

**Problem:** Some important concepts lack distinct types

Missing types that should exist:

| Concept | Used Where | Frequency |
|---------|-----------|-----------|
| Statement | Code blocks | 100+ uses |
| Expression | Values | 50+ uses |
| TypeSection | Type defs | 10+ uses |
| FieldDef | Object fields | 20+ uses |

**Current Workaround:** Use base NormNode, lose semantic information

**Solution:** Add 3-4 more types to normalizedast
```nim
type
  Statement* = distinct NormNode  # nnkStmtList, nnkStmtListExpr, etc.
  Expression* = distinct NormNode # expressions with type info
```

**Estimated Impact:** Would remove ~30% of remaining NimNode uses

---

## 4. REFACTORING OPPORTUNITIES

### 4.1 Phase 1: Foundation (Weeks 1-2)

**Goal:** Establish patterns and boundaries

#### 1.1 Rename normalizedast.nim → ast.nim

```bash
mv cps/normalizedast.nim cps/ast.nim
# Update imports everywhere:
# import cps/normalizedast  →  import cps/ast
```

**Ripple locations:**
- cps.nim: line 2
- environment.nim: line 10
- transform.nim: line 4
- spec.nim: line 24
- returns.nim: line 3
- hooks.nim: line 3
- defers.nim: line 1
- exprs.nim: line 3
- callbacks.nim: line 11
- 5 test files

**Effort:** 1-2 hours (mechanical)
**Value:** Signals architectural importance

#### 1.2 Add Missing Type Categories

```nim
# In ast.nim, add:

type
  # Statements - execution blocks
  Statement* = distinct NormNode
    ## A statement or sequence of statements (nnkStmtList, etc.)
  
  # Expressions - value-producing code
  Expression* = distinct NormNode
    ## An expression that produces a value with type information
  
  # Type constructors
  FieldDef* = distinct IdentDef
    ## Object field definition (variant of IdentDef)
  
  # Callables
  Callable* = distinct NormNode
    ## A callable expression or symbol

# Add constructors:
proc newStatement*(children: NormalizedVarargs): Statement
proc newTypeSection*(defs: varargs[TypeDef]): TypeSection
```

**Effort:** 4-6 hours (design + implementation)
**Blocks:** Environment.nim redesign

#### 1.3 Create "genNormAst" Wrapper

```nim
# New helper in ast.nim for genAst/genAstOpt boundary

template genNormAst(bindings: varargs[untyped]; body: untyped): NormNode =
  ## Wrapper around genAst that accepts NormNode bindings
  ## and returns NormNode result
  let bindings {.inject.} = block:
    var result: seq[(string, NimNode)] = @[]
    # ... extract bindings ...
    result
  NormNode(genAst(body))

# Alternative: just standardize .NimNode pattern
```

**Effort:** 2-3 hours
**Value:** Document intended boundary clearly

### 4.2 Phase 2: environment.nim Redesign (Weeks 2-3)

**Goal:** Return only semantic types from public API

#### 2.1 Type Specification

For each public function returning `NimNode`:

```nim
# BEFORE:
proc objectType(e: Env): NimNode
proc makeType*(e: Env): NimNode
proc initialization(e: Env; field: Name, section: VarLetIdentDef): NimNode

# AFTER:
proc objectType(e: Env): TypeExpr
proc makeType*(e: Env): TypeSection  # or new type
proc initialization(...): Call        # or NormNode | Call

# Or create parametric return:
type CodeFragment* = distinct NormNode  # General executable code
```

#### 2.2 Update ~50 Call Sites

Location analysis:

```
transform.nim:
  Line 705: Uses makeType result in genAstOpt
  Line 445: Uses createRecover result
  
environment.nim:
  Line 305: getFieldViaLocal internal use
  Line 340: Uses objectType in construction
```

**Solution Strategy:**
1. Change function signatures
2. Update 10-15 direct callers in transform.nim
3. Update internal environment.nim calls (10 sites)
4. Update callbacks.nim uses (3 sites)

**Effort:** 8-10 hours

#### 2.3 Add Type Validators

```nim
# For environment.nim internal mutations:

proc validateObjectType(n: NormNode): TypeExpr =
  ## Ensure n is a valid object type expression
  if n.kind != nnkObjectTy:
    errorGot "expected object type", n
  n.TypeExpr
```

### 4.3 Phase 3: Call Site Updates (Week 3-4)

**Goal:** Eliminate .NimNode conversions in main code paths

#### 3.1 transform.nim Cleanup (High Impact)

**Current pattern:**
```nim
# 17 instances of .NimNode conversion
genAstOpt({}, assign = assign.NimNode, ...):
  # ...
```

**Target pattern:**
```nim
# Either:
# Option A - Use wrapper
let result = genNormAst(assign, tail):
  # ...

# Option B - Accept pattern but document
genAstOpt({}, assign = assign.NimNode):  # REQUIRED: genAst limitation
  # ... comment explaining boundary
```

**Effort:** 3-4 hours (cleanup + documentation)

#### 3.2 Callback Imports Fix

**Current:** `import cps/normalizedast except newTree, newStmtList`

**Fix:** Import all, use what's needed

**Lines:** 15-25 of callbacks.nim

**Effort:** 1 hour

#### 3.3 hooks.nim Direct Calls

**Lines:** 96-98 of hooks.nim

```nim
# BEFORE:
let fun = newLit(nameForNode n.NimNode)

# AFTER:
let fun = newLit(nameForNode n.NormNode)  # If nameForNode accepts NormNode
# OR
let fun = newLit(nameForNode (n: Name))
```

**Effort:** 1-2 hours

### 4.4 Phase 4: Validation & Testing (Week 4)

**Goal:** Ensure no regressions

#### 4.1 Run Full Test Suite
```bash
balls --define:debug    # Fast iteration
balls --define:danger   # Full matrix with sanitizers
```

**Effort:** 2-4 hours (initial failures + fixes)

#### 4.2 Add Type Coverage Tests

New tests verifying API consistency:

```nim
# In t40_ast.nim or new test file:

import pkg/cps/normalizedast

test "all public functions return semantic types":
  # Verify environment.nim functions return types not NimNode
  # Verify transform.nim constructs use abstractions
  # etc.
```

**Effort:** 2-3 hours

---

## 5. COMPLETENESS METRICS

### 5.1 Current State (Before Refactoring)

**AST Handling via NormNode:**

```
Component                  Coverage    Notes
─────────────────────────────────────────────────────────
Type system (Names)        95%         Excellent: Name, Ident, Sym
Type expressions           90%         Good: TypeExpr variants  
Routine definitions        90%         Good: RoutineDef, ProcDef
Identdefs                  85%         Good: IdentDef, RoutineParam
Var/Let sections           85%         Good: VarLet variants
Calls                      80%         Fair: Call exists, but genAst bypass
Pragmas                    80%         Fair: Full type coverage
Statements                 40%         POOR: No Statement type
Expressions                35%         POOR: No Expression type
Type sections              30%         POOR: Returns bare NimNode
─────────────────────────────────────────────────────────
OVERALL COVERAGE:          45-50%      
```

**NormNode adoption by module:**

```
rewrites.nim:  30%  (foundational, can't change)
callbacks.nim: 12%  (minor module)
defers.nim:    50%  (weak adoption)
environment.nm:40%  (CRITICAL)
hooks.nim:     50%  (mixed)
spec.nim:      60%  (acceptable)
returns.nim:   70%  (good)
exprs.nim:     75%  (good)
transform.nm:  70%  (high-value target)
```

### 5.2 Post-Refactoring Target State

**Expected improvements:**

```
Component                  Current  Post-Refactor  Gain
─────────────────────────────────────────────────────
Type system                95%      98%            +3%
Type expressions           90%      92%            +2%
Routine definitions        90%      95%            +5%
Identdefs                  85%      90%            +5%
Var/Let sections           85%      90%            +5%
Calls                      80%      85%            +5%
Pragmas                    80%      85%            +5%
Statements                 40%      75%            +35%
Expressions                35%      70%            +35%
Type sections              30%      80%            +50%
─────────────────────────────────────────────────────
OVERALL:                   45-50%   72-78%         +27-28%
```

**Module improvements:**

```
Module            Current  Target   Notes
─────────────────────────────────────────────────
environment.nim    40%      85%     Major redesign
transform.nim      70%      85%     genAst boundary
spec.nim           60%      75%     Minor cleanup
callbacks.nim      12%      60%     Import + refactor
rewrites.nim       30%      35%     Intentionally mixed
```

---

## 6. DESIGN IMPROVEMENTS NEEDED

### 6.1 Abstraction Completeness

#### Current Gaps

1. **No Statement type** 
   - Used for: Code blocks, statement lists
   - Current: Bare NormNode
   - Impact: ~50 locations without semantic type

2. **No Expression type**
   - Used for: Expressions with type info
   - Current: Bare NormNode or Call
   - Impact: ~30 locations

3. **No unified CodeFragment type**
   - Used for: Any executable code
   - Current: Function-dependent (returns NimNode)
   - Impact: High ambiguity in environment.nim

#### Recommended Additions

```nim
type
  # Base classifications
  CodeFragment* = distinct NormNode
    ## Any executable code (statement or expression)
  
  Statement* = distinct CodeFragment
    ## Sequence of statements (nnkStmtList, nnkStmtListExpr)
  
  Expression* = distinct CodeFragment  
    ## Value-producing expression with type
  
  # Construction detail
  TypeSection* = distinct NormNode
    ## nnkTypeSection (already defined but not exported)
  
  FieldDef* = distinct IdentDef
    ## Object field definition
  
  GenericParam* = distinct IdentDef
    ## Generic parameter definition
```

### 6.2 API Consistency

#### Naming Convention Standards

Establish and document:

```nim
# Pattern 1: Validation constructors
func asName*(n: NormNode): Name
func asCall*(n: NormNode): Call
# Errors if kind doesn't match

# Pattern 2: Fallible constructors
func asNameAllowEmpty*(n: NormNode): Name
# Allows nnkEmpty, but still validates structure

# Pattern 3: Safe constructors  
proc newName*(s: string): Name
proc newCall*(f: NormNode, args: varargs[NormNode]): Call
# Always succeed, return valid type

# Pattern 4: Type narrowing
func asIdentDefLet*(n: IdentDef): IdentDefLet
# Casts if possible, or refines type

# CONVENTION: Never accept bare NimNode if NormNode exists
#             Each path (asXXX, newXXX) should be clear
```

#### Function Signature Audit

Functions that should change:

| Function | Current Sig | New Sig | Reason |
|----------|------------|---------|--------|
| environment.makeType | `(): NimNode` | `(): TypeSection` | Semantic |
| environment.objectType | `(): NimNode` | `(): TypeExpr` | Semantic |
| environment.initialization | `(): NimNode` | `(): Call \| NormNode` | Clarify |
| environment.addAssignment | `(): NimNode` | `(): Call` | Semantic |
| transform.childCallName | `(n: NimNode)` | `(n: Call)` | Stricter |

### 6.3 Boundary Documentation

Create clear contract for genAst boundary:

```nim
## genAst Boundary Layer
##
## The genAst/genAstOpt macros from std/macros require bare NimNode
## arguments. To maintain NormNode abstraction, follow these patterns:
##
## Pattern 1: Minimal extraction
##   let result = NormNode genAst(x = n.NimNode):
##     # x is usable as NimNode here
##     discard x
##
## Pattern 2: Wrapper function
##   proc myGenAst[T](n: T): NormNode =
##     genAst(x = n.NimNode): # ...
##
## All .NimNode conversions at genAst boundaries should be
## documented with this explanation.
```

---

## 7. RENAMING TO ast.nim

### 7.1 What the Rename Signals

**Current name: normalizedast.nim**
- "Normalized AST" - emphasizes constraint
- Suggests it's optional/supplementary
- Reads as "an alternative AST"

**Proposed name: ast.nim**
- "AST" - suggests primacy
- Implies "this is THE AST abstraction"
- Positions it as the canonical layer

### 7.2 Architectural Signal

| Aspect | With "normalizedast" | With "ast" |
|--------|---------------------|-----------|
| **Position** | Optional abstraction | Primary layer |
| **Expectation** | May work with raw NimNode | Should use ast types |
| **Import clarity** | `import cps/normalizedast` seems optional | `import cps/ast` seems essential |
| **API priority** | Mixed NimNode and NormNode OK | Only exported types OK |
| **Refactoring pressure** | Low - "can coexist" | High - "should be primary" |

### 7.3 Justification for Rename

**YES, rename makes sense because:**

1. **Abstraction is comprehensive**
   - Covers ~95% of important AST concepts
   - Every major operation has a type

2. **Design is mature**
   - Consistent patterns established
   - Well-documented philosophy
   - Proven patterns in use

3. **Adoption is widespread**
   - 10 modules import it
   - 200+ lines use it effectively
   - Blocks show what good looks like

4. **Barriers are resolvable**
   - genAst boundary is known and documented
   - Missing types are few (5-6)
   - API gaps are specific

5. **Would accelerate adoption**
   - Signals architectural importance
   - Creates pressure to complete adoption
   - Makes imports more idiomatic

### 7.4 Post-Rename Activities

After `normalizedast.nim` → `ast.nim`:

1. **Update documentation** (1 hour)
   - AGENTS.md - update code examples
   - Module-level docstring
   - README.md references

2. **Update imports** (2 hours)
   - 10 import statements across codebase
   - Update test imports

3. **Announce in comments** (0.5 hours)
   ```nim
   # At top of ast.nim:
   ## This module provides the primary AST abstraction for CPS.
   ## Named "ast" (not "normalizedast") to signal architectural primacy.
   ## All CPS code should work with types from this module, not bare NimNode.
   ```

4. **Add migration guide** (1 hour)
   - Document how to update code
   - Show before/after patterns
   - Examples for common cases

---

## 8. IMPLEMENTATION ROADMAP

### Phase 1: Foundation (1 week)
- [ ] Rename normalizedast.nim → ast.nim (1-2 hours)
- [ ] Update all imports (2 hours)
- [ ] Add missing types (Statement, Expression, etc.) (4-6 hours)
- [ ] Update module-level documentation (2 hours)
- [ ] Run full test suite (2 hours)

**Deliverable:** Renamed module with expanded type coverage

### Phase 2: environment.nim Redesign (1 week)  
- [ ] Analyze all public functions returning NimNode (2 hours)
- [ ] Design semantic types for each (2 hours)
- [ ] Update function signatures (3 hours)
- [ ] Update 40+ call sites (6-8 hours)
- [ ] Test and debug (4 hours)

**Deliverable:** environment.nim returns only semantic types

### Phase 3: Call Site Cleanup (1 week)
- [ ] Clean up transform.nim genAst boundaries (3-4 hours)
- [ ] Fix callbacks.nim imports and usage (2 hours)
- [ ] Fix hooks.nim direct calls (1-2 hours)
- [ ] Add documentation for genAst pattern (1 hour)
- [ ] Code review and testing (3-4 hours)

**Deliverable:** Minimal .NimNode conversions outside genAst boundaries

### Phase 4: Validation (1 week)
- [ ] Full test suite run with sanitizers (2-4 hours)
- [ ] Add type coverage tests (2-3 hours)
- [ ] Fix any regressions (3-5 hours)
- [ ] Documentation updates (2 hours)
- [ ] Code review and finalization (2 hours)

**Deliverable:** Complete adoption with 70%+ coverage

---

## 9. EFFORT SUMMARY

### Time Investment by Category

```
Activity                        Hours   Week
─────────────────────────────────────────────
Phase 1: Foundation
  Rename + imports                2      1
  Add types                        5      1
  Documentation                    2      1
  Testing                          2      1
  Subtotal                        11      1

Phase 2: environment.nim
  Analysis                         2      2
  Design                           2      2
  Signature updates                3      2
  Call site updates                8      2
  Testing                          4      2
  Subtotal                        19      1

Phase 3: Cleanup
  transform.nim                    4      3
  callbacks/hooks                  3      3
  Documentation                    1      3
  Testing                          4      3
  Subtotal                        12      1

Phase 4: Validation
  Test suite runs                  3      4
  Type coverage tests              3      4
  Regression fixes                 4      4
  Final documentation              2      4
  Review & finalization            2      4
  Subtotal                        14      1

TOTAL EFFORT:                     56 hours (4 weeks)
```

### Resource Requirements

- **1 developer** for 4 weeks full-time, OR
- **2 developers** for 2 weeks (one on phases 1-2, one on 3-4), OR
- **Distributed:** 5-10 hours/week over 8 weeks

### Risk Factors

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| genAst limitation blocks design | Medium | High | Accept boundary, document |
| Regressions in transform.nim | Medium | High | Comprehensive test suite |
| environment.nim call sites scattered | High | Medium | Grep audit before starting |
| Type signature changes break code | Low | Low | Focused refactoring, tests |

---

## 10. RECOMMENDATIONS

### Immediate Actions (Next Sprint)

1. **Rename to ast.nim** ✓
   - Symbolic importance
   - Low risk change
   - Signals direction

2. **Add missing types** ✓
   - Statement, Expression type classes
   - Unblocks environment.nim redesign
   - 4-6 hour investment

3. **Document genAst boundary** ✓
   - Clarifies why some code uses .NimNode
   - Prevents future confusion
   - 1-2 hour investment

### Medium-term (Next 2-4 weeks)

4. **Complete environment.nim adoption** ✓
   - Highest-impact module
   - 15+ functions need updates
   - 40+ call site updates

5. **Standardize naming conventions** ✓
   - asXXX validation functions
   - newXXX constructors
   - Clear, documented patterns

### Longer-term (Month 2+)

6. **Automated type coverage audit** 
   - Add lint rules detecting raw NimNode in CPS layer
   - Catch regressions automatically

7. **Extend to compiler integration**
   - Consider exposing ast types in public API
   - Enable better error messages in user code

---

## 11. CONCLUSION

### Summary

The normalizedast module is a **well-designed abstraction** with ~45-50% current adoption. Complete adoption to 70%+ is **feasible and desirable**, requiring:

- **4 weeks of focused work** (40-56 developer hours)
- **5-6 new types** to fill semantic gaps
- **50+ call sites** updated for consistency
- **Renaming to ast.nim** to signal architectural importance

### Why Complete Adoption?

1. **Type Safety** - Catch misuse at compile time
2. **Clarity** - Semantic types make intent obvious
3. **Maintainability** - Less context needed to understand code
4. **Consistency** - Uniform patterns across codebase
5. **Extensibility** - Easier to add features with typed abstractions

### Critical Path

1. Rename module (signals commitment)
2. Add missing type categories
3. Redesign environment.nim public API
4. Clean up remaining call sites
5. Validate and test

### Verdict: Renaming to ast.nim

**RECOMMENDATION: Yes, rename**

The abstraction is comprehensive and mature enough to justify being "the" AST layer. The rename creates architectural clarity and accelerates adoption through increased visibility and cultural pressure.

---

**End of Analysis**

*This document should be updated after implementing changes to track actual effort and validate estimates.*
