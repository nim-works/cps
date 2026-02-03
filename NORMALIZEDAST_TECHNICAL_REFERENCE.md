# Technical Reference: normalizedast.nim Adoption Patterns

This document provides concrete code examples and patterns for adopting normalizedast (proposed ast.nim) throughout the CPS codebase.

---

## 1. Type System Reference

### 1.1 Name Types (nnkIdent, nnkSym)

**Definition in normalizedast.nim (lines 73-82):**
```nim
type
  Name* = distinct NormNode
    ## opaque sum: `Ident` | `Sym`
  Ident* = distinct Name
    ## nnkIdent
  Sym* = distinct Name
    ## nnkSym
```

**Construction patterns:**

```nim
# From string - creates nnkIdent
let myName = asName("variable")  # Name

# From genSym - creates nnkSym with fresh binding
let freshVar = genSymVar("temp")  # Name
let freshProc = genSymProc("cont")  # Name
let freshType = genSymType("Env")  # Name
let freshField = genSymField("x")  # Name

# Upgrade specific symbol type
let sym: Sym = asName(someNode).Sym  # Must be nnkSym
let ident: Ident = asName(someNode).Ident  # Must be nnkIdent

# From TypeExpr - extract as Name
let typeName: Name = asName(typeExpr)

# Conversion utilities
let desymmed = desym(someName)  # nnkIdent | nnkSym → nnkIdent
let isSymbol = someName.isSymbol  # bool check
let stringVal = someName.strVal  # String extraction
```

**Usage in environment.nim (lines 55-61):**
```nim
proc inherits*(e: Env): Name =
  if e.parent.isNil:
    e.via  # Name type
  else:
    e.parent.id  # Name type

proc identity*(e: Env): Name = e.id  # Name type
```

### 1.2 Type Expression Types

**Definition (lines 87-94):**
```nim
type
  TypeExpr* = distinct NormNode
    ## opaque sum of all type expressions
  TypeExprObj* = distinct TypeExpr
    ## nnkObjectTy
  TypeExprRef* = distinct TypeExpr
    ## nnkRefTy
```

**Construction patterns:**

```nim
# From empty node - unspecified type
let emptyType = newEmptyNormNode().TypeExpr

# From existing expression - coerce
let asType: TypeExpr = asTypeExpr(someNode)

# Type-specific construction
let refType: TypeExprRef = newRefType(typeName)  # ref T

# Sink-annotated type
let sinkType = sinkAnnotated(someType)  # sink T

# Get type from IdentDef
let x: IdentDef = newIdentDef(name, intType, value)
let gotType: TypeExpr = x.typ

# Type narrowing
let asObj: TypeExprObj = asTypeExprObj(someNode)
let asRef: TypeExprRef = asTypeExprRef(someNode)
```

**Usage patterns:**

```nim
# Declaring return type
let retType: TypeExpr = bindName"Continuation"

# In function definitions
proc newProcDef(name: Name, retType: TypeExpr, ...): ProcDef

# Accessing type
proc typ*(n: DefLike): TypeExpr = TypeExpr n.NormNode[^2]
```

### 1.3 IdentDef Types

**Definition (lines 96-109):**
```nim
type
  IdentDef* = distinct NormNode
    ## normalized nnkIdentDefs with exactly 3 children:
    ## [0] name (Ident | Sym | PragmaExpr)
    ## [1] type (TypeExpr or empty)
    ## [2] value (initializer or empty)
  
  RoutineParam* = distinct IdentDef
    ## variant used for routine parameters
```

**Construction patterns:**

```nim
# Basic identdef
let simple: IdentDef = newIdentDef(name, typeExpr, value)

# Without type (infer from value)
let inferred: IdentDef = newIdentDef(name, value)

# Without value (declaration only)
let declared: IdentDef = newIdentDef(name, typeExpr)

# As routine parameter
let param: RoutineParam = newIdentDef(name, typeExpr, value).RoutineParam

# Validation from raw NimNode
let validated: IdentDef = asIdentDefs(someNode)

# Access parts
let paramName: Name = simple.name
let paramType: TypeExpr = simple.typ
let paramValue: NormNode = simple.val
let hasVal: bool = simple.hasValue
let hasType: bool = simple.hasType
```

**Important invariant (from rewrites.nim validation, line 757-764):**
```nim
proc validateIdentDefs(n: NimNode) =
  if n.kind != nnkIdentDefs:
    errorGot "not an IdentDef", n
  elif n.len != 3:  # ← CRITICAL: Must have exactly 3 children
    errorGot "bad rewrite, failed to set init", n
  elif n[0].kind notin {nnkIdent, nnkSym, nnkPragmaExpr}:
    errorGot "bad rewrite presented", n
```

### 1.4 Var/Let Section Types (Complex Type Hierarchy)

**Definition (lines 151-174):**
```nim
type
  VarLet* = distinct NormNode              # Union base
  VarLetTuple* = distinct VarLet           # with VarTuple
  VarLetIdentDef* = distinct VarLet        # with IdentDef
  
  LetSection* = distinct VarLet            # let variant
  VarSection* = distinct VarLet            # var variant
  
  VarIdentDef* = distinct VarLetIdentDef   # var + IdentDef
  LetIdentDef* = distinct VarLetIdentDef   # let + IdentDef
```

**Hierarchy visualization:**
```
VarLet (nnkLetSection | nnkVarSection)
├── VarLetIdentDef (contains nnkIdentDefs)
│   ├── VarIdentDef (var section variant)
│   └── LetIdentDef (let section variant)
└── VarLetTuple (contains nnkVarTuple)
```

**Construction patterns:**

```nim
# Create var section with IdentDef
let varX: VarSection = newVarSection(name, typeExpr, value)
# Produces: `var x: Type = value`

# Create let section with IdentDef
let letY: LetSection = newLetSection(name, typeExpr, value)
# Produces: `let y: Type = value`

# Create specific subtypes
let varIdent: VarIdentDef = newVarIdentDef(identdef)
let letIdent: LetIdentDef = newLetIdentDef(identdef)

# Upgrade to parent type
let section: VarLet = (letY: LetSection).VarLet
```

**Runtime type narrowing:**

```nim
proc asVarLet*(n: NimNode): VarLet =
  ## Accepts both let and var

proc asVarLetIdentDef*(n: VarLet): VarLetIdentDef =
  ## Narrows to IdentDef variant only

proc asVarLetTuple*(n: VarLet): VarLetTuple =
  ## Narrows to Tuple variant only

# Usage:
let section = asVarLet(someNode)
if section.isTuple:
  let tupleVersion = asVarLetTuple(section)
  # Use tuple-specific operations
else:
  let identVersion = asVarLetIdentDef(section)
  # Use identdef-specific operations
```

**Access patterns:**

```nim
# Generic VarLet interface
let defNode: DefVarLet | TupleDefVarLet = section.def
let value: NormNode = section.val
let typ: TypeExpr = section.typ
let isIdentDef: bool = not section.isTuple

# IdentDef-specific
let identSection: VarLetIdentDef = asVarLetIdentDef(section)
let ident: IdentDef = identSection.identdef
let name: Name = identSection.name
```

### 1.5 Routine Definition Types

**Definition (lines 101-104, 106):**
```nim
type
  RoutineDef* = distinct NormNode
    ## any RoutineNode (proc, func, template, macro, etc.)
  ProcDef* = distinct RoutineDef
    ## specifically nnkProcDef
  FormalParams* = distinct NormNode
    ## formal parameters (return param + calling params)
```

**Construction patterns:**

```nim
# Create proc definition
let proc: ProcDef = newProcDef(
  name = procName,
  retType = returnType,
  newIdentDef(paramName, paramType)
)

# Clone existing proc
let newProc: ProcDef = proc.clone(newBody)

# Access parts
let procName: Name = proc.name
proc.name = newName  # Set name
let procBody: NormNode = proc.body
proc.body = newBody  # Set body
let params: FormalParams = proc.formalParams
proc.formalParams = newParams

# Iterate calling parameters (skip return param)
for param in proc.callingParams:
  # param is RoutineParam
  echo param.name

# Get first calling parameter
let contParam = proc.firstCallParam  # RoutineParam
```

### 1.6 Call Types

**Definition (lines 111-116):**
```nim
type
  Call* = distinct NormNode
    ## opaque over CallNodes (various call AST forms)
  CallKind* = Call
    ## alias for Call (not meaningfully different)
```

**Construction patterns:**

```nim
# Create calls
let call1: Call = newCall(funcName, arg1, arg2)  # With Name
let call2: Call = newCall(funcNode, arg1, arg2)  # With NormNode
let call3: Call = newCall("functionName", arg1)  # With string (becomes Ident)

# From raw AST - with validation
let call: Call = asCall(someNode)  # Must be CallNodes kind
let callKind: CallKind = asCallKind(someNode)  # Must be nnkCallKinds

# Access parts
let function: Name = call.name
call.name = newFunction  # Set function
let args = call[1..^1]  # All arguments after function

# Check for implementation
let hasImpl: bool = call.hasImpl  # Function name is symbol with impl
let impl: RoutineDef = call.impl  # Get the function's definition

# Utility: recover result from continuation call
let result: Call = call.childCallToRecoverResult(symbol, field)

# Prepend argument (for adding continuation param)
call.prependArg(contParam)
```

### 1.7 Pragma Types

**Definition (lines 121-128, 130):**
```nim
type
  Pragma* = distinct NormNode
    ## opaque sum: PragmaStmt | PragmaBlock | PragmaExpr
  PragmaStmt* = distinct Pragma
    ## nnkPragma node
  PragmaBlock* = distinct Pragma
    ## nnkPragmaBlock
  PragmaExpr* = distinct Pragma
    ## nnkPragmaExpr
  PragmaAtom* = distinct NormNode
    ## individual pragma expression
```

**Construction patterns:**

```nim
# Create pragma statement
let prags: PragmaStmt = newPragmaStmt(atom1, atom2)

# Create individual pragma atoms
let atom1: PragmaAtom = asPragmaAtom(name)
let atom2: PragmaAtom = newPragmaColonExpr("deprecated", reasonValue)

# Add pragmas to routine
proc.addPragma(pragmaName)  # {.pragmaName.}
proc.addPragma(pragmaName, argument)  # {.pragmaName: argument.}

# Check for pragma
let has: bool = proc.hasPragma("cpsCall")
let pragmas: PragmaLike = proc.pragma
```

---

## 2. Conversion Function Reference

### 2.1 Validation Conversions (asXXX Functions)

These validate the input kind and convert, or error out:

```nim
# Name conversions
func asName*(n: NormNode): Name  # line 486
func asNameAllowEmpty*(n: NormNode): Name  # line 487

# Type conversions
func asTypeExpr*(n: NormNode): TypeExpr  # line 630
func asTypeExprAllowEmpty*(n: NormNode): TypeExpr  # line 631
func asTypeExprObj*(n: NormNode): TypeExprObj  # line 648
func asTypeExprRef*(n: NormNode): TypeExprRef  # line 652

# Identdef
proc asIdentDefs*(n: NimNode): IdentDef  # line 766

# VarLet variants
proc asVarLet*(n: NimNode): VarLet  # line 878
proc asVarLetTuple*(n: VarLet): VarLetTuple  # line 885
proc asVarLetIdentDef*(n: VarLet): VarLetIdentDef  # line 888

# Pragma
func asPragmaStmt*(n: NormNode): PragmaStmt  # line 990

# Routine
func asRoutineDef*(n: NormNode): RoutineDef  # line 1145
func asProcDef*(n: NormNode): ProcDef  # line 1189

# Call
func asCall*(n: NormNode): Call  # line 1077
func asCallKind*(n: NormNode): CallKind  # line 1076

# Conversion
func asConv*(n: NormNode): Conv  # line 1125

# Type sections
func asTypeSection*(n: NormNode): TypeSection  # line 623
func asTypeDef*(n: NormNode): TypeDef  # line 626
```

**Usage Pattern:**
```nim
# If you know it should be a Call, validate:
try:
  let call: Call = asCall(node)
  # Now safely use call-specific operations
except:
  # Wrong kind - will error at compile time if kind is known
```

### 2.2 Construction Functions (newXXX Functions)

Create values guaranteeing invariants:

```nim
# Names - from string
proc asName*(n: string; info: NormNode = NilNormNode): Name  # line 507

# Names - genSym variants
proc genSymType*(n: string; info: NormNode = NilNormNode): Name  # line 511
proc genSymVar*(n: string = ""; info: NormNode = NilNormNode): Name  # line 514
proc genSymLet*(n: string = ""; info: NormNode = NilNormNode): Name  # line 517
proc genSymProc*(n: string; info: NormNode = NilNormNode): Name  # line 520
proc genSymField*(n: string; info: NormNode = NilNormNode): Name  # line 523
proc genSymUnknown*(n: string; info: NormNode = NilNormNode): Name  # line 526

# Types
proc newRefType*(n: Name): TypeExprRef  # line 654

# Identdef
proc newIdentDef*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDef  # line 771
proc newIdentDef*(n: string, t: TypeExprLike, val = newEmptyNode()): IdentDef  # line 776
proc newIdentDef*(n: Name, val: NormNode): IdentDef  # line 780

# Var/Let sections
proc newVarSection*(i: IdentDef): VarSection  # line 958
proc newVarSection*(n: Name, t: TypeExprLike, val = newEmptyNode()): VarSection  # line 962
proc newLetIdentDef*(i: IdentDef): LetIdentDef  # line 968
proc newLetIdentDef*(n: Name, val: NormNode): LetIdentDef  # line 972
proc newLetIdentDef*(n: Name, t: TypeExprLike, val = newEmptyNode()): LetIdentDef  # line 974

# Procedures
proc newProcDef*(name: Name, retType: TypeExpr, callParams: varargs[IdentDef]): ProcDef  # line 1181

# Calls
proc newCall*(n: NormNode, args: AnyNodeVarargs): Call  # line 612
proc newCall*(n: Name, args: AnyNodeVarargs): Call  # line 615
proc newCall*(n: string, args: AnyNodeVarargs): Call  # line 618

# Pragmas
proc newPragmaStmt*(es: varargs[PragmaAtom]): PragmaStmt  # line 995
proc newPragmaStmt*(n: Name): PragmaStmt  # line 1005
proc newPragmaStmtWithInfo*(inf: NormNode, es: varargs[PragmaAtom]): PragmaStmt  # line 1010

# Type sections
proc newTypeSection*(defs: varargs[TypeDef]): TypeSection  # NOT YET DEFINED
  ## Should be added to complete coverage
```

---

## 3. Before/After Refactoring Examples

### 3.1 environment.nim Functions

**BEFORE (current code):**

```nim
# Line 133
proc objectType(e: Env): NimNode =
  # Result is unclear - is it a type? A statement?
  # Callers must know what to do with NimNode
  
# Line 168
proc makeType*(e: Env): NimNode =
  # Creates a type but returns untyped NimNode
  # Callers see: NimNode typeNode = makeType(env)
  # Then what? Is it a TypeSection? TypeDef? Type expression?
```

**AFTER (proposed):**

```nim
# Fully semantic return types
proc objectType(e: Env): TypeExpr =
  ## Returns the object type expression (always nnkObjectTy)
  # Now type-safe: callers know exactly what they have

proc makeType*(e: Env): TypeSection =
  ## Returns a complete type section (nnkTypeSection)
  # Clear to caller: this is a full type section, ready to add to code
```

**Call sites change from:**

```nim
# BEFORE (line 340 in environment.nim)
(NimNode n)[0] = addInitializationToDefault(n[0].NimNode)

# AFTER - more precise type
let typeExpr: TypeExpr = objectType(env)
# Callers can use TypeExpr-specific operations
```

### 3.2 transform.nim genAst Pattern

**BEFORE:**

```nim
# Line 26 in transform.nim
proc makeContProc(name, cont, contType: Name; source: NimNode): ProcDef =
  genAstOpt({}, contParam = contParam.NimNode):  # Must convert to NimNode
    # Inside genAst, working with raw NimNode
    if not contParam.ex.isNil:
      raise contParam.ex
```

**AFTER (with documented boundary):**

```nim
proc makeContProc(name, cont, contType: Name; source: NimNode): ProcDef =
  ## GENAST BOUNDARY: genAstOpt requires NimNode arguments
  ## All .NimNode conversions below are necessary for genAst interop
  genAstOpt({}, contParam = contParam.NimNode):  # GENAST REQUIRED
    # Inside genAst, working with raw NimNode
    if not contParam.ex.isNil:
      raise contParam.ex
```

### 3.3 callbacks.nim Import Pattern

**BEFORE:**

```nim
# Line 11 of callbacks.nim
import cps/normalizedast except newTree, newStmtList
# ↑ Imports but excludes key constructors
```

**AFTER:**

```nim
# Full import - use what's needed
import cps/normalizedast
# Use ast.newTree, ast.newStmtList, etc. directly if needed
# Or rely on macros equivalents for clarity
```

### 3.4 hooks.nim Direct Calls

**BEFORE (lines 96-98):**

```nim
let fun = newLit(nameForNode n.NimNode)
let call = newCall(Trace.sym, event, c.NimNode, abbreviation n.NimNode,
                   "fun".eq fun, "info".eq info, body.NimNode).NormNode
```

**AFTER:**

```nim
let fun = newLit(nameForNode n)  # Accept NormNode directly
let call = newCall(Trace.sym, event, c, abbreviation n,
                   "fun".eq fun, "info".eq info, body)
```

---

## 4. Common Patterns and Anti-Patterns

### 4.1 Good Patterns

**Pattern 1: Type narrowing for operations**
```nim
# Good: Use specific type to enable specific operations
proc foo(n: IdentDefLike) =
  let name: Name = n.name  # Works on RoutineParam, IdentDef, etc.
  let typ: TypeExpr = n.typ
  if n.hasValue:
    # Do something with value
```

**Pattern 2: Validation at boundaries**
```nim
# Good: Convert to semantic type at entry point
proc processCall(n: NimNode) =
  let call: Call = asCall(n)  # Validate here
  # All subsequent code works with typed Call

# Bad: Check kind repeatedly
proc processCall(n: NormNode) =
  if n.kind in CallNodes:
    let name = n[0]  # Still working with NormNode
  # Lose type information
```

**Pattern 3: Construction with invariants**
```nim
# Good: Use constructor that ensures invariants
let param: IdentDef = newIdentDef(name, typ, value)
# Guaranteed: 3 children, name at [0], type at [1], value at [2]

# Bad: Manual construction
let param = nnkIdentDefs.newTree(name, typ, value)
# No guarantee structure is correct
```

### 4.2 Anti-Patterns to Avoid

**Anti-pattern 1: Mixing NimNode and NormNode**
```nim
# BAD
proc foo(n: NimNode): NormNode =
  # Sometimes uses n directly, sometimes uses NormNode n
  case n.kind:
    of nnkIdent: result = NormNode ident("x")
  # Inconsistent - callers don't know what's what

# GOOD
proc foo(n: NormNode): NormNode =
  # Always works with NormNode
  case n.kind:
    of nnkIdent: result = asName(n)
```

**Anti-pattern 2: Returning bare NormNode**
```nim
# BAD
proc buildTypeDefinition(name: Name): NormNode =
  # Caller doesn't know this is a TypeDef/TypeSection
  nnkTypeDef.newTree(name, newEmptyNode(), ...)

# GOOD
proc buildTypeDefinition(name: Name): TypeDef =
  # Type tells caller exactly what they have
  asTypeDef(nnkTypeDef.newTree(name, newEmptyNode(), ...))
```

**Anti-pattern 3: Avoiding conversion functions**
```nim
# BAD
proc process(n: NormNode) =
  if n.kind == nnkIdent or n.kind == nnkSym:
    let name: Name = n.Name  # Direct cast
  # No validation!

# GOOD
proc process(n: NormNode) =
  let name: Name = asName(n)  # Validates and converts
  # Will error if n is wrong kind
```

---

## 5. Type Coverage Analysis

### 5.1 By Module

**environment.nim - Missing semantic types:**
```nim
# Functions returning bare NimNode:

proc objectType(e: Env): NimNode
  # Should return: TypeExpr

proc makeType*(e: Env): NimNode
  # Should return: TypeSection (need to define)

proc initialization(e: Env; field: Name, section: VarLetIdentDef): NimNode
  # Should return: Call (initializer call)
  # Or: CodeFragment (new type to define)

proc addAssignment(e: var Env; d: IdentDef): NimNode
  # Should return: Call (assignment expression)

proc addAssignment(e: var Env; section: VarLetIdentDef): NimNode
  # Should return: Call

proc createContinuation*(e: Env; name: Name; goto: NimNode): NimNode
  # Input: goto should be Call or CodeFragment
  # Return: Call (continuation creation)

proc genException*(e: var Env): NimNode
  # Should return: Call (exception allocation)

proc createRecover*(env: Env, exported = false): NimNode
  # Should return: ProcDef or seq[ProcDef]
```

**transform.nim - Major consumers of environment:**
```nim
# 20+ call sites that work with raw NimNode results
# Fix environment.nim signatures → fix transform.nim automatically

# Also internal genAst patterns:
genAstOpt({}, contParam = contParam.NimNode):  # Boundary (can't fix)
```

**spec.nim - Mixed usage:**
```nim
# Has both NimNode and NormNode APIs
# Could be unified to use only NormNode throughout
# Low impact change (60% adoption already)
```

### 5.2 Type Gap Analysis

**Completely Missing:**

1. **Statement type**
   - Used for: Code blocks, statement sequences
   - Current: Bare NormNode
   - Occurrences: ~50 locations
   - Priority: HIGH

2. **Expression type**
   - Used for: Value expressions with type
   - Current: Bare NormNode or Call
   - Occurrences: ~30 locations
   - Priority: MEDIUM

3. **TypeSection type**
   - Currently: Defined (line 133) but not exported
   - Used for: Type definitions
   - Occurrences: ~10 locations
   - Priority: MEDIUM

4. **CodeFragment type**
   - Used for: Generic "executable code"
   - Current: NormNode with unclear semantics
   - Occurrences: 15+ locations
   - Priority: LOW (could use union with other types)

**Partially Missing:**

1. **FieldDef**
   - Currently uses: IdentDef (confusing)
   - Needed: Dedicated type or clear convention
   - Occurrences: 15 locations

2. **GenericParam**
   - Currently uses: IdentDef (works but confusing)
   - Needed: Clear convention documented
   - Occurrences: 5 locations

---

## 6. Migration Checklist

### For environment.nim Redesign

- [ ] **Analysis Phase**
  - [ ] List all functions returning NimNode
  - [ ] Determine semantic type for each return
  - [ ] Identify 40+ call sites in transform.nim
  - [ ] Plan missing type definitions

- [ ] **Type Definition Phase**
  - [ ] Add Statement type to normalizedast.nim
  - [ ] Add Expression type to normalizedast.nim
  - [ ] Add CodeFragment union type
  - [ ] Export TypeSection (currently internal)
  - [ ] Document FieldDef convention

- [ ] **Signature Update Phase**
  - [ ] Change objectType return to TypeExpr
  - [ ] Change makeType return to TypeSection
  - [ ] Change initialization return type
  - [ ] Change addAssignment return type
  - [ ] Change createContinuation to take/return semantically typed nodes
  - [ ] Change genException return type
  - [ ] Change createRecover return type

- [ ] **Call Site Updates**
  - [ ] Update transform.nim line 445 (createRecover usage)
  - [ ] Update transform.nim line 705 (makeType usage)
  - [ ] Update callbacks.nim (3 call sites)
  - [ ] Update any other consumers

- [ ] **Testing**
  - [ ] Run full test suite
  - [ ] Add type coverage tests
  - [ ] Test genAst boundaries still work

---

## 7. Helper Procedures to Add

### Proposed new functions for normalizedast.nim:

```nim
# Type construction
proc newTypeSection*(defs: varargs[TypeDef]): TypeSection =
  ## Create a new type section from definitions
  nnkTypeSection.newTree(varargs[NimNode] defs).TypeSection

# Statement construction
proc newStatement*(body: NormalizedVarargs): Statement =
  ## Create a new statement or statement list
  result = Statement nnkStmtList.newTree(varargs[NimNode] body)

# Expression wrapping
proc newExpression*(n: NormNode): Expression =
  ## Wrap any expression maintaining type information
  # This would need semantic validation
  result = Expression n

# FieldDef utilities  
proc asFieldDef*(n: IdentDef): FieldDef =
  ## Coerce IdentDef to FieldDef (object field variant)
  FieldDef n

proc newFieldDef*(name: Name, typ: TypeExpr, default: NormNode = newEmptyNormNode()): FieldDef =
  ## Create an object field definition
  asFieldDef(newIdentDef(name, typ, default))

# Generic parameter utilities
proc asGenericParam*(n: IdentDef): GenericParam =
  ## Coerce IdentDef to GenericParam variant
  GenericParam n

# CodeFragment union
type
  CodeFragment* = distinct NormNode

proc asCodeFragment*(n: NormNode): CodeFragment =
  ## Accept any executable code
  CodeFragment n
```

---

## 8. Documentation Template

### For each converted function:

```nim
proc newName*(parameters): ReturnType =
  ## [Brief description of what this produces]
  ##
  ## Produces:
  ##   [Example AST output]
  ##
  ## Invariants maintained:
  ##   - [Key structural property]
  ##   - [Key type property]
  ##
  ## Example:
  ##   let ident = newName("variable")  # nnkIdent
  ##   let sym = genSymVar("temp")      # nnkSym with fresh binding
  ##
  ## See also: asXXX() for validation, XXX type for semantics
```

---

**End of Technical Reference**
