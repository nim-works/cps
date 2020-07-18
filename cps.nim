import std/macros
import std/tables
import std/sets
import std/strutils
import std/sequtils

##[
## To transform a CPC program into CPS-convertible form, the CPC
## translator needs to ensure that every call to a cps function is either
## in tail position or followed by a tail call to another cps function.
]##

const
  strict = true     ## only cps operations are strictly cps operations
  comments = true   ## embed comments within the transformation
  lifting = true    ## whether to lift generated symbols to top-level

when NimMajor < 1 or NimMinor < 3:
  {.fatal: "requires nim-1.3".}

import cps/environment
import cps/eventqueue

export Cont

type
  Primitive = enum    ## operations on which all others are based
    Spawn
    Signal
    SignalAll
    Yield
    Sleep
    Wait
    Io
    Attach
    Detach

const
  unexiter = {nnkWhileStmt, nnkBreakStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkElse, nnkStmtList}

when strict:
  # currently, we only run Spawn from anywhere
  const
    cpsContext = {Signal .. high(Primitive)}
    cpsAnywhere = {Spawn}
else:
  # i think we may want to also run Signals from anywhere
  const
    cpsContext = {Yield .. high(Primitive)}
    cpsAnywhere = {Spawn .. SignalAll}
assert cpsContext + cpsAnywhere == {Primitive.low .. Primitive.high}
assert cpsContext * cpsAnywhere == {}

func makeIdent(op: Primitive): NimNode =
  ident("cps_" & $op)

proc makeIdents(): seq[NimNode] {.compileTime.} =
  for op in items(Primitive):
    result.add makeIdent(op)
let cpsIdents {.compileTime.} = makeIdents()

proc cps_yield*(cont: Cont) = discard

proc cps_sleep*(ms: int) {.deprecated.} =
  addTimer(nil.Cont, ms)

proc cps_sleep*(cont: Cont; ms: int): Cont =
  addTimer(cont, ms)

proc tailCall(e: Env; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` to ident (or nil) `n`
  if n.kind == nnkNilLit:
    result = nnkReturnStmt.newTree(n)
  else:
    result = newStmtList()
    let locals = result.defineLocals(e)
    result.add newAssignment(locals, newCall(e.identity))
    for name, asgn in e.localAssignments(locals):
      result.add asgn
    result.add nnkReturnStmt.newTree(newCall(n, locals))

func doc(s: string): NimNode =
  ## generate a doc statement for debugging
  when comments:
    newCommentStmtNode(s)
  else:
    newEmptyNode()

proc doc(n: var NimNode; s: string) =
  ## add a doc statement to the ast for debugging
  when comments:
    if n.kind == nnkStmtList:
      n.add doc(s)

func stripComments(n: NimNode): NimNode =
  ## remove doc statements because that was a stupid idea
  result = n.copyNimNode
  for child in items(n):
    if child.kind != nnkCommentStmt:
      result.add child.stripComments

func returnTo(n: NimNode): NimNode =
  ## take a `return foo()` or (proc foo() = ...) and yield `foo`
  let n = n.stripComments
  case n.kind
  of nnkProcDef:
    result = n.name
  of nnkCall:
    result = n[0]
  of nnkIdent, nnkSym:
    result = n
  of nnkNilLit:
    result = n
  else:
    result = returnTo(n[0])

func isReturnCall(n: NimNode): bool =
  ## true if the node looks like a tail call
  let n = n.stripComments
  case n.kind
  # simple `return foo()`
  of nnkReturnStmt:
    if n.len > 0:
      if n[0].kind == nnkCall:
        result = true
  # `return foo(); proc foo() = ...`
  of nnkStmtList:
    # FIXME: this is stupid and will misbehave on type sections
    result = case n.len
    of 1:
      n[0].isReturnCall
    of 2:
      n[0].isReturnCall and n[1].kind == nnkProcDef and n[1].name == n[0][0][0]
    else:
      false
  else:
    discard

func asSimpleReturnCall(n: NimNode; r: var NimNode): bool =
  ## fill `r` with `return foo()` if that is a safe simplification
  var n = n.stripComments
  block done:
    while n.kind == nnkStmtList:
      if len(n) != 1:
        break done
      n = n[0]
    result = isReturnCall(n)
    if result:
      r = newStmtList([doc "simple return call: " & n.repr, n])

proc isCpsProc(n: NimNode): bool

proc hasPragma(n: NimNode; s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  assert not n.isNil
  case n.kind
  of RoutineNodes:
    result = bindSym(s) in toSeq(n.pragma)
  of nnkObjectTy:
    result = bindSym(s) in toSeq(n[0])
  of nnkRefTy:
    result = hasPragma(n.last, s)
  of nnkTypeDef:
    result = hasPragma(n.last, s)
  of nnkTypeSection:
    result = anyIt(toSeq items(n), hasPragma(it, s))
  else:
    assert false, "unimplemented for " & $n.kind

proc filterPragma(ns: seq[NimNode], liftee: NimNode): NimNode =
  ## given a seq of pragmas, omit a match and return Pragma or Empty
  var pragmas = nnkPragma.newNimNode
  for p in filterIt(ns, it != liftee):
    pragmas.add p
  if len(pragmas) > 0:
    pragmas
  else:
    newEmptyNode()

proc stripPragma(n: NimNode; s: static[string]): NimNode =
  ## filter a pragma with the matching name from various nodes
  case n.kind
  of nnkPragma:
    result = filterPragma(toSeq n, bindSym(s))
  of RoutineNodes:
    n.pragma = stripPragma(n.pragma, s)
    result = n
  of nnkObjectTy:
    n[0] = filterPragma(toSeq n[0], bindSym(s))
    result = n
  of nnkRefTy:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeDef:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeSection:
    result = newNimNode(n.kind)
    for item in items(n):
      result.add stripPragma(item, s)
  else:
    result = n

proc isLiftable(n: NimNode): bool =
  ## is this a node we should float to top-level?
  result = case n.kind
  of RoutineNodes:
    n.hasPragma("cpsLift")
  of nnkTypeSection:
    anyIt(toSeq items(n), it.hasPragma("cpsLift"))
  else:
    false

proc isCpsIdent(n: NimNode): bool =
  ## it's a cps identifier
  result = n.kind == nnkIdent and anyIt(cpsIdents, eqIdent(n, it))

proc isCpsCall(n: NimNode): bool =
  ## `true` if `n` is a call to a cps routine
  result = n.kind == nnkCall and n[0].isCpsIdent
  when not strict:
    result = result or n.kind in RoutineNodes and n.isLiftable
    result = result or n.isCpsProc

proc isCpsBlock(n: NimNode): bool =
  ## `true` if the block `n` contains a cps call
  case n.kind
  of nnkProcDef, nnkElse, nnkElifBranch:
    result = isCpsBlock(n.last)
  #of nnkBreakStmt:
  #  result = insideCps()
  of nnkIfStmt:
    for child in n.children:
      result = child.isCpsBlock
      if result:
        break
  of nnkStmtList:
    for i, nc in pairs(n):
      result = (i != n.len-1 and nc.isCpsCall) or nc.isCpsBlock
      if result:
        break
  else:
    discard

proc mkLabel(s: string): NimNode =
  ## how we name a generated proc
  result = genSym(nskProc, ident = s)

proc foldTailCalls(n: NimNode): NimNode =
  ## this may optimize a `proc foo() = return bar()` to `return bar()`
  result = n
  assert result.kind == nnkProcDef, "not a proc"
  if not asSimpleReturnCall(n, result):
    if isReturnCall(n.last):
      result = newStmtList()
      result.doc "optimized proc into tail call"
      result.add n.last

proc liften(n: var NimNode): NimNode =
  ## lift ast tagged with cpsLift pragma to top-level and omit the pragma
  result = newStmtList()
  var dad = n.copyNimNode
  for kid in items(n):
    var kid = kid

    # lift anything below
    for k in items(liften(kid)):
      add(result, k)

    if kid.isLiftable:
      kid = stripPragma(kid, "cpsLift")
      result.add kid      # cps calls and types go in the result
    else:
      dad.add kid         # other stuff stays where it is

  n = dad

proc makeTail(env: var Env; name: NimNode; n: NimNode): NimNode =
  ## make a tail call and put it in a single statement list;
  ## this will always create a tail call proc and call it
  let lifter = bindSym"cpsLift"
  result = newStmtList()
  result.doc "new tail call: " & name.repr
  result.add env.tailCall(name)
  if n.kind == nnkProcDef:
    result.doc "adding the proc verbatim"
    result.add n
  else:
    # add the required type section
    result.doc "add type section"
    env.storeTypeSection(result)
    var body = newStmtList()
    var locals = genSym(nskParam, "locals")
    for name, asgn in localRetrievals(env, locals):
      body.add asgn
    body.add n
    var fun = newProc(name = name, body = body,
                      params = [env.inherits,
                                newIdentDefs(locals, env.identity)])
    if len(n) == 0:
      {.warning: "creating an empty tail call".}
    result.doc "creating a new proc: " & name.repr

    # prep it for lifting and add it to the result
    fun.addPragma lifter
    result.add fun

proc returnTail(env: var Env; name: NimNode; n: NimNode): NimNode =
  ## either create and return a tail call proc, or return nil
  let cont = bindSym"Cont"
  if len(n) == 0:
    # no code to run means we just `return Cont()`
    result = nnkReturnStmt.newNimNode(newCall(cont))
  else:
    # create a tail call with the given body
    result = env.makeTail(name, n)

proc callTail(env: var Env; n: NimNode): NimNode =
  ## given a node, either turn it into a
  ## `return call(); proc call() = ...`
  ## or optimize it into a `return subcall()`
  case n.kind
  of nnkProcDef:
    # if you already put it in a proc, we should just use it
    result = n
  of nnkIdent:
    # if it's an identifier, we'll just issue a call of it
    result = env.tailCall(n)
  of nnkStmtList:
    # maybe we can optimize it out
    if asSimpleReturnCall(n, result):
      discard "the call was stuffed into result"
    elif isReturnCall(n):
      # just copy the call
      result = newStmtList([doc"verbatim tail call", n])
    else:
      result = env.returnTail(mkLabel"tailcall", n)
  else:
    # wrap whatever it is and recurse on it
    result = env.callTail(newStmtList(n))

func next(ns: seq[NimNode]): NimNode =
  ## read the next call off the stack
  if len(ns) == 0:
    newNilLit()
  else:
    ns[^1]

proc optimizeSimpleReturn(env: var Env; into: var NimNode; n: NimNode) =
  ## experimental optimization
  var simple: NimNode
  var n = n.stripComments
  if asSimpleReturnCall(n.last.last, simple):
    into.doc "possibly unsafe optimization: " & n.repr
    env.optimizeSimpleReturn(into, simple)
  else:
    into.doc "add an unoptimized tail call"
    into.add env.callTail(n)

proc xfrm(n: NimNode; c: NimNode): NimNode =
  if c.kind == nnkEmpty:
    error "provide a continuation return type"
  else:
    hint "continuation return type " & repr(c)

  # create the environment in which we'll store locals
  var env = newEnv(c)

  # identifiers of future break or return targets
  var goto: seq[NimNode]
  var breaks: seq[NimNode]

  func insideCps(): bool = len(goto) > 0 or len(breaks) > 0

  proc saften(penv: var Env; input: NimNode): NimNode

  proc splitAt(env: var Env; n: NimNode; name: string; i: int): NimNode =
    ## split a statement list to create a tail call given
    ## a label prefix and an index at which to split
    let label = mkLabel name
    var body = newStmtList()
    body.doc "split as " & label.repr & " at index " & $i
    if i < n.len-1:
      body.add n[i+1 ..< n.len]
      body = env.saften(body)
      result = env.makeTail(label, body)
    else:
      result = env.callTail(newStmtList())

  template withGoto(n: NimNode; body: untyped): untyped =
    ## run a body with a longer goto stack
    if len(n.stripComments) > 0:
      add(goto, n)
      try:
        body
      finally:
        discard pop(goto)
    else:
      body

  proc saften(penv: var Env; input: NimNode): NimNode =
    ## transform `input` into a mutually-recursive cps convertible form
    result = input.copyNimNode

    # the accumulated environment
    var env = result.newEnv(penv)

    let n = input.stripComments
    result.doc "saften $1 with $2 gotos and $3 breaks" %
      [ $n.kind, $len(goto), $len(breaks) ]

    for i, nc in pairs(n):
      # if the child is a cps block (not a call), then push a tailcall
      # onto the stack during the saftening of the child
      if i < n.len-1:
        if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
          withGoto env.splitAt(n, "exit", i):
            result.add env.saften(nc)
            result.doc "add the exit proc definition"
            result.add next(goto)

            # we've completed the split, so we're done here
            return

      case nc.kind
      of nnkVarSection, nnkLetSection:
        # add definitions into the environment
        env.add nc
        # include the section normally (for now)
        result.add nc

      of nnkYieldStmt:
        if insideCps() or (i < n.len-1 and nc.isCpsCall):
          result.add newCall(ident"cps_yield")
        else:
          result.add nc

      of nnkCall:
        echo treeRepr(nc)
        if isCpsIdent(nc[0]):
          let x = env.splitAt(n, "cps", i)
          var p = x.params
          echo treeRepr(p)
          withGoto x:
            result.add env.saften(nc)
            if len(x.stripComments) > 0:
              result.add next(goto)
          # the split is complete
          return
        else:
          result.add nc

      of nnkBreakStmt:
        if len(breaks) > 0:
          result.doc "simple break statement"
          result.add env.tailCall(next(breaks).returnTo)
        else:
          result.doc "no break statements to pop"

      of nnkBlockStmt:
        let bp = env.splitAt(n, "break", i)
        add(breaks, bp)
        try:
          result.add env.saften(nc)
          if i < n.len-1:
            result.doc "add tail call for block-break proc"
            result.add env.callTail(next(breaks))
            return
        finally:
          discard pop(breaks)

      of nnkWhileStmt:
        let w = mkLabel "while"
        let bp = env.splitAt(n, "break", i)
        echo treeRepr(n)
        add(breaks, bp)
        add(goto, w)
        try:
          var loop = newStmtList()
          result.doc "add tail call for while loop"
          env.storeTypeSection(result)
          result.add env.makeTail(w, loop)
          # guys, lemme tell you about where we're goin'
          let (expr, body) = (nc[0], env.saften(nc[1]))
          loop.add newIfStmt((expr, newStmtList(body)))
          discard pop(goto)
          if i < n.len-1:
            loop.doc "add tail call for break proc"
            loop.add env.callTail(next(breaks))
            return
        finally:
          discard pop(breaks)

      of nnkIfStmt:
        # if any `if` clause is a cps block, then every clause must be
        # if we've pushed any goto or breaks, then we're already in cps
        if nc.isCpsBlock:
          let x = env.splitAt(n, "if", i)
          withGoto x:
            result.add env.saften(nc)
            if len(x.stripComments) > 0:
              result.add next(goto)
          # the split is complete
          return
        elif insideCps():
          result.add env.saften(nc)
        else:
          result.add nc

      else:
        result.doc "adding normal saften child " & $nc.kind
        result.add env.saften(nc)

      # if the child isn't last,
      if i < n.len-1:
        # and it's a cps call,
        if nc.isCpsCall or nc.isCpsBlock:
          let x = env.splitAt(n, "tailcall", i)
          env.optimizeSimpleReturn(result, x)
          # the split is complete
          return

    if n.kind in returner:
      if next(goto).kind != nnkNilLit:
        let duh = result.stripComments
        if len(duh) > 0 and isReturnCall(duh.last):
          result.doc "omit return call from " & $n.kind
        else:
          result.doc "adding return call to " & $n.kind
          result.add env.tailCall(next(goto).returnTo)
      else:
        result.doc "nil return"

  result = env.saften(n)

proc transform(n: NimNode; c: NimNode): NimNode =
  ## returns a statement list that might be transformed
  var body =
    if n.kind in RoutineNodes:
      n.body
    else:
      n
  var safe = xfrm(body, c).newStmtList # wrap it for liften
  let decls = liften(safe)
  if len(decls) == 0:
    # nothing lifted, nothing gained
    result = newStmtList(n)
  else:
    if n.kind in RoutineNodes:
      var new = copyNimTree(n)
      new.body = safe
      safe = new
    result = newStmtList(decls, safe)

macro cps*(n: typed) =
  when defined(nimdoc): return n
  if n.kind in RoutineNodes:
    result = transform(n, n.params[0])
  else:
    result = transform(n, bindSym"Cont")
  echo repr(result)

macro cps*(c: typed; n: typed) =
  result = transform(n, c)

proc isCpsProc(n: NimNode): bool =
  ## `true` if the node is a routine with our .cps. pragma
  let liftee = bindSym"cps"
  result = n.kind in RoutineNodes and liftee in toSeq(n.pragma)
