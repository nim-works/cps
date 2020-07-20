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
  #lifting = true    ## whether to lift generated symbols to top-level

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

  NodeFilter = proc(n: NimNode): NimNode

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

proc identityFilter(n: NimNode): NimNode = n

proc filter(n: NimNode; f: NodeFilter = identityFilter): NimNode =
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

func makeIdent(op: Primitive): NimNode =
  ident("cps_" & $op)

proc makeIdents(): seq[NimNode] {.compileTime.} =
  for op in items(Primitive):
    result.add makeIdent(op)
let cpsIdents {.compileTime.} = makeIdents()

proc isCpsIdent(n: NimNode): bool =
  ## it's a cps identifier
  result = n.kind in {nnkIdent, nnkSym} and
             anyIt(cpsIdents, eqIdent(n, it))

proc isCpsCall(n: NimNode): bool =
  ## `true` if `n` is a call to a cps routine
  result = n.kind == nnkCall and n[0].isCpsIdent
  when not strict:
    result = result or n.kind in RoutineNodes and n.isLiftable
    result = result or n.isCpsProc

proc maybeConvertToRoot(e: Env; locals: NimNode): NimNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    newDotExpr(locals, e.root)
  else:
    locals

proc tailCall(e: Env; p: NimNode; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` via cps call `p`
  assert p.isCpsCall
  # install locals as the 1st argument
  result = newStmtList()
  let locals = result.defineLocals(e, n)
  p.insert(1, e.maybeConvertToRoot(locals))
  result.add nnkReturnStmt.newNimNode(n).add p

proc tailCall(e: Env; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` to ident (or nil) `n`
  assert not isCpsCall(n)
  var ret = nnkReturnStmt.newNimNode(n)
  result = newStmtList()
  if n.kind == nnkNilLit:
    ret.add n
  else:
    # return a statement list including the setup for the locals
    # and the return statement casting those locals to the root type
    let locals = result.defineLocals(e, n)
    ret.add e.maybeConvertToRoot(locals)
  result.add ret

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
  result = copyNimNode n
  for child in items(n):
    if child.kind != nnkCommentStmt:
      result.add stripComments(child)

func returnTo(n: NimNode): NimNode =
  ## take a `return foo()` or (proc foo() = ...) and yield `foo`
  let n = stripComments n
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
  let n = stripComments n
  case n.kind
  # simple `return foo()`
  of nnkReturnStmt:
    if n.len > 0:
      if n[0].kind == nnkCall:
        result = true
  # `return foo(); proc foo() = ...`
  of nnkStmtList:
    # FIXME: this is stupid and does misbehave
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
  var n = stripComments n
  block done:
    while n.kind == nnkStmtList:
      if len(n) != 1:
        break done
      n = n[0]
    result = isReturnCall(n)
    if result:
      r = newStmtList([doc "simple return call: " & n.repr, n])

when not strict:
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
    result = false

proc filterPragma(ns: seq[NimNode], liftee: NimNode): NimNode =
  ## given a seq of pragmas, omit a match and return Pragma or Empty
  var pragmas = nnkPragma.newNimNode
  for p in filterIt(ns, it != liftee):
    pragmas.add p
    copyLineInfo(pragmas, p)
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
    result = newNimNode(n.kind, n)
    for item in items(n):
      result.add stripPragma(item, s)
  else:
    result = n

proc isLiftable(n: NimNode): bool =
  ## is this a node we should float to top-level?
  result = n.hasPragma "cpsLift"

proc hasLiftableChild(n: NimNode): bool =
  result = anyIt(toSeq items(n), it.isLiftable or it.hasLiftableChild)

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

when false:
  proc foldTailCalls(n: NimNode): NimNode =
    ## this may optimize a `proc foo() = return bar()` to `return bar()`
    result = n
    assert result.kind == nnkProcDef, "not a proc"
    if not asSimpleReturnCall(n, result):
      if isReturnCall(n.last):
        result = newStmtList()
        result.doc "optimized proc into tail call"
        result.add n.last

proc liften(lifted: NimNode; n: NimNode): NimNode =
  ## lift ast tagged with cpsLift pragma to top-level and omit the pragma
  proc doLift(n: NimNode): NimNode =
    if n.isLiftable:
      lifted.add filter(n.stripPragma "cpsLift", doLift)
      result = newEmptyNode()

  result = filter(n, doLift)
  result = newStmtList(lifted, result)

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
    env.storeTypeSection(result)
    var body = newStmtList()
    var locals = genSym(nskParam, "locals")
    for name, asgn in localRetrievals(env, locals):
      body.add asgn
    body.add n
    var fun = newProc(name = name, body = body,
                      params = [env.root, newIdentDefs(locals, env.root)])
    if len(n) == 0:
      {.warning: "creating an empty tail call".}
    result.doc "creating a new proc: " & name.repr

    # prep it for lifting and add it to the result
    fun.addPragma lifter
    result.add fun

proc returnTail(env: var Env; name: NimNode; n: NimNode): NimNode =
  ## either create and return a tail call proc, or return nil
  if len(n) == 0:
    # no code to run means we just `return Cont()`
    result = nnkReturnStmt.newNimNode(n).add newCall(env.root)
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
  var n = stripComments n
  if asSimpleReturnCall(n.last.last, simple):
    into.doc "possibly unsafe optimization: " & n.repr
    env.optimizeSimpleReturn(into, simple)
  else:
    into.doc "add an unoptimized tail call"
    into.add env.callTail(n)

proc xfrm(n: NimNode; c: NimNode): NimNode =
  if c.isEmpty:
    error "provide a continuation return type"

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
      # FIXME: we should pop goto here, right?
      result = env.callTail next(goto)

  template withGoto(result: NimNode; n: NimNode; body: untyped): untyped =
    ## run a body with a longer goto stack
    if len(stripComments n) > 0:
      add(goto, n)
      try:
        body
      finally:
        result.add next(goto)
        discard pop(goto)
    else:
      body

  proc saften(penv: var Env; input: NimNode): NimNode =
    ## transform `input` into a mutually-recursive cps convertible form

    result = copyNimNode input

    # the accumulated environment
    var env = result.newEnv(penv)

    let n = stripComments input
    result.doc "saften $1 with $2 gotos and $3 breaks" %
      [ $n.kind, $len(goto), $len(breaks) ]

    for i, nc in pairs(n):
      # if the child is a cps block (not a call), then push a tailcall
      # onto the stack during the saftening of the child
      if i < n.len-1:
        if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
          result.withGoto env.splitAt(n, "exit", i):
            result.add env.saften(nc)
            result.doc "add the exit proc definition"
            # we've completed the split, so we're done here
            return

        if isCpsCall(nc):
          env.storeTypeSection(result)
          result.withGoto env.splitAt(n, "after", i):
            result.add env.tailCall(nc, next(goto).returnTo)
            return
          assert false, "unexpected"

      case nc.kind
      of nnkVarSection, nnkLetSection:
        # add definitions into the environment
        env.add nc
        # include the section normally (for now)
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
        if false and not nc.isCpsBlock:
          # FIXME
          result.add env.saften(nc)
        else:
          let w = mkLabel "while"
          let bp = env.splitAt(n, "break", i)
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
              #loop.doc n.repr
              loop.add env.callTail(next(breaks))
              return
          finally:
            discard pop(breaks)

      of nnkIfStmt:
        # if any `if` clause is a cps block, then every clause must be
        # if we've pushed any goto or breaks, then we're already in cps
        if nc.isCpsBlock:
          result.withGoto env.splitAt(n, "if", i):
            result.add env.saften(nc)
            # the split is complete
            return
          # if we didn't return, then this is the last block
          assert false, "unexpected"
          result.add nc
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
        let duh = stripComments result
        if len(duh) > 0 and isReturnCall(duh.last):
          result.doc "omit return call from " & $n.kind
        else:
          result.doc "adding return call to " & $n.kind
          result.add env.tailCall(next(goto).returnTo)
      else:
        result.doc "nil return"

  result = env.saften(n)

macro cps*(n: untyped): untyped =
  when defined(nimdoc): return n
  n.body = xfrm(n.body, n.params[0])
  result = newStmtList()
  var x = liften(result, n)
  result = newStmtList(result, x)
  echo treeRepr(result)
  echo repr(result)

when false:
  macro cps*(c: typed; n: typed): untyped =
    var
      safe = xfrm(n, c)
      decls = liften(safe)
    result = newStmtList()
    if len(decls) > 0:
      result.add decls
    result.add safe
    assert false

macro cpsMagic*(n: untyped): untyped =
  ## upgrade cps primitives to generate errors out of context
  ## and take continuations as input inside {.cps.} blocks
  when defined(nimdoc): return n
  expectKind(n, nnkProcDef)
  result = newStmtList()

  # create a version of the proc that pukes outside of cps context
  var m = copyNimTree n
  let msg = $n.name & "() is only valid in {.cps.} context"
  m.params[0] = newEmptyNode()
  when false:
    m.addPragma newColonExpr(ident"error", msg.newLit)
    m.body = nnkDiscardStmt.newNimNode(n).add newEmptyNode()
  elif true:
    m.body = nnkPragma.newNimNode(n).add newColonExpr(ident"warning",
                                                      msg.newLit)
  else:
    m.body = nnkCall.newNimNode(n).newTree(ident"error", msg.newLit)
  result.add m

  # manipulate the primitive to take its return type as a first arg
  var prefixed = nnkFormalParams.newNimNode(n)
  prefixed.add n.params[0]
  prefixed.add newIdentDefs(ident"c", n.params[0])
  prefixed.add n.params[1..^1]
  n.params = prefixed
  result.add n

when not strict:
  proc isCpsProc(n: NimNode): bool =
    ## `true` if the node is a routine with our .cps. pragma
    let liftee = bindSym"cps"
    result = n.kind in RoutineNodes and liftee in toSeq(n.pragma)

proc cps_yield*(): Cont {.cpsMagic.} =
  ## yield to pending continuations in the dispatcher before continuing
  addYield(c)

proc cps_sleep*(ms: int): Cont {.cpsMagic.} =
  ## sleep for `ms` milliseconds before continuing
  addTimer(c, ms)
