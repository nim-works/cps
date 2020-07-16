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

when NimMinor < 3:
  {.fatal: "requires nim-1.3".}

import environment
import eventqueue

export Cont, cpsLift

type
  Operation = enum    ## "native" cps operations
    Yield
    Sleep
    Wait
    Io
    Attach
    Detach
    Spawn
    Signal
    SignalAll

const
  unexiter = {nnkWhileStmt, nnkBreakStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkElse, nnkStmtList}

  cpsContext = {Yield .. Detach}
  cpsAnywhere = {Spawn .. SignalAll}
assert cpsContext + cpsAnywhere == {Operation.low .. Operation.high}
assert cpsContext * cpsAnywhere == {}

func makeIdent(op: Operation): NimNode =
  ident("cps_" & $op)

proc makeIdents(): seq[NimNode] {.compileTime.} =
  for op in items(Operation):
    result.add ident("cps_" & $op)
let cpsIdents {.compileTime.} = mapIt(toSeq(Operation.items),
                                      it.makeIdent)

template cps_yield*() = echo "yield"
template cps_sleep*() = echo "sleep"

proc tailCall(e: Env; n: NimNode): NimNode =
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
  when comments:
    newCommentStmtNode(s)
  else:
    newEmptyNode()

proc doc(n: var NimNode; s: string) =
  when comments:
    if n.kind == nnkStmtList:
      n.add doc(s)

func stripComments(n: NimNode): NimNode =
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
  let n = n.stripComments
  case n.kind
  # simple `return foo()`
  of nnkReturnStmt:
    if n.len > 0:
      if n[0].kind == nnkCall:
        result = true
  # `return foo(); proc foo() = ...`
  of nnkStmtList:
    result = case n.len
    of 1:
      n[0].isReturnCall
    of 2:
      n[0].isReturnCall and n[1].kind == nnkProcDef and n[1].name == n[0][0][0]
    else:
      false
  else: discard

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
    assert false, "wut"

proc filterPragma(ns: seq[NimNode], liftee: NimNode): NimNode =
  var pragmas = nnkPragma.newNimNode
  for p in filterIt(ns, it != liftee):  # pragmas; sorry!
    pragmas.add p
  if len(pragmas) > 0:
    pragmas
  else:
    newEmptyNode()

proc stripPragma(n: NimNode; s: static[string]): NimNode =
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

func isCpsType(n: NimNode): bool =
  ## it's a liftable continuation typedef
  result = n.kind == nnkTypeSection and n.isLiftable

proc isCpsIdent(n: NimNode): bool =
  ## it's a cps identifier
  result = n.kind == nnkIdent and anyIt(cpsIdents, eqIdent(n, it))

proc isCpsCall(n: NimNode): bool =
  ## it's a call to a cps routine
  result = n.kind == nnkCall and n[0].isCpsIdent
  when not strict:
    result = result or n.kind in RoutineNodes and n.isLiftable
    result = result or n.isCpsProc

# Every block with calls to CPS procs is a CPS block
proc isCpsBlock(n: NimNode): bool =
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
  result = genSym(nskProc, ident = s)

proc foldTailCalls(n: NimNode): NimNode =
  ## this may optimize a `proc foo() = return bar()` to `return bar()`
  result = n
  if n.kind == nnkProcDef:
    if not asSimpleReturnCall(n, result):
      if isReturnCall(n.last):
        result = newStmtList()
        result.doc "optimized proc into tail call"
        result.add n.last
  else:
    assert false, "not a proc"

proc liften(n: var NimNode): NimNode =
  ## lift cps procs to top-level
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

proc xfrm(prc: NimNode; n: NimNode = nil; c: NimNode = nil): NimNode =
  assert prc.kind in RoutineNodes
  if n == nil:
    return xfrm(prc, prc.last, c = prc.params[0])

  # make sure we have a valid continuation type to work with
  assert c != nil
  if c.kind == nnkEmpty:
    error "provide a continuation return type on " & $prc.name
  else:
    hint "continuation return type " & repr(c)

  var env = newEnv(c)

  # identifiers of future break or return targets
  var goto: seq[NimNode]
  var breaks: seq[NimNode]

  func insideCps(): bool = len(goto) > 0 or len(breaks) > 0

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
    ## given a node, either turn it into a `return call(); proc call() = ...`
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

  func next(ns: seq[NimNode]): NimNode =
    if len(ns) == 0:
      newNilLit()
    else:
      ns[^1]

  template withGoto(n: NimNode; body: untyped): untyped =
    if len(n.stripComments) > 0:
      add(goto, n)
      try:
        body
      finally:
        discard pop(goto)
    else:
      body

  proc optimizeSimpleReturn(env: var Env; into: var NimNode; n: NimNode) =
    var simple: NimNode
    var n = n.stripComments
    if asSimpleReturnCall(n.last.last, simple):
      into.doc "possibly unsafe optimization: " & n.repr
      env.optimizeSimpleReturn(into, simple)
    else:
      into.doc "add an unoptimized tail call"
      into.add env.callTail(n)

  # Make sure all CPS calls become tail calls
  proc saften(penv: var Env; input: NimNode): NimNode =
    # xfrm the input into a mutually-recursive "cps convertible form".
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
      #[

      cute, but not yet.

      of nnkIdent:
        if isCpsIdent(nc):
          result.add newCall(nc)
        elif isCpsIdent(ident("cps_" & nc.strVal)):
          result.add newCall(ident("cps_" & nc.strVal))
        else:
          result.add nc

      ]#
      of nnkYieldStmt:
        if insideCps() or (i < n.len-1 and nc.isCpsCall):
          result.add newCall(ident"cps_yield")
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

  var safe = env.saften(n)
  when lifting:
    safe = newStmtList(safe) # wrap it for liften
    let decls = liften(safe)
    if len(decls) == 0:
      prc.body = n
      result = prc
    else:
      prc.body = safe
      result = newStmtList(decls, prc)
  else:
    prc.body = safe
    result = prc

macro cps*(n: untyped) =
  assert n.kind in RoutineNodes
  result = n
  when not defined(nimdoc):
    result = xfrm(result)
    echo repr(result)

proc isCpsProc(n: NimNode): bool =
  let liftee = bindSym"cps"
  result = n.kind in RoutineNodes and liftee in toSeq(n.pragma)
