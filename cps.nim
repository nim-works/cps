import std/macros
import std/tables
import std/sets
import std/strutils
import std/sequtils
import std/algorithm

const
  cpsDebug {.booldefine.} = false
  strict = true        ## only cps operations are strictly cps operations
  comments = cpsDebug  ## embed comments within the transformation
  #lifting = true       ## whether to lift generated symbols to top-level

when (NimMajor, NimMinor) < (1, 3):
  {.fatal: "requires nim-1.3".}

import cps/environment

type
  Continuation* = concept c
    c.fn is ContinuationProc[Continuation]
    c is ref object
    c of RootObj

  ContinuationProc*[T] = proc(c: T): T {.nimcall.}

  NodeFilter = proc(n: NimNode): NimNode

const
  unexiter = {nnkWhileStmt, nnkBreakStmt, nnkContinueStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkElse, nnkStmtList}


proc filter(n: NimNode; f: NodeFilter): NimNode =
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

proc isCpsCall(n: NimNode): bool =
  # cps foo()
  result = n.kind == nnkCommand and
           n[0].eqIdent("cps") and
           n[1].kind in {nnkCall, nnkCommand}

proc maybeConvertToRoot(e: Env; locals: NimNode): NimNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    newDotExpr(locals, e.root)
  else:
    locals

func stripComments(n: NimNode): NimNode =
  ## remove doc statements because that was a stupid idea
  result = copyNimNode n
  for child in items(n):
    if child.kind != nnkCommentStmt:
      result.add stripComments(child)

func returnTo(n: NimNode): NimNode {.deprecated.} =
  ## given a goto, find the ident/sym it's pointing to
  let n = stripComments n
  case n.kind
  of nnkIdent, nnkSym, nnkNilLit:
    result = n
  of nnkCall, nnkObjConstr, nnkExprColonExpr:
    result = returnTo(n[1])
  else:
    result = returnTo(n[0])

proc tailCall(e: var Env; p: NimNode; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` via cps call `p`
  assert p.isCpsCall
  # install locals as the 1st argument
  result = newStmtList()
  let locals = e.defineLocals(returnTo(e.nextGoto))
  p[1].insert(1, e.maybeConvertToRoot(locals))
  result.add nnkReturnStmt.newNimNode(n).add p[1]

proc tailCall(e: var Env; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` to ident (or nil) `n`
  assert not isCpsCall(n)
  var ret = nnkReturnStmt.newNimNode(n)
  if n.kind == nnkNilLit:
    if insideCps(e):
      result = tailCall(e, returnTo(e.nextGoto))
    else:
      ret.add n
      result = ret
  else:
    # return a statement list including the setup for the locals
    # and the return statement casting those locals to the root type
    result = newStmtList()
    let locals = e.defineLocals(n)
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
  ## `true` if the block `n` contains a cps call anywhere at all;
  ## this is used to figure out if a block needs tailcall handling...
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
      # it's a CPS block if ANY call exists inside
      #result = (i != n.len-1 and nc.isCpsCall) or nc.isCpsBlock
      result = nc.isCpsCall or nc.isCpsBlock
      if result:
        break
  else:
    discard

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

proc cmpKind(a, b: NimNode): int =
  if a.kind == b.kind:
    if a.kind == nnkProcDef:
      result = cmp(a.body.len, b.body.len)  # work around decl bug
    else:
      result = 0
  elif a.kind == nnkProcDef:
    result = 1
  else:
    result = -1

proc lambdaLift(lifted: NimNode; n: NimNode): NimNode =
  ## lift ast tagged with cpsLift pragma to top-level and omit the pragma

  # pull the liftable procs out of the input; the result is, uh, `result`
  proc liften(n: NimNode): NimNode =
    if n.isLiftable:
      lifted.add filter(n.stripPragma "cpsLift", liften)
      result = newEmptyNode()
  result = filter(n, liften)

  # we currently install proc declarations during proc construction
  when false:
    # clone the proc declarations
    proc declaren(n: NimNode): NimNode =
      if n.kind == nnkProcDef:
        var decl = copyNimTree(n)
        decl.body = newEmptyNode()
        result = newStmtList(decl, n)
    var lifted = filter(lifted, declaren)

  # flatten the series of declarations
  var flatter: seq[NimNode]
  proc flatten(n: NimNode): NimNode {.nosinks.} =
    if n.kind != nnkStmtList:
      flatter.add n
      result = newEmptyNode()
  discard filter(lifted, flatten)

  # sort the declarations so that we predeclare types and procs
  sort(flatter, cmpKind)

  # the result is the lifted stuff followed by the original code
  result = newStmtList(flatter.newStmtList, result)

proc makeTail(env: var Env; name: NimNode; n: NimNode): NimNode =
  ## make a tail call and put it in a single statement list;
  ## this will always create a tail call proc and call it
  let pragmas = nnkPragma.newTree bindSym"cpsLift"
  result = newStmtList()
  result.doc "new tail call: " & name.repr
  result.add env.tailCall(name)
  if n.kind == nnkProcDef:
    result.doc "adding the proc verbatim"
    result.add n
  else:
    var body = newStmtList(n)
    var locals = genSym(nskParam, "locals")
    for name, asgn in localRetrievals(env, locals):
      body.insert(0, asgn)
    body.insert(0, doc "installing locals for env " & $env.identity)
    result.doc "creating a new proc: " & name.repr
    # add the declaration
    when false: # this should work, but it provokes ICE...
      result.add newProc(name = name, pragmas = pragmas, body = newEmptyNode(),
                         params = [env.root, newIdentDefs(locals, env.root)])
    else:
      result.add newProc(name = name, pragmas = pragmas,
                         params = [env.root, newIdentDefs(locals, env.root)])
    # add the implementation
    result.add newProc(name = name, pragmas = pragmas, body = body,
                       params = [env.root, newIdentDefs(locals, env.root)])

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
  of nnkNilLit:
    # if it's nil, let the tailCall() issue the return normally
    result = env.tailCall(n)
  of nnkIdent, nnkSym:
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
      result = env.returnTail(genSym(nskProc, "tail"), n)
  else:
    # wrap whatever it is and recurse on it
    result = env.callTail(newStmtList(n))

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

proc saften(penv: var Env; input: NimNode): NimNode

proc splitAt(env: var Env; n: NimNode; name: string; i: int): NimNode =
  ## split a statement list to create a tail call given
  ## a label prefix and an index at which to split
  let label = genSym(nskProc, name)
  var body = newStmtList()
  body.doc "split as " & label.repr & " at index " & $i
  result = newStmtList()
  if i < n.len-1:
    body.add n[i+1 ..< n.len]
    body = env.saften(body)
    result.doc "split at: " & name
    result.add env.makeTail(label, body)
  else:
    result.doc "split at: " & name & " - no body left"
    if returnTo(env.nextGoto).kind == nnkNilLit:
      warning "nil goto at end of split"
    result.add env.callTail returnTo(env.nextGoto)

proc saften(penv: var Env; input: NimNode): NimNode =
  ## transform `input` into a mutually-recursive cps convertible form
  result = copyNimNode input

  # the accumulated environment
  var env =
    if input.kind == nnkStmtList:
      newEnv(penv)
    else:
      penv

  let n = stripComments input
  for i, nc in pairs(n):
    # if the child is a cps block (not a call), then push a tailcall
    # onto the stack during the saftening of the child
    if nc.isCpsCall:
      withGoto nc.kind, env.splitAt(n, "after", i):
        result.add env.tailCall(nc, returnTo(env.nextGoto))
        result.doc "post-cps call; time to bail"
        return
      assert false, "unexpected"

    if i < n.len-1:
      if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
        withGoto nc.kind, env.splitAt(n, "exit", i):
          result.add env.saften(nc)
          result.doc "add the exit proc definition"
          # we've completed the split, so we're done here
          return

    case nc.kind
    of nnkVarSection, nnkLetSection:
      # add definitions into the environment
      env.add nc
      # include the section normally (for now)
      result.add nc

    of nnkForStmt:
      let bp = env.splitAt(n, "brake", i)
      env.addBreak nc.kind, bp
      nc[^1] = env.saften(nc[^1])
      result.add nc

    of nnkContinueStmt:
      if env.insideFor:
        # if we are inside a for loop, just continue
        result.add nc
      else:
        # else, goto the top of the loop
        result.add env.tailCall returnTo(env.topOfWhile)

    of nnkBreakStmt:
      # FIXME: does not support named break yet
      if env.insideFor:
        result.add nc
      else:
        if env.nextBreak.kind != nnkNilLit:
          result.doc "simple break statement"
          result.add env.tailCall(returnTo(env.nextBreak))
        else:
          assert false, "no break statements to pop"

    of nnkBlockStmt:
      let bp = env.splitAt(n, "brake", i)
      env.addBreak nc.kind, bp
      withGoto nc.kind, bp:
        try:
          result.add env.saften(nc)
          if i < n.len-1 or env.insideCps:
            result.doc "add tail call for block-break proc"
            result.add env.callTail(env.nextBreak)
            return
        finally:
          discard env.popBreak

    of nnkWhileStmt:
      let w = genSym(nskProc, "loop")
      let brakeEngaged = true
      if brakeEngaged:
        env.addBreak nc.kind, env.splitAt(n, "brake", i)
      # the goto is added here so that it won't appear in the break proc
      env.addGoto nc.kind, w
      try:
        var loop = newStmtList()
        result.doc "add tail call for while loop with body " & $nc[1].kind
        result.add env.makeTail(w, loop)
        loop.add newIfStmt((nc[0], newStmtList(env.saften(nc[1]))))
        discard env.popGoto # the loop rewind was added to the body
        if brakeEngaged:
          loop.doc "add tail call for break proc"
          loop.add env.callTail(env.nextBreak)
          return
      finally:
        if brakeEngaged:
          discard env.popBreak

    of nnkIfStmt:
      # if any `if` clause is a cps block, then every clause must be
      # if we've pushed any goto or breaks, then we're already in cps
      if nc.isCpsBlock:
        withGoto nc.kind, env.splitAt(n, "maybe", i):
          result.doc "add if body"
          result.add env.saften(nc)
          # the split is complete
          return
        # if we didn't return, then this is the last block
        assert false, "unexpected"
        result.add nc
      elif insideCps(env):
        result.doc "if body inside cps"
        result.add env.saften(nc)
      else:
        result.doc "boring if body"
        result.add env.saften(nc)

    else:
      result.add env.saften(nc)

    # if the child isn't last,
    if i < n.len-1:
      # and it's a cps call,
      if nc.isCpsCall or nc.isCpsBlock:
        let x = env.splitAt(n, "tail", i)
        env.optimizeSimpleReturn(result, x)
        # the split is complete
        return

  if result.kind == nnkStmtList and n.kind in returner:
    # let a for loop, uh, loop
    if not env.insideFor:
      let duh = stripComments result
      if len(duh) > 0 and isReturnCall(duh.last):
        result.doc "omit return call from " & $n.kind
      elif env.nextGoto.kind != nnkNilLit:
        result.doc "adding return call to " & $n.kind
        result.add env.tailCall returnTo(env.nextGoto)
      else:
        discard "nil return; no remaining goto for " & $n.kind

macro cps*(n: untyped): untyped =
  ## rewrite the target procedure in Continuation-Passing Style
  when defined(nimdoc): return n

  # enhanced spam before it all goes to shit
  when cpsDebug:
    var orig = copyNimTree(n)
    debugEcho "=== .cps. on " & $n.name & "(original)  ==="
    debugEcho repr(orig)

  assert n.kind in RoutineNodes
  if n.params[0].isEmpty:
    error "provide a continuation return type"

  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()
  var env = newEnv(types, n.params[0])

  # adding the proc params to the environment
  for defs in n.params[1..^1]:
    env.add defs

  # ensaftening the proc's body
  n.body = env.saften(n.body)

  # lifting the generated proc bodies
  result = lambdaLift(types, n)

  # spamming the developers
  when cpsDebug:
    debugEcho "=== .cps. on " & $n.name & "(transform) ==="
    debugEcho repr(result)

when false:
  macro cps*(c: typed; n: typed): untyped =
    var
      safe = xfrm(n, c)
      decls = lambdaLift(safe)
    result = newStmtList()
    if len(decls) > 0:
      result.add decls
    result.add safe
    assert false

macro cpsMagic*(n: untyped): untyped =
  ## upgrade cps primitives to generate errors out of context
  ## and take continuations as input inside {.cps.} blocks
  expectKind(n, nnkProcDef)
  result = newStmtList()

  # create a version of the proc that pukes outside of cps context
  var m = copyNimTree n
  let msg = $n.name & "() is only valid in {.cps.} context"
  m.params[0] = newEmptyNode()
  m.body = newStmtList(n.body[0])
  when false:
    m.addPragma newColonExpr(ident"error", msg.newLit)
    m.body.add nnkDiscardStmt.newNimNode(n).add newEmptyNode()
  elif true:
    m.body.add nnkPragma.newNimNode(n).add newColonExpr(ident"warning",
                                                        msg.newLit)
  else:
    m.body.add nnkCall.newNimNode(n).newTree(ident"error", msg.newLit)
  result.add m

  when not defined(nimdoc):
    # manipulate the primitive to take its return type as a first arg
    n.params.insert(1, newIdentDefs(ident"c", n.params[0]))
    result.add n

when not strict:
  proc isCpsProc(n: NimNode): bool =
    ## `true` if the node is a routine with our .cps. pragma
    let liftee = bindSym"cps"
    result = n.kind in RoutineNodes and liftee in toSeq(n.pragma)
