import std/macros
import std/tables
import std/sets
import std/strutils
import std/sequtils
import std/algorithm

const
  cpsMutant {.booldefine, used.} = false    ## mutate continuations
  cpsDebug {.booldefine, used.} = false
  strict = true        ## only cps operations are strictly cps operations

when (NimMajor, NimMinor) < (1, 3):
  {.fatal: "requires nim-1.3".}

import cps/scopes
import cps/environment
export Continuation, ContinuationProc

type
  NodeFilter = proc(n: NimNode): NimNode

template installLocal(id, env, field) =
  when cpsMutant:
    template id(): untyped = (env(result).field)
  else:
    template id(): untyped = (env(continuation).field)

when not defined(nimdoc): export installLocal # omit from docs

const
  callish = {nnkCall, nnkCommand}           ## all cps call nodes
  cpsish = {nnkYieldStmt, nnkContinueStmt}  ## precede cps calls
  unexiter = {nnkWhileStmt, nnkBreakStmt, nnkContinueStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkElse, nnkStmtList}

when cpsDebug:
  proc numberedLines(s: string): string =
    for n, line in pairs(splitLines(s, keepEol = true)):
      result.add "$1  $2" % [ align($n, 3), line ]

  proc snippet(n: NimNode; name: string): string =
    result &= "----8<---- " & name & "\t" & "vvv"
    result &= "\n" & n.repr.numberedLines & "\n"
    result &= "----8<---- " & name & "\t" & "^^^"

proc filter(n: NimNode; f: NodeFilter): NimNode =
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

proc isCpsCall(n: NimNode): bool =
  # cps foo()
  if len(n) > 0:
    case n.kind
    of callish:
      result = n[0].eqIdent("cps") and n[1].kind in callish
    of cpsish:
      result = n[0].kind in callish
    else:
      result = false

func stripComments(n: NimNode): NimNode =
  ## remove doc statements because that was a stupid idea
  result = copyNimNode n
  for child in items(n):
    if child.kind != nnkCommentStmt:
      result.add stripComments(child)

proc tailCall(e: var Env; p: NimNode; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` via cps call `p`
  # install locals as the 1st argument
  result = newStmtList()
  let locals = e.defineLocals(n)  # goto supplied identifier, not nextGoto!
  var call: NimNode
  case p.kind
  of callish:
    # cps foo()
    call = p[1]
  of cpsish:
    # yield foo()
    call = p[0]
  else:
    assert p.isCpsCall, "does not appear to be a cps call"
    raise newException(Defect, "unexpected cps call type: " & $p.kind)
  call.insert(1, e.maybeConvertToRoot(locals))
  when cpsMutant:
    result.add newAssignment(e.first, call)
    result.add nnkReturnStmt.newNimNode(n).add newEmptyNode()
  else:
    result.add nnkReturnStmt.newNimNode(n).add call

proc tailCall(e: var Env; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` to ident (or nil) `n`
  assert not isCpsCall(n)
  var ret = nnkReturnStmt.newNimNode(n)
  if n.kind == nnkNilLit:
    # we don't want to "fall back" to goto here
    if false and insideCps(e):
      result = tailCall(e, returnTo(e.nextGoto))
    else:
      when cpsMutant:
        ret.add newEmptyNode()
        result.add n
        result.add ret
      else:
        ret.add n
        result = ret
  else:
    # return a statement list including the setup for the locals
    # and the return statement casting those locals to the root type
    result = newStmtList()
    let locals = e.defineLocals(n)
    when cpsMutant:
      ret.add newEmptyNode()
      result.add newAssignment(e.first, e.maybeConvertToRoot(locals))
    else:
      ret.add e.maybeConvertToRoot(locals)
    result.add ret

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
    if len(n) > 0:
      result = n.last.isReturnCall
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
  result = n.kind in {nnkProcDef, nnkTypeSection} and n.hasPragma "cpsLift"

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
    # the locals value is, nominally, a proc param -- or it was.
    # now we just use whatever the macro provided the env
    var locals = env.first
    var body = env.wrapProcBody(locals, n)

    when cpsMutant:
      #result.doc "creating a new proc: " & name.repr
      # add the declaration
      result.add newProc(name = name, pragmas = pragmas,
                         body = newEmptyNode(),
                         params = [newEmptyNode(),
                                   newIdentDefs(locals,
                                                newTree(nnkVarTy,
                                                        env.root))])
      # add the implementation
      result.add newProc(name = name, pragmas = pragmas, body = body,
                         params = [newEmptyNode(),
                                   newIdentDefs(locals,
                                                newTree(nnkVarTy,
                                                        env.root))])
    else:
      # add the declaration
      result.add newProc(name = name, pragmas = pragmas,
                         body = newEmptyNode(),
                         params = [env.root, newIdentDefs(locals,
                                                          env.root)])
      # add the implementation
      result.add newProc(name = name, pragmas = pragmas, body = body,
                         params = [env.root, newIdentDefs(locals,
                                                          env.root)])

proc returnTail(env: var Env; name: NimNode; n: NimNode): NimNode =
  ## either create and return a tail call proc, or return nil
  if len(n) == 0:
    # no code to run means we just `return Cont()`
    when cpsMutant:
      result = nnkReturnStmt.newNimNode(n).add newEmptyNode()
    else:
      result = nnkReturnStmt.newNimNode(n).add newCall(env.root)
  else:
    # create a tail call with the given body
    result = env.makeTail(name, n)

proc callTail(env: var Env; scope: Scope): NimNode =
  ## given a node, either turn it into a
  ## `return call(); proc call() = ...`
  ## or optimize it into a `return subcall()`
  if scope.isNil:
    return nnkReturnStmt.newTree newNilLit()
  var n = scope.node
  case n.kind
  of nnkProcDef:
    # if you already put it in a proc, we should just use it
    result = n
    warning "weirdo"
  of nnkIdent, nnkSym, nnkNilLit:
    # it's an identifier, symbol, or nil; just issue a call of it
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
  of nnkEmpty:
    error "empty node in call tail"
  else:
    # wrap whatever it is and recurse on it
    result = env.callTail newScope(newStmtList(n))
    warning "another weirdo"

proc optimizeSimpleReturn(env: var Env; into: var NimNode; n: NimNode) =
  ## experimental optimization
  var simple: NimNode
  var n = stripComments n
  if asSimpleReturnCall(n.last.last, simple):
    into.doc "possibly unsafe optimization: " & n.repr
    env.optimizeSimpleReturn(into, simple)
  else:
    into.doc "add an unoptimized tail call"
    into.add env.callTail(newScope(n))

proc saften(parent: var Env; input: NimNode): NimNode

proc splitAt(env: var Env; n: NimNode; name: string; i: int): Scope =
  ## split a statement list to create a tail call given
  ## a label prefix and an index at which to split

  if i < n.len-1:
    # if a tail remains after this crap
    # select lines from `i` to `n[^1]`
    var body = newStmtList().add n[i+1 ..< n.len]

    # ensure the proc body is rewritten
    body = env.saften(body)

    var name = genSym(nskProc, name)
    # we'll return a scope holding the tail call to the proc
    result = newScope(n[i], name, env.makeTail(name, body))
    result.goto = env.nextGoto
    result.brake = env.nextBreak
  else:
    # there's nothing left to do in this scope; we're
    # going to just return the next goto (this might be empty)
    result = env.nextGoto

proc saften(parent: var Env; input: NimNode): NimNode =
  ## transform `input` into a mutually-recursive cps convertible form
  result = copyNimNode input

  when cpsDebug and false:
    echo input.snippet "saften"

  # the accumulated environment
  var env =
    if input.kind == nnkStmtList:
      newEnv(parent)
    else:
      parent

  let n = stripComments input
  for i, nc in pairs(n):
    # if it's a cps call,
    if nc.isCpsCall:
      # we want to make sure that a pop inside the after body doesn't
      # return to after itself, so we don't add it to the goto list...
      let after = env.splitAt(n, "after", i)
      result.add env.tailCall(nc, returnTo(after))
      # include the definition for the after proc
      if after.node.kind != nnkSym:
        # XXX: hack
        result.add after.node
      result.doc "post-cps call; time to bail"
      # done!
      return

    if i < n.len-1:
      # if the child is a cps block (not a call), then push a tailcall
      # onto the stack during the saftening of the child
      if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
        withGoto env.splitAt(n, "exit", i):
          result.add env.saften(nc)
          result.doc "add the exit proc definition"
          # we've completed the split, so we're done here
          return

    case nc.kind
    of nnkVarSection, nnkLetSection:
      # add definitions into the environment
      for name, list in env.localSection(nc):
        result.add list

    of nnkForStmt:
      withBreak env.splitAt(n, "brake", i):
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
      if env.insideFor and (len(nc) == 0 or nc[0].isEmpty):
        # unnamed break inside for loop
        result.add nc
      elif env.nextBreak.isNil:
        #assert false, "no break statements to pop"
        result.add env.tailCall(returnTo(env.nextBreak))
      elif (len(nc) == 0 or nc[0].isEmpty):
        result.doc "simple break statement"
        result.add env.tailCall(returnTo(env.nextBreak))
      else:
        result.doc "named break statement to " & repr(nc[0])
        result.add env.tailCall(returnTo(env.namedBreak(nc)))

    of nnkBlockStmt:
      let bp = env.splitAt(n, "brake", i)
      env.addBreak bp
      withGoto bp:
        try:
          result.add env.saften(nc)
          if i < n.len-1 or env.insideCps:
            result.doc "add tail call for block-break proc"
            result.add env.callTail(env.nextBreak)
            return
        finally:
          if not bp.isEmpty:
            discard env.popBreak

    of nnkWhileStmt:
      let w = genSym(nskProc, "loop")
      let bp = env.splitAt(n, "brake", i)
      # we have to assume a break may exist
      let brakeEngaged = true
      if brakeEngaged:
        env.addBreak bp
      # the goto is added here so that it won't appear in the break proc
      env.addGoto nc, w
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
        withGoto env.splitAt(n, "maybe", i):
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
        env.optimizeSimpleReturn(result, x.node)
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


proc cpsXfrmProc*(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style
  when defined(nimdoc): return n

  # enhanced spam before it all goes to shit
  when cpsDebug:
    var orig = copyNimTree(n)
    debugEcho "=== .cps. on " & $n.name & "(original)  ==="
    when defined(cpsTree):
      debugEcho treeRepr(orig)
    else:
      debugEcho repr(orig).numberedLines

  assert n.kind in RoutineNodes

  # check or set the continuation return type
  if not n.params[0].isEmpty:
    error "No return type allowed for now"
  n.params[0] = T

  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()
  var env: Env

  # creating the env with the continuation type,
  # and adding proc parameters to the env
  var first = 1 # index of first param to add to locals
  if len(n.params) > 1 and eqIdent(n.params[0], n.params[1][1]):
    # if the return type matches that of the first argument, we'll assume
    # the user wants that argument name to reflect the continuation
    env = newEnv(n.params[1][0], types, n.params[0])
    inc first

  else:
    when false:
      ##
      ## we don't do this anymore,
      ## -- so that we don't get an `environment misses` error
      ##
      # otherwise, just use a gensym'd "cps"
      env = newEnv(genSym(nskParam, "cps"), types, n.params[0])
    else:
      when cpsMutant:
        env = newEnv(ident"result", types, n.params[0])
      else:
        env = newEnv(ident"continuation", types, n.params[0])
    when false:
      ##
      ## we don't do this anymore,
      ## -- so that you can foo() from outside cps context
      ##

      # and insert it into the proc's params automatically
      n.params.insert(1, env.firstDef)
      inc first

  var preamble = newStmtList()

  # adding the remaining proc params to the environment
  for defs in n.params[first .. ^1]:
    for name, list in env.localSection(defs):
      preamble.add list

  # XXX: this will fail if requires-init
  # now we can insert our `result =`, which includes the proc params
  preamble.insert(0, env.rootResult(ident"result"))
  # template continuation = result; insert it after the `result =`
  when not cpsMutant:
    preamble.insert(1, env.rootTemplate)

  # ensaftening the proc's body and combining it with the preamble
  n.body = newStmtList(preamble, env.saften(n.body))

  # "encouraging" a write of the current accumulating type
  env = env.storeType(force = off)

  # lifting the generated proc bodies
  result = lambdaLift(types, n)

  # spamming the developers
  when cpsDebug:
    debugEcho "=== .cps. on " & $n.name & "(transform) ==="
    when defined(cpsTree):
      debugEcho treeRepr(result)
    else:
      debugEcho repr(result).numberedLines


proc cpsXfrm*(T: NimNode, n: NimNode): NimNode =
  # Perform CPS transformation on a NimNode. This can be a single
  # proc, or a top level stmtList.
  n.expectKind {nnkStmtList, nnkProcDef}

  if n.kind == nnkProcDef:
    return cpsXfrmProc(T, n)

  if n.kind == nnkStmtList:
    result = n.copyNimNode
    for nc in n:
      if nc.kind == nnkProcDef:
        result.add cpsXfrmProc(T, nc)
      else:
        result.add nc.copyNimTree


macro cps*(T: untyped, n: untyped): untyped =
  # I hate doing stuff inside macros, call the proc to do the work
  cpsXfrm(T, n)


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
  # add it to our statement list result
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
