import std/macros
import std/sequtils
import std/algorithm

import cps/spec
import cps/scopes
import cps/environment
export Continuation, ContinuationProc, cpsCall
export cpsDebug, cpsTrace, cpsMutant

template installLocal(id, env, field) =
  when cpsMutant:
    template id(): untyped = (env(result).field)
  else:
    template id(): untyped = (env(continuation).field)

when not defined(nimdoc): export installLocal # omit from docs

const
  callish = {nnkCall, nnkCommand}           ## all cps call nodes
  unexiter = {nnkWhileStmt, nnkBreakStmt, nnkContinueStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkElse, nnkStmtList}

when defined(yourdaywillcomelittleonecommayourdaywillcomedotdotdot):
  const
    cpsish = {nnkYieldStmt, nnkContinueStmt}  ## precede cps calls

proc isCpsCall(n: NimNode): bool =
  ## true if this node holds a call to a cps procedure
  assert not n.isNil
  if len(n) > 0:
    if n.kind in callish:
      let p = n[0].getImpl
      result = p.hasPragma("cpsCall")

proc firstReturn(p: NimNode): NimNode =
  ## find the first return statement within statement lists, or nil
  case p.kind
  of nnkReturnStmt:
    result = p
  of nnkStmtList:
    for child in p.items:
      result = child.firstReturn
      if not result.isNil:
        break
  else:
    result = nil

proc addReturn(p: var NimNode; n: NimNode) =
  ## adds a return statement if none exists; can consume nnkReturnStmt
  ## or wrap other nodes as necessary
  if p.firstReturn.isNil:
    p.add:
      if n.isNil:
        newEmptyNode()
      elif not n.firstReturn.isNil:
        n
      else:
        when cpsMoves:
          nnkReturnStmt.newNimNode(n).add newEmptyNode()
        else:
          nnkReturnStmt.newNimNode(n).add n
  else:
    p.doc "omitted a return of " & repr(n)

template addReturn(e: var Env; p: NimNode; n: untyped) =
  ## adds a return statement with consideration of the env;
  ## this means performing an appropriate rewriteReturn
  addReturn(p, e.rewriteReturn n)

proc tailCall(e: var Env; p: NimNode; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` via cps call `p`
  # install locals as the 1st argument
  assert p.isCpsCall, "does not appear to be a cps call"
  result = newStmtList()
  # goto supplied identifier, not nextGoto!
  let locals = e.defineLocals(result, n)
  p[0] = desym(p[0])              # de-sym the proc target
  p.insert(1, locals)
  when cpsMutant:
    result.add newAssignment(e.first, p)
    result.addReturn newEmptyNode()
  else:
    result.addReturn p

proc tailCall(e: var Env; n: NimNode): NimNode =
  ## compose a tail call from the environment `e` to ident (or nil) `n`
  assert not n.isCpsCall
  var ret = nnkReturnStmt.newNimNode(n)
  if n.kind == nnkNilLit:
    # we don't want to "fall back" to goto here
    if false and insideCps(e):
      result = tailCall(e, returnTo(e.nextGoto))
    else:
      when cpsMutant:
        ret.add newEmptyNode()
        result = nnkStmtList.newNimNode(n).add n
        result.addReturn newEmptyNode()
      else:
        ret.add n
        result = ret
  else:
    # return a statement list including the setup for the locals
    # and the return statement casting those locals to the root type
    result = newStmtList()
    let locals = e.defineLocals(result, n)
    when cpsMutant:
      ret.add newEmptyNode()
      result.add newAssignment(e.first, locals)
    else:
      ret.add locals
    result.add ret

func isReturnCall(n: NimNode): bool =
  ## true if the node looks like a tail call
  if n.isNil:
    return false
  case n.kind
  # simple `return foo()`
  of nnkReturnStmt:
    if n.len > 0:
      if n[0].kind == nnkCall:
        result = true
  # `return foo(); proc foo() = ...`
  of nnkStmtList:
    result = n.firstReturn.isReturnCall
  else:
    discard

func asSimpleReturnCall(n: NimNode; r: var NimNode): bool =
  ## fill `r` with `return foo()` if that is a safe simplification
  var n = n.firstReturn
  result = not n.isNil
  if result:
    r = n

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
  result.add:
    tailCall(env, name)

  var procs = newStmtList()
  if n.kind == nnkProcDef:
    result.doc "adding the proc verbatim"
    procs.add n
  else:
    # the locals value is, nominally, a proc param -- or it was.
    # now we just use whatever the macro provided the env
    var locals = env.first

    # setup the proc body with whatever locals it still needs
    var body = env.prepProcBody(n)

    when cpsMutant:
      #result.doc "creating a new proc: " & name.repr
      # add the declaration
      procs.add newProc(name = name, pragmas = pragmas,
                        body = newEmptyNode(),
                        params = [newEmptyNode(),
                                  newIdentDefs(locals,
                                               newTree(nnkVarTy,
                                                       env.root))])
      # add the implementation
      procs.add newProc(name = name, pragmas = pragmas, body = body,
                        params = [newEmptyNode(),
                                  newIdentDefs(locals,
                                               newTree(nnkVarTy,
                                                       env.root))])
    else:
      # add the declaration
      procs.add newProc(name = name, pragmas = pragmas,
                        body = newEmptyNode(),
                        params = [env.root, newIdentDefs(locals,
                                                         env.root)])
      # add the implementation
      procs.add newProc(name = name, pragmas = pragmas, body = body,
                        params = [env.root, newIdentDefs(locals,
                                                         env.root)])

    when true:
      # immediately push these definitions into the store and just
      # return the tail call
      for p in procs.items:
        env.defineProc p
    else:
      result.add procs

proc callTail(env: var Env; scope: Scope): NimNode =
  ## given a node, either turn it into a
  ## `return call(); proc call() = ...`
  ## or optimize it into a `return subcall()`
  if scope.isNil:
    return nnkReturnStmt.newTree newNilLit()
  var n = scope.node
  case n.kind
  of nnkEmpty:
    raise newException(Defect, "unexpected")
  of nnkProcDef:
    # if you already put it in a proc, we should just use it
    result = n
    warning "weirdo"
  of nnkIdent, nnkSym, nnkNilLit:
    # it's an identifier, symbol, or nil; just issue a call of it
    result = tailCall(env, n)
  of nnkStmtList:
    # maybe we can optimize it out
    if stripComments(n).len == 0:
      result = nnkReturnStmt.newNimNode(n).add:
        # no code to run means we just `return Cont()`
        when cpsMutant:
          newEmptyNode()          # return
        else:
          newCall env.root        # return Cont()
    # FIXME: removed because it broke endWhile
    #elif asSimpleReturnCall(n, result):
    #  discard "the call was stuffed into result"
    elif not n.firstReturn.isNil:
      # just copy the call
      result = newStmtList([doc"verbatim tail call", n])
    else:
      # create a tail call with the given body
      result = env.makeTail(genSym(nskProc, "tail"), n)
  else:
    # wrap whatever it is and recurse on it
    result = env.callTail newScope(newStmtList(n))
    warning "another weirdo"

proc optimizeSimpleReturn(env: var Env; into: var NimNode; n: NimNode) =
  ## experimental optimization
  var simple: NimNode
  if asSimpleReturnCall(n.last.last, simple):
    into.doc "possibly unsafe optimization: " & n.repr
    env.optimizeSimpleReturn(into, simple)
  else:
    into.doc "add an unoptimized tail call"
    let call = env.callTail newScope(n)
    if call.isReturnCall:
      into.addReturn call
    else:
      into.add call

proc saften(parent: var Env; n: NimNode): NimNode

proc procScope(env: var Env; parent: NimNode;
              body: NimNode, name = "scope"): Scope =
    var body = env.prepProcBody(body)
    # ensure the proc body is rewritten
    body = env.saften(body)
    # generate a new name for this proc
    var procName = genSym(nskProc, name)
    # we'll return a scope holding the tail call to the proc
    result = newScope(parent, procName, env.makeTail(procName, body))
    result.goto = env.nextGoto
    result.brake = env.nextBreak

proc splitAt(env: var Env; n: NimNode; i: int; name = "splat"): Scope =
  ## split a statement list to create a tail call given
  ## a label prefix and an index at which to split

  block:
    # if a tail remains after this crap
    if i < n.len-1:
      # select the remaining lines
      var body = newStmtList(n[i+1 ..< n.len])
      if stripComments(body).len > 0:
        # they aren't merely comments, so put them into a proc
        result = procScope(env, n[i], body, name)
        break
    # there's nothing left to do in this scope; we're
    # going to just return the next goto (this might be empty)
    result = env.nextGoto

proc saften(parent: var Env; n: NimNode): NimNode =
  ## transform `input` into a mutually-recursive cps convertible form

  # the accumulated environment
  var env =
    if n.kind == nnkStmtList:
      newEnv(parent)
    else:
      parent

  # first, rewrite any symbols that have been moved to the env
  var n = rewriteSymbolsIntoEnvDotField(parent, n)

  # the result is a copy of the current node
  result = copyNimNode n
  result.doc "saftening..."

  # we're going to iterate over the (non-comment) children
  for i, nc in n.pairs:
    if nc.isNil:
      result.add nc
      continue
    # if it's a cps call,
    if nc.isCpsCall:
      # we want to make sure that a pop inside the after body doesn't
      # return to after itself, so we don't add it to the goto list...
      let after = splitAt(env, n, i, "afterCall")
      result.add:
        tailCall(env, nc):
          returnTo after
      # include the definition for the after proc
      if after.node.kind != nnkSym:
        # add the proc definition and declaration without the return
        for child in after.node.items:
          if child.kind == nnkProcDef:
            result.add child
      # done!
      return

    if i < n.len-1:
      # if the child is a cps block (not a call), then push a tailcall
      # onto the stack during the saftening of the child
      if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
        withGoto env.splitAt(n, i, "done"):
          result.doc "saftening during done"
          result.add env.saften(nc)
          # we've completed the split, so we're done here
          return

    case nc.kind
    of nnkReturnStmt:
      # add a return statement with a potential result assignment
      # stored in the environment
      env.addReturn(result, nc)

    of nnkVarSection, nnkLetSection:
      if nc.len != 1:
        # our rewrite pass should prevent this guard from triggering
        raise newException(Defect, "unexpected section size: " & repr(nc))

      if isCpsCall(nc.last.last):

        #[

        this is a little tricky, so here's a sketch of the transform:

        let foo = cpsProcedure(bar)
        echo foo

        #     ...turns into...

        return Cont(fn: shim, bar: bar)

        proc shim(c: Cont): Cont =
          # we use a new proc here for easier type transition
          cpsProcedure(env(c).bar)

          # this result field was created by cpsProcedure
          return Cont(fh: after, foo: env(c).result)

        proc after(c: Cont): Cont =
          # now we have foo in our env and we continue on...
          echo env(c).foo

        ]#

        # add the local into the env so we can install the field
        var field: NimNode
        for name, list in env.localSection(nc):
          # our rewrite pass should prevent this guard from triggering
          if not field.isNil:
            raise newException(Defect, "too many variable names in section")
          field = name

        # XXX: probably should be i and not i+1
        # skip this node by passing i + 1 to the split
        let after = env.splitAt(n, i + 1, "afterShim")
        # we add it to the goto stack as usual
        env.addGoto(after)

        let call = nc.last[^1]
        let variable = nc.last[0]
        # our field is gensym'd but the strVal should match
        assert variable.strVal == field.strVal

        var body = newStmtList()
        # add the call to the cps proc and let it get saftened as per usual
        body.add call
        let shim = procScope(env, nc, body, "shim")
        assert shim.name != nil

        # now we'll add a tail call to the shim; gratuitous returnTo?
        result.add:
          tailCall(env, shim.name):
            returnTo after

        # let's get the hell outta here before things get any uglier
        return

      elif isCpsBlock(nc.last.last):
        raise newException(Defect,
          "only calls are supported here: " & repr(nc.last))
      else:
        # add definitions into the environment
        for name, list in env.localSection(nc):
          result.add list

    of nnkForStmt:
      withBreak env.splitAt(n, i, "forBreak"):
        nc[^1] = env.saften(nc[^1])
        result.add nc

    of nnkContinueStmt:
      if env.insideFor:
        # if we are inside a for loop, just continue
        result.add nc
      else:
        # else, goto the top of the loop
        result.add:
          tailCall env:
            returnTo env.topOfWhile

    of nnkBreakStmt:
      if env.insideFor and (len(nc) == 0 or nc[0].isEmpty):
        # unnamed break inside for loop
        result.add nc
      elif env.nextBreak.isNil:
        #assert false, "no break statements to pop"
        result.add:
          tailCall env:
            returnTo env.nextBreak
      elif (len(nc) == 0 or nc[0].isEmpty):
        result.doc "simple break statement"
        result.add:
          tailCall env:
            returnTo env.nextBreak
      else:
        result.doc "named break statement to " & repr(nc[0])
        result.add:
          tailCall env:
            returnTo namedBreak(env, nc)

    of nnkBlockStmt:
      let bp = env.splitAt(n, i, "blockBreak")
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
      let bp = env.splitAt(n, i, "endWhile")
      env.addBreak bp
      # the goto is added here so that it won't appear in the break proc
      env.addGoto nc, w
      try:
        var loop = newStmtList()
        result.doc "add tail call for while loop with body " & $nc[1].kind
        # process the loop itself, and only then, turn it into a tail call
        loop.add newIfStmt((nc[0], env.saften(nc[1])))
        discard env.popGoto # the loop rewind was added to the body
        loop.doc "add tail call for break proc: " & repr(bp.name)
        loop.add env.callTail(bp)
        # this will rewrite the loop using filter, so...  it's destructive
        result.add env.makeTail(w, loop)
        return
      finally:
        discard env.popBreak

    of nnkIfStmt:
      # if any `if` clause is a cps block, then every clause must be
      # if we've pushed any goto or breaks, then we're already in cps
      if nc.isCpsBlock:
        withGoto env.splitAt(n, i, "ifClause"):
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
        withGoto env.splitAt(n, i, "ifClause"):
          result.doc "boring if clause"
          result.add env.saften(nc)

    # not a statement cps is interested in
    else:
      result.add env.saften(nc)

    # if the child isn't last,
    if i < n.len-1:
      # and it's a cps call,
      if nc.isCpsCall or nc.isCpsBlock:
        let x = env.splitAt(n, i, "procTail")
        env.optimizeSimpleReturn(result, x.node)
        # the split is complete
        return

  if result.kind == nnkStmtList and n.kind in returner:
    # let a for loop, uh, loop
    if not env.insideFor:
      if not result.firstReturn.isNil:
        result.doc "omit return call from " & $n.kind
      elif env.nextGoto.kind != nnkNilLit:
        result.doc "adding return call to " & $n.kind
        result.add:
          env.tailCall:
            returnTo env.nextGoto
      else:
        discard "nil return; no remaining goto for " & $n.kind

proc normalizingRewrites(n: NimNode): NimNode =
  proc rewriteIdentDefs(n: NimNode): NimNode =
    ## Rewrite an identDefs to ensure it has three children.
    if n.kind == nnkIdentDefs:
      if n.len == 2:
        n.add newEmptyNode()
      elif n[1].isEmpty:          # add explicit type symbol
        n[1] = getType n[2]
      result = n

  proc rewriteVarLet(n: NimNode): NimNode =
    ## Rewrite a var|let section of multiple identDefs
    ## into multiple such sections with well-formed identDefs
    if n.kind in {nnkLetSection, nnkVarSection}:
      result = newStmtList()
      for child in n.items:
        case child.kind
        of nnkVarTuple:
          # a new section with a single rewritten identdefs within
          # for each symbol in the VarTuple statement
          for i, value in child.last.pairs:
            result.add:
              newNimNode(n.kind, n).add:
                rewriteIdentDefs:  # for consistency
                  newIdentDefs(child[i], getType(value), value)
        of nnkIdentDefs:
          # a new section with a single rewritten identdefs within
          result.add:
            newNimNode(n.kind, n).add:
              rewriteIdentDefs child
        else:
          raise newException(Defect, treeRepr(child) & "\nunexpected")

  proc rewriteHiddenAddrDeref(n: NimNode): NimNode =
    ## Remove nnkHiddenAddr/Deref because they cause the carnac bug
    case n.kind
    of nnkHiddenAddr, nnkHiddenDeref:
      result = n[0]
    else: discard

  case n.kind
  of nnkIdentDefs:
    rewriteIdentDefs n
  of nnkLetSection, nnkVarSection:
    rewriteVarLet n
  of nnkHiddenAddr, nnkHiddenDeref:
    rewriteHiddenAddrDeref n
  else:
    nil

proc cloneProc(n: NimNode): NimNode =
  ## create a copy of a typed proc which satisfies the compiler
  assert n.kind == nnkProcDef
  result = nnkProcDef.newTree(
    ident($n.name),
    newEmptyNode(),
    newEmptyNode(),
    n.params,
    newEmptyNode(),
    newEmptyNode(),
    copy n.body)

proc cpsXfrmProc*(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style

  # enhanced spam before it all goes to shit
  when cpsDebug:
    let info = lineInfoObj(n)
    debugEcho "=== .cps. on " & $n.name & "(original)  === " & $info
    when defined(cpsTree):
      debugEcho treeRepr(n)
    else:
      debugEcho repr(n).numberedLines(info.line)

  # strip hidden converters early
  var n = unhide n

  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()
  var env: Env
  var booty: NimNode

  # creating the env with the continuation type,
  # and adding proc parameters to the env
  var first = 1 # index of first param to add to locals
  when cpsMutant:
    env = newEnv(ident"result", types, T)
  else:
    env = newEnv(ident"continuation", types, T)

  # assign the return type if necessary
  if not n.params[0].isEmpty:
    env.setReturn n.params[0]
    n.params[0] = newEmptyNode()

  ## the preamble for the proc is the space above the user-supplied body.
  ## here we setup the locals, mapping the proc parameters into our
  ## continuation.
  var preamble = newStmtList()

  ## what we DO do,
  ## -- is to write a copy of the proc to the result; this will serve
  ## as the "bootstrap" which performs alloc of the continuation before
  ## calling the cps version of the proc

  block:
    let name = env.first      # ident"result" or ident"continuation", etc.

    booty = cloneProc n
    booty.body = newStmtList()

    # add the remaining proc params to the environment
    # and set them up in the bootstrap at the same time
    for defs in n.params[first .. ^1]:
      for name, list in env.localSection(defs):
        preamble.add list

    # XXX: let the user supply the trampoline?
    # add a trampoline to resolve the continuation
    when not cpsMutant:
      when cpsTrampBooty:
        # if we're not storing to result, we need a variable
        booty.body.add nnkVarSection.newTree newIdentDefs(name, T,
                                                          newEmptyNode())
        # XXX: this may fail if requires-init
        # now we can insert our `result =`, which includes the proc params
        booty.body.add env.rootResult(name, booty.name)

        let fn = newDotExpr(name, ident"fn")

        # we'll construct the while statement's body first
        var wh = newStmtList()
        when cpsDebug:
          wh.add nnkCommand.newTree(ident"echo", "bootstrap trampoline".newLit)
        wh.add newAssignment(name, newCall(fn, name))

        # if a result is expected, copy it out when only the fn is nil
        if not n.params[0].isEmpty:
          wh.add newIfStmt((infix( infix(name, "!=", newNilLit()), "and",
                            infix(fn, "==", newNilLit())),
                           newAssignment(ident"result", env.get)))

        # compose the complete trampoline with the while and its guards
        wh = nnkWhileStmt.newTree(
          infix( infix(name, "!=", newNilLit()), "and",
                 infix(fn, "!=", newNilLit())), wh)

        # add the trampoline to the bootstrap
        booty.body.add wh
      else:
        booty.params[0] = T
        # XXX: this may fail if requires-init
        # now we can insert our `result =`, which includes the proc params
        booty.body.add env.rootResult(ident"result", booty.name)

  # we can't mutate typed nodes, so copy ourselves
  n = cloneProc n

  # and do some pruning of these typed trees
  for p in [booty, n]:
    p.name = desym(p.name)
    while len(p) > 7:
      p.del(7)

  # the bootstrap may not have a continuation as its first argument,
  # but we know that we do, because we insert it here ;-)
  n.params.insert(1, env.firstDef)
  inc first   # gratuitous tracking for correctness

  # install our return type in the clone
  n.params[0] = T

  # now remove any other arguments (for now)
  while len(n.params) > 2:
    del(n.params, 2)

  # make the body easier for us to consume
  n.body = filter(n.body, normalizingRewrites)

  # perform sym substitutions (or whatever)
  n.body = env.prepProcBody(newStmtList n.body)

  # ensaftening the proc's body
  n.body = env.saften(n.body)

  when false:
    if len(preamble) > 0:           # if necessary, insert the preamble
      n.body.insert(0, preamble)    # ahead of the rest of the body

  # add in a pragma so other cps macros can identify this as a cps call
  n.addPragma ident"cpsCall"

  # "encouraging" a write of the current accumulating type
  env = env.storeType(force = off)

  # Araq: "learn how to desemantic your ast, knuckleheads"
  n.body = replacedSymsWithIdents(n.body)
  types = replacedSymsWithIdents(types)
  booty = replacedSymsWithIdents(booty)

  # lifting the generated proc bodies
  result = lambdaLift(types, n)

  # adding in the bootstrap
  result.add booty

  # spamming the developers
  when cpsDebug:
    debugEcho "=== .cps. on " & $n.name & "(transform) === " & $info
    when defined(cpsTree):
      debugEcho treeRepr(result)
    else:
      debugEcho repr(result).numberedLines(info.line)

proc workaroundRewrites(n: NimNode): NimNode =
  proc workaroundSigmatchSkip(n: NimNode): NimNode =
    if n.kind in nnkCallKinds:
      # We recreate the arguments nodes here, to set their .typ to nil
      # so that sigmatch doesn't decide to skip it
      result = n
      for i in 1..<result.len:
        if result[i].kind notin AtomicNodes:
          var arg = newNimNode(result[i].kind, result[i])
          for child in result[i].items:
            arg.add child
          result[i] = arg

  result = filter(n, workaroundSigmatchSkip)

proc cpsXfrm*(T: NimNode, n: NimNode): NimNode =
  # Perform CPS transformation on a NimNode. This can be a single
  # proc, or a top level stmtList.
  case n.kind
  of nnkProcDef:
    result = cpsXfrmProc(T, n)
  of nnkStmtList:
    result = copyNimNode n
    for nc in n.items:
      result.add cpsXfrm(T, nc)
  else:
    result = copy n
  result = workaroundRewrites(result)

macro cps*(T: typed, n: typed): untyped =
  # I hate doing stuff inside macros, call the proc to do the work
  when defined(nimdoc):
    result = n
  else:
    result = cpsXfrm(T, n)

macro cpsMagic*(n: untyped): untyped =
  ## upgrade cps primitives to generate errors out of context
  ## and take continuations as input inside {.cps.} blocks
  expectKind(n, nnkProcDef)
  result = newStmtList()

  # ensure that .cpsCall. is added to the copies of the proc
  n.addPragma ident"cpsCall"

  # create a version of the proc that pukes outside of cps context
  var m = copyNimTree n
  let msg = $n.name & "() is only valid in {.cps.} context"
  m.params[0] = newEmptyNode()
  when cpsMagicExists:
    del(m.params, 1)
  m.body = newStmtList()
  # add a documentation comment if possible
  if len(n.body) > 0 and n.body[0].kind == nnkCommentStmt:
    m.body.add n.body[0]
  when false:
    m.addPragma newColonExpr(ident"error", msg.newLit)
    m.body.add nnkDiscardStmt.newNimNode(n).add newEmptyNode()
  elif defined(release) and not defined(cpsDebug):
    m.body.add nnkPragma.newNimNode(n).add newColonExpr(ident"warning",
                                                        msg.newLit)
  elif false:
    m.body.add nnkCall.newNimNode(n).newTree(ident"error", msg.newLit)
  # add it to our statement list result
  result.add m

  when not defined(nimdoc):
    # manipulate the primitive to take its return type as a first arg
    when not cpsMagicExists:
      n.params.insert(1, newIdentDefs(ident"c", n.params[0]))
    result.add n

when false:
  proc isCpsProc(n: NimNode): bool =
    ## `true` if the node is a routine with our .cps. pragma
    let liftee = bindSym"cps"
    result = n.kind in RoutineNodes and liftee in toSeq(n.pragma)
