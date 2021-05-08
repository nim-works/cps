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
  returner = {nnkBlockStmt, nnkOfBranch, nnkElifBranch, nnkElse}

when defined(yourdaywillcomelittleonecommayourdaywillcomedotdotdot):
  const
    cpsish = {nnkYieldStmt, nnkContinueStmt}  ## precede cps calls

proc isCpsCall(n: NimNode): bool =
  ## true if this node holds a call to a cps procedure
  assert not n.isNil
  if len(n) > 0:
    if n.kind in callish:
      let callee = n[0]
      # all cpsCall are normal functions called via a generated symbol
      if not callee.isNil and callee.kind == nnkSym:
        result = callee.getImpl.hasPragma("cpsCall")

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

proc asSimpleReturnCall(n: NimNode; r: var NimNode): bool =
  ## fill `r` with `return foo()` if that is a safe simplification
  var n = n.firstReturn
  result = not n.isNil
  if result:
    r = n

proc isCpsBlock(n: NimNode): bool =
  ## `true` if the block `n` contains a cps call anywhere at all;
  ## this is used to figure out if a block needs tailcall handling...
  case n.kind
  of nnkElse, nnkElifBranch, nnkOfBranch:
    result = n.last.isCpsBlock
  of nnkStmtList, nnkIfStmt, nnkCaseStmt:
    for n in n.items:
      if n.isCpsBlock:
        return true
  of callish:
    result = n.isCpsCall
  else:
    result = false

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
    result = n.errorAst "empty scope node in callTail()"
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

func hasContinuation(n: NimNode): bool =
  ## determine whether `n` will continue into an another control flow
  ## this is only valid in the body of a continuation
  doAssert n.kind == nnkStmtList
  if n.len > 0:
    case n.last.kind
    of nnkReturnStmt:
      true
    of nnkStmtList:
      n.last.hasContinuation
    else:
      false
  else:
    false

proc replacePending(n, replacement: NimNode): NimNode

proc makeContProc(name, cont, body: NimNode): NimNode =
  ## creates a continuation proc from with `name` using continuation `cont`
  ## with the given body.
  let
    contParam = desym cont
    contType = getTypeInst cont

  result = newProc(name, [contType, newIdentDefs(contParam, contType)])
  # make it something that we can consume and modify
  result.body = normalizingRewrites body
  # replace any `cont` within the body with the parameter of the newly made proc
  result.body = resym(result.body, cont, contParam)
  # if this body don't have any continuing control-flow
  if not result.body.hasContinuation:
    # annotate it so that outer macros can fill in as needed
    result.body.add newTree(
      nnkPragma,
      bindSym"cpsPending"
    )
  # tell cpsFloater that we want this to be lifted to the top-level
  result.addPragma bindSym"cpsLift"

func tailCall(cont, to: NimNode, via: NimNode = nil): NimNode =
  ## Produce a tail call to `to` with `cont` as the continuation
  ##
  ## If `to` is nil, this is the last continuation and will be
  ## set to nil.
  ##
  ## If `via` is not nil, it is expected to be a cps jumper call
  if not via.isNil:
    doAssert via.kind == nnkCall

  result = newStmtList()

  # if `to` is nil, this should be the last leg and the next continuation
  # should be nil.
  let cont = if to.isNil: newNilLit() else: cont
  if not to.isNil:
    # progress to the next function in the continuation
    result.add:
      newAssignment(newDotExpr(cont, ident"fn"), to)

  if via.isNil:
    result.add:
      nnkReturnStmt.newTree:
        cont
  else:
    let jump = copyNimTree via
    # desym the jumper, it is currently sem-ed to the variant that
    # doesn't take a continuation
    jump[0] = desym jump[0]
    # insert our continuation as the first parameter
    jump.insert(1, cont)
    result.add:
      nnkReturnStmt.newTree:
        jump

proc workaroundRewrites(n: NimNode): NimNode

macro cpsJump(cont, call, n: typed): untyped =
  ## rewrite `n` into a tail call via `call` where `cont` is the symbol of the
  ## continuation and `fn` is the identifier/symbol of the function field.
  ##
  ## all AST rewritten by cpsJump should end in a control flow statement.
  expectKind cont, nnkSym
  expectKind call, nnkCallKinds
  expectKind n, nnkStmtList

  debug("cpsJump", n, akOriginal)

  result = newStmtList()

  let prc = makeContProc(genSym(nskProc, "afterCall"), cont, n)

  # make the call safe to modify
  var call = normalizingRewrites call
  result.add prc
  result.add:
    cont.tailCall prc.name:
      call

  result = workaroundRewrites(result)

  debug("cpsJump", result, akTransformed, n)

macro cpsJump(cont, call: typed): untyped =
  ## a version of cpsJump that doesn't take a continuing body.
  result = getAst(cpsJump(cont, call, newStmtList()))

macro cpsMayJump(cont, n, after: typed): untyped =
  ## the block in `n` is tained by a `cpsJump` and may require a jump to enter `after`.
  ##
  ## this macro evaluate `n` and replace all `{.cpsPending.}` in `n` with tail calls
  ## to `after`.
  expectKind cont, nnkSym
  expectKind n, nnkStmtList
  expectKind after, nnkStmtList

  debug("cpsMayJump", n, akOriginal)
  debug("cpsMayJump", after, akOriginal)

  let
    afterProc = makeContProc(genSym(nskProc, "done"), cont, after)
    afterTail =
      newStmtList(
        newAssignment(newDotExpr(desym cont, ident"fn"), afterProc.name),
        nnkReturnStmt.newTree(desym cont)
      )

    # make `n` safe to modify
    n = normalizingRewrites n
    resolvedBody = replacePending(n, afterTail)

  if not resolvedBody.hasContinuation:
    resolvedBody.add afterTail

  result = newStmtList()
  result.add afterProc
  result.add resolvedBody

  result = workaroundRewrites result

  debug("cpsMayJump", result, akTransformed, n)

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
  result.doc "saften at " & n.lineAndFile

  # we're going to iterate over the (non-comment) children
  for i, nc in n.pairs:
    if nc.isNil:
      result.add nc
      continue
    # if it's a cps call,
    if nc.isCpsCall:
      when true:
        let jumpCall = newCall(bindSym"cpsJump")
        jumpCall.add(env.first)
        jumpCall.add:
          env.saften nc
        if i < n.len - 1:
          jumpCall.add:
            env.saften newStmtList(n[i + 1 .. ^1])
        result.add jumpCall
        return
      else:
        # we want to make sure that a pop inside the after body doesn't
        # return to after itself, so we don't add it to the goto list...

        let after = splitAt(env, n, i, "afterCall")
        result.add:
          tailCall(env, env.saften nc):
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
      when false:
        # if the child is a cps block (not a call), then push a tailcall
        # onto the stack during the saftening of the child
        if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
          withGoto env.splitAt(n, i, "done"):
            result.doc "saftening during done"
            result.add env.saften(nc)
            # we've completed the split, so we're done here
            return
      else:
        if nc.isCpsBlock and not nc.isCpsCall:
          case nc.kind
          of nnkWhileStmt, nnkBlockStmt:
            doAssert false, "not supported yet"
          of nnkOfBranch, nnkElse, nnkElifBranch, nnkExceptBranch, nnkFinally:
            discard "these require their outer structure to be captured"
          else:
            let jumpCall = newCall(bindSym"cpsMayJump")
            jumpCall.add(env.first)
            jumpCall.add:
              env.saften:
                newStmtList nc
            jumpCall.add:
                env.saften newStmtList(n[i + 1 .. ^1])
            result.add jumpCall
            return

    case nc.kind
    of nnkReturnStmt:
      # add a return statement with a potential result assignment
      # stored in the environment
      env.addReturn(result, nc)

    of nnkVarSection, nnkLetSection:
      if nc.len != 1:
        # our rewrite pass should prevent this guard from triggering
        result.add:
          nc.errorAst "unexpected section size"
        return

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
            result.add:
              nc.errorAst "too many variable names in section"
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
        result.add:
          errorAst(nc.last, "only calls are supported here")
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
      let name = genSym(nskProc, "whileLoop")
      let bp = env.splitAt(n, i, "postWhile")
      env.addBreak bp
      # the goto is added here so that it won't appear in the break proc
      env.addGoto nc, name
      var loop = newStmtList()
      result.doc "add tail call for while loop with body " & $nc[1].kind
      # we find that the body of the loop may be optimized to not be a
      # statement list, but this will blow saften's little mind, so we
      # recompose it here
      var body =
        if nc[1].kind != nnkStmtList:
          newStmtList [nc[1]]
        else:
          nc[1]
      # we add the loop rewind to the body although it might confuse the
      # saften call; this is deemed more correct than adding it in after
      # the saften call, even though rewriteReturn() needs to know how to
      # ignore `return continuation` as a result
      body.add:
        env.tailCall:
          returnTo env.nextGoto
      loop.add newIfStmt((nc[0], env.saften body))
      # now we can remove the goto from the stack; no other code will
      # resume at this while proc
      discard env.popGoto
      # this is the bottom of the while loop's proc, where we failed
      # the loop's predicate; the same place a `break` will jump to,
      # so we need to call the same break/post-while proc here as well
      loop.doc "add tail call for break proc: " & repr(bp.name)
      loop.add env.callTail(bp)
      # this will rewrite the loop using filter, so...  it's destructive
      result.add env.makeTail(name, loop)
      discard env.popBreak
      return

    of nnkIfStmt, nnkCaseStmt:
      # if any clause is a cps block, then every clause must be.
      # if we've pushed any goto or breaks, then we're already in cps
      if nc.isCpsBlock:
        withGoto env.splitAt(n, i, "isCpsBlockClause"):
          result.doc "add if/of body"
          result.add env.saften(nc)
          return
        result.add:
          nc.errorAst "unexpected control flow from if/case"
      elif insideCps(env):
        result.doc "if/of body inside cps"
        result.add env.saften(nc)
      else:
        result.doc "boring if/of clause"
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
    if env.insideFor or env.insideWhile:
      result.add:
        n.errorAst "no remaining goto for " & $n.kind
    else:
      if not result.firstReturn.isNil:
        result.doc "omit return call from " & $n.kind
      elif env.nextGoto.isNil:
        result.add:
          n.errorAst "nil return; no remaining goto for " & $n.kind
      elif env.nextGoto.kind != nnkNilLit:
        result.doc "adding return call to " & $n.kind
        result.add:
          env.tailCall:
            returnTo env.nextGoto
      else:
        result.add:
          n.errorAst "super confused after " & $n.kind

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

proc replacePending(n, replacement: NimNode): NimNode =
  ## Replace cpsPending annotations with something else, usually
  ## a jump to an another location. If `replacement` is nil, remove
  ## the annotation.
  proc resolved(n: NimNode): NimNode =
    case n.kind
    of nnkPragma:
      # a {.cpsPending.} annotation is a standalone pragma statement
      if n.len == 1 and n.hasPragma("cpsPending"):
        if replacement.isNil:
          result = newEmptyNode()
        else:
          result = copyNimTree replacement
    else: discard

  result = filter(n, resolved)

macro cpsStripPending(n: typed): untyped =
  ## remove any remaining {.cpsPending().}
  ##
  ## this is not needed, but it's here so we can change this to
  ## a sanity check pass later.
  expectKind n, nnkProcDef
  debug(".cpsStripPending.", n, akOriginal)

  # make `n` safe for modification
  let n = normalizingRewrites n
  # replace all `pending` with `return nil`, signifying end of continuations
  result = replacePending(n, tailCall(nil, nil))
  result = workaroundRewrites result

  debug(".cpsStripPending.", result, akOriginal, n)

proc xfrmFloat(n: NimNode): NimNode =
  var floats = newStmtList()

  proc float(n: NimNode): NimNode =
    if not n.isNil and n.hasPragma("cpsLift"):
      var n = n.stripPragma("cpsLift")
      floats.add:
        xfrmFloat n
      result = newEmptyNode()

  let cleaned = filter(n, float)
  result = floats
  result.add cleaned

macro cpsFloater(n: typed): untyped =
  ## float all `{.cpsLift.}` to top-level
  expectKind n, nnkProcDef
  debug(".cpsFloater.", n, akOriginal)

  var n = copyNimTree n
  result = xfrmFloat n

  debug(".cpsFloater.", result, akOriginal, n)

proc cpsXfrmProc(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style

  # enhanced spam before it all goes to shit
  debug(".cps.", n, akOriginal)

  # make the ast easier for us to consume
  var n = normalizingRewrites n
  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()
  var env: Env

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

  var booty: NimNode
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

  # perform sym substitutions (or whatever)
  n.body = env.prepProcBody(newStmtList n.body)

  # ensaftening the proc's body
  n.body = env.saften(n.body)

  when false:
    if len(preamble) > 0:           # if necessary, insert the preamble
      n.body.insert(0, preamble)    # ahead of the rest of the body

  # add in a pragma so other cps macros can identify this as a cps call
  n.addPragma ident"cpsCall"

  # run other stages
  n.addPragma bindSym"cpsFloater"
  n.addPragma bindSym"cpsStripPending"

  # "encouraging" a write of the current accumulating type
  env = env.storeType(force = off)

  # Araq: "learn how to desemantic your ast, knuckleheads"
  # No longer necessary as we are desym-ing on a selective basis
  when false:
    n.body = replacedSymsWithIdents(n.body)
    types = replacedSymsWithIdents(types)
    booty = replacedSymsWithIdents(booty)

  # lifting the generated proc bodies
  result = lambdaLift(types, n)

  # adding in the bootstrap
  result.add booty

  # spamming the developers
  debug(".cps.", result, akOriginal, n)

proc workaroundRewrites(n: NimNode): NimNode =
  proc rewriteContainer(n: NimNode): NimNode =
    ## Helper function to recreate a container node while keeping all children
    ## to discard semantic data attached to the container.
    ##
    ## Returns the same node if its not a container node.
    result = n
    if n.kind notin AtomicNodes:
      result = newNimNode(n.kind, n)
      for child in n:
        result.add child

  proc workaroundSigmatchSkip(n: NimNode): NimNode =
    if n.kind in nnkCallKinds:
      # We recreate the nodes here, to set their .typ to nil
      # so that sigmatch doesn't decide to skip it
      result = newNimNode(n.kind, n)
      for child in n.items:
        # The containers of direct children always has to be rewritten
        # since they also have a .typ attached from the previous sem pass
        result.add:
          rewriteContainer:
            workaroundRewrites child

  result = filter(n, workaroundSigmatchSkip)

proc cpsXfrm(T: NimNode, n: NimNode): NimNode =
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
