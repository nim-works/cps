import std/macros
import std/sequtils
import std/algorithm

import cps/spec
import cps/scopes
import cps/environment
export Continuation, ContinuationProc, cpsCall
export cpsDebug, cpsTrace

template installLocal(id, env, field) =
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
      ret.add n
      result = ret
  else:
    # return a statement list including the setup for the locals
    # and the return statement casting those locals to the root type
    result = newStmtList()
    let locals = e.defineLocals(result, n)
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
  of nnkBlockStmt, nnkWhileStmt, nnkElse, nnkElifBranch, nnkOfBranch:
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
        newCall env.root
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

proc makeContProc(name, cont, body: NimNode): NimNode =
  ## creates a continuation proc from with `name` using continuation `cont`
  ## with the given body.
  # we always wrap the body because there's no reason not to
  let
    body = newStmtList body

    contParam = desym cont
    contType = getTypeInst cont

  result = newProc(name, [contType, newIdentDefs(contParam, contType)])
  # make it something that we can consume and modify
  result.body = normalizingRewrites body
  # replace any `cont` within the body with the parameter of the newly made proc
  result.body = resym(result.body, cont, contParam)
  # if this body don't have any continuing control-flow
  if result.body.firstReturn.isNil:
    # annotate it so that outer macros can fill in as needed
    result.body.add newCpsPending()
  # tell cpsFloater that we want this to be lifted to the top-level
  result.addPragma bindSym"cpsLift"

func tailCall(cont, to: NimNode, via: NimNode = nil): NimNode =
  ## Produce a tail call to `to` with `cont` as the continuation
  ##
  ## If `to` is nil, this is the last continuation and will be
  ## set to nil.
  ##
  ## If `via` is not nil, it is expected to be a cps jumper call
  doAssert not cont.isNil
  doAssert not to.isNil

  result = newStmtList()

  # progress to the next function in the continuation
  result.add:
    newAssignment(newDotExpr(cont, ident"fn"), to)

  if via.isNil:
    result.add:
      nnkReturnStmt.newTree:
        cont
  else:
    doAssert via.kind in CallNodes

    let jump = copyNimTree via
    # desym the jumper, it is currently sem-ed to the variant that
    # doesn't take a continuation
    jump[0] = desym jump[0]
    # insert our continuation as the first parameter
    jump.insert(1, cont)
    result.add:
      nnkReturnStmt.newTree:
        jump

func endContinuation(): NimNode =
  ## Produce a tail signifying the end of the continuation
  nnkReturnStmt.newTree:
    newNilLit()

macro cpsJump(cont, call, n: typed): untyped =
  ## rewrite `n` into a tail call via `call` where `cont` is the symbol of the
  ## continuation and `fn` is the identifier/symbol of the function field.
  ##
  ## all AST rewritten by cpsJump should end in a control flow statement.
  expectKind cont, nnkSym
  expectKind call, nnkCallKinds

  debug("cpsJump", n, Original)

  result = newStmtList()

  let prc = makeContProc(genSym(nskProc, "afterCall"), cont, n)

  # make the call safe to modify
  var call = normalizingRewrites call
  result.add prc
  result.add:
    cont.tailCall prc.name:
      call

  result = workaroundRewrites(result)

  debug("cpsJump", result, Transformed, n)

macro cpsJump(cont, call: typed): untyped =
  ## a version of cpsJump that doesn't take a continuing body.
  result = getAst(cpsJump(cont, call, newStmtList()))

macro cpsMayJump(cont, n, after: typed): untyped =
  ## the block in `n` is tained by a `cpsJump` and may require a jump to enter `after`.
  ##
  ## this macro evaluate `n` and replace all `{.cpsPending.}` in `n` with tail calls
  ## to `after`.
  expectKind cont, nnkSym

  debug("cpsMayJump", n, Original)
  debug("cpsMayJump", after, Original)

  # we always wrap the input because there's no reason not to
  var n = newStmtList n

  # make `n` safe to modify
  n = normalizingRewrites n

  let
    afterProc = makeContProc(genSym(nskProc, "done"), cont, after)
    afterTail = tailCall(desym cont, afterProc.name)

    resolvedBody =
      n.replace(isCpsPending):
        afterTail

  if resolvedBody.firstReturn.isNil:
    resolvedBody.add afterTail

  result = newStmtList()
  result.add afterProc
  result.add resolvedBody

  result = workaroundRewrites result

  debug("cpsMayJump", result, Transformed, n)

func matchCpsBreak(label: NimNode): Matcher =
  ## create a matcher matching cpsBreak with the given label
  ## and cpsBreak without any label
  result =
    func (n: NimNode): bool =
      if n.isCpsBreak:
        let breakLabel = n.breakLabel
        breakLabel.kind == nnkEmpty or breakLabel == label
      else:
        false

func restoreBreak(n: NimNode, label: NimNode = newEmptyNode()): NimNode =
  ## restore {.cpsBreak: label.} into break statements
  let match = matchCpsBreak(label)
  proc restorer(n: NimNode): NimNode =
    if match(n):
      newNimNode(nnkBreakStmt, n).add:
        n.breakLabel
    else:
      nil

  filter(n, restorer)

func restoreContinue(n: NimNode): NimNode =
  ## restore {.cpsContinue.} into continue statements
  proc restorer(n: NimNode): NimNode =
    if n.isCpsContinue:
      newNimNode(nnkContinueStmt, n).add:
        newEmptyNode()
    else:
      nil

  filter(n, restorer)

macro cpsBlock(cont, label, n: typed): untyped =
  ## the block with `label` is tained by a `cpsJump` and may require a jump to
  ## break out of the block.
  ##
  ## this macro evaluate `n` and replace all `{.cpsBreak.}` in `n` with
  ## `{.cpsPending.}`.
  expectKind cont, nnkSym
  expectKind label, {nnkSym, nnkEmpty}

  debug("cpsBlock", n, Original)

  # we always wrap the input because there's no reason not to
  var n = newStmtList n

  # make `n` safe to modify
  n = normalizingRewrites n

  result = n.replace(matchCpsBreak(label), newCpsPending())

  result = workaroundRewrites result

  debug("cpsBlock", result, Transformed, n)

macro cpsBlock(cont, n: typed): untyped =
  ## a block statement tained by a `cpsJump` and may require a jump to enter `after`.
  ##
  ## this is just an alias to cpsBlock with an empty label
  result = getAst(cpsBlock(cont, newEmptyNode(), n))

macro cpsWhile(cont, cond, n: typed): untyped =
  ## a while statement tainted by a `cpsJump` and may require a jump to exit
  ## the loop.
  ##
  ## this macros evaluate `n` and replace all `{.cpsPending.}` with jump to
  ## loop condition and `{.cpsBreak.}` with `{.cpsPending.}` to next
  ## control-flow.
  expectKind cont, nnkSym

  debug("cpsWhile", newTree(nnkWhileStmt, cond, n), Original, cond)

  result = newStmtList()

  let loopBody = newStmtList:
    newIfStmt((cond, n)).add:
      nnkElse.newTree:
        newCpsBreak()

  let
    whileLoop = makeContProc(genSym(nskProc, "whileLoop"), cont, loopBody)
    whileTail = tailCall(desym cont, whileLoop.name)

  whileLoop.body = whileLoop.body.multiReplace(
    (isCpsPending.Matcher, whileTail),
    (isCpsContinue.Matcher, whileTail),
    (matchCpsBreak(newEmptyNode()), newCpsPending())
  )

  result.add whileLoop
  result.add whileTail

  result = workaroundRewrites result

  debug("cpsWhile", result, Transformed, cond)

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
      let jumpCall = newCall(bindSym"cpsJump")
      jumpCall.add(env.first)
      jumpCall.add:
        env.saften nc
      if i < n.len - 1:
        jumpCall.add:
          env.saften newStmtList(n[i + 1 .. ^1])
      result.add jumpCall
      return

    if i < n.len-1:
      if nc.isCpsBlock and not nc.isCpsCall:
        case nc.kind
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
      result.add newCpsContinue()
      result[^1].copyLineInfo nc

    of nnkBreakStmt:
      result.add newCpsBreak(nc.breakLabel)
      result[^1].copyLineInfo(nc)

    of nnkBlockStmt:
      if nc.isCpsBlock:
        let jumpCall = newCall(bindSym"cpsBlock")
        jumpCall.copyLineInfo nc
        jumpCall.add env.first
        if nc[0].kind != nnkEmpty:
          # add label if it exists
          jumpCall.add nc[0]
        jumpCall.add:
          env.saften newStmtList(nc[1])
        result.add jumpCall
        return
      else:
        var transformed = env.saften(nc)
        # this is not a cps block, so all the break should be preserved
        transformed = transformed.restoreBreak(nc[0])
        result.add transformed

    of nnkWhileStmt:
      if nc.isCpsBlock:
        let jumpCall = newCall(bindSym"cpsWhile")
        jumpCall.copyLineInfo nc
        jumpCall.add env.first
        for child in nc:
          jumpCall.add:
            env.saften child
        result.add jumpCall
        return
      else:
        var transformed = env.saften(nc)
        # this is not a cps block, so all the break and continue should be preserved
        transformed = restoreBreak transformed
        transformed = restoreContinue transformed
        result.add transformed

    # not a statement cps is interested in
    else:
      result.add env.saften(nc)

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
    if n.isCpsPending:
      if replacement.isNil:
        result = newEmptyNode()
      else:
        result = copyNimTree replacement

  result = filter(n, resolved)

proc danglingCheck(n: NimNode): NimNode =
  ## look for un-rewritten control-flow then replace them with errors
  proc dangle(n: NimNode): NimNode =
    case n.kind
    of nnkPragma:
      result = n
      if n.len == 1:
        if n.hasPragma("cpsContinue") or n.hasPragma("cpsBreak") or n.hasPragma("cpsPending"):
          result = errorAst(n, "cps error: un-rewritten cps control-flow")
    else: discard

  filter(n, dangle)

macro cpsResolver(n: typed): untyped =
  ## resolve any left over cps control-flow annotations
  ##
  ## this is not needed, but it's here so we can change this to
  ## a sanity check pass later.
  expectKind n, nnkProcDef
  debug(".cpsResolver.", n, Original)

  # make `n` safe for modification
  let n = normalizingRewrites n
  # replace all `pending` with the end of continuation
  result = replacePending(n, endContinuation())
  result = danglingCheck(result)
  result = workaroundRewrites(result)

  debug(".cpsResolver.", result, Transformed, n)

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
  debug(".cpsFloater.", n, Original)

  var n = copyNimTree n
  result = xfrmFloat n

  debug(".cpsFloater.", result, Transformed, n)

proc cpsXfrmProc(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style

  # enhanced spam before it all goes to shit
  debug(".cps.", n, Original)

  # make the ast easier for us to consume
  var n = normalizingRewrites n
  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()
  var env: Env

  # creating the env with the continuation type,
  # and adding proc parameters to the env
  var first = 1 # index of first param to add to locals
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
  n.addPragma bindSym"cpsResolver"

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
  debug(".cps.", result, Transformed)

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
