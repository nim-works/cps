import std/[macros, sequtils, algorithm]
import cps/[spec, environment]
export Continuation, ContinuationProc, cpsCall
export cpsDebug, cpsTrace

when CallNodes - {nnkHiddenCallConv} != nnkCallKinds:
  {.error: "i'm afraid of what you may have become".}

proc isCpsCall(n: NimNode): bool =
  ## true if this node holds a call to a cps procedure
  assert not n.isNil
  if len(n) > 0:
    if n.kind in nnkCallKinds:
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

proc makeReturn(n: NimNode): NimNode =
  ## generate a `return` of the node if it doesn't already contain a return
  assert not n.isNil, "we no longer permit nil nodes"
  if n.firstReturn.isNil:
    nnkReturnStmt.newNimNode(n).add:
      if n.kind in nnkCallKinds:
        n
      else:
        newCall(ident"coop", n)
  else:
    n

proc makeReturn(pre: NimNode; n: NimNode): NimNode =
  ## if `pre` holds no `return`, produce a `return` of `n` after `pre`
  result = newStmtList pre
  result.add:
    if pre.firstReturn.isNil:
      makeReturn n
    else:
      doc "omitted a return of " & repr(n)

proc isCpsBlock(n: NimNode): bool =
  ## `true` if the block `n` contains a cps call anywhere at all;
  ## this is used to figure out if a block needs tailcall handling...
  case n.kind
  of nnkForStmt, nnkBlockStmt, nnkElse, nnkOfBranch:
    return n.last.isCpsBlock
  of nnkStmtList, nnkIfStmt, nnkCaseStmt, nnkWhileStmt, nnkElifBranch,
     nnkTryStmt:
    for n in n.items:
      if n.isCpsBlock:
        return true
  of nnkCallKinds:
    return n.isCpsCall
  else:
    return false

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
  ## lift AST tagged with cpsLift pragma to top-level and omit the pragma

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
    makeReturn:
      if name.kind == nnkNilLit:
        name                                   # return nil
      else:
        env.continuationReturnValue name       # return continuation

  var procs = newStmtList()
  if n.kind == nnkProcDef:
    result.doc "adding the proc verbatim"
    procs.add n
  else:
    # the locals value is, nominally, a proc param -- or it was.
    # now we just use whatever the macro provided the env
    var locals = env.first

    # setup the proc body with whatever locals it still needs
    var body = env.rewriteSymbolsIntoEnvDotField(n)

    # add the declaration
    procs.add newProc(name = name, pragmas = pragmas,
                      body = newEmptyNode(),
                      params = [env.root, newIdentDefs(locals,
                                                       env.root)])
    # add the implementation
    procs.add newProc(name = name, pragmas = pragmas, body = body,
                      params = [env.root, newIdentDefs(locals,
                                                       env.root)])

    # immediately push these definitions into the store and just
    # return the tail call
    for p in procs.items:
      env.defineProc p

proc saften(parent: var Env; n: NimNode): NimNode

proc makeContProc(name, cont, body: NimNode): NimNode =
  ## creates a continuation proc from with `name` using continuation
  ## `cont` with the given body.
  let
    # we always wrap the body because there's no reason not to
    body = newStmtList body
    contParam = desym cont
    contType = getTypeInst cont

  # mix the coop symbol into any continuation proc
  body.insert(0, nnkMixinStmt.newTree ident"coop")

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
  # let other macros know this is a continuation
  result.addPragma bindSym"cpsCont"

func tailCall(cont: NimNode; to: NimNode; jump: NimNode = nil): NimNode =
  ## a tail call to `to` with `cont` as the continuation; if the `jump`
  ## is supplied, return that call instead of the continuation itself
  result = newStmtList:
    newAssignment(newDotExpr(cont, ident"fn"), to)
  let jump = if jump.isNil: cont else: jump
  result = makeReturn(result, jump)

func jumperCall(cont: NimNode; to: NimNode; via: NimNode): NimNode =
  ## Produce a tail call to `to` with `cont` as the continuation
  ## The `via` argument is expected to be a cps jumper call.
  assert not via.isNil
  assert via.kind in nnkCallKinds

  let jump = copyNimTree via
  # desym the jumper, it is currently sem-ed to the variant that
  # doesn't take a continuation
  jump[0] = desym jump[0]
  # insert our continuation as the first parameter
  jump.insert(1, cont)
  result = tailCall(cont, to, jump)

macro cpsJump(cont, call, n: typed): untyped =
  ## Rewrite `n` into a tail call via `call` where `cont` is the symbol of
  ## the continuation and `fn` is the identifier/symbol of the function
  ## field.
  ##
  ## All AST rewritten by cpsJump should end in a control-flow statement.
  expectKind cont, nnkSym
  expectKind call, nnkCallKinds

  #debug("cpsJump", n, Original)

  let name = nskProc.genSym"afterCall"
  result = newStmtList makeContProc(name, cont, n)
  result.add:
    cont.jumperCall name:
      normalizingRewrites call
  result = workaroundRewrites result

  #debug("cpsJump", result, Transformed, n)

macro cpsJump(cont, call: typed): untyped =
  ## a version of cpsJump that doesn't take a continuing body.
  result = getAst(cpsJump(cont, call, newStmtList()))

macro cpsMayJump(cont, n, after: typed): untyped =
  ## The block in `n` is tainted by a `cpsJump` and may require a jump
  ## to enter `after`.
  ##
  ## This macro evaluates `n` and replaces all `{.cpsPending.}` in `n`
  ## with tail calls to `after`.
  expectKind cont, nnkSym

  #debug("cpsMayJump", n, Original)
  #debug("cpsMayJump", after, Original)

  # make `n` safe to modify
  var n = normalizingRewrites:
    # we always wrap the input because there's no reason not to
    newStmtList n

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

  #debug("cpsMayJump", result, Transformed, n)

func matchCpsBreak(label: NimNode): Matcher =
  ## create a matcher matching cpsBreak with the given label
  ## and cpsBreak without any label
  result =
    proc (n: NimNode): bool =
      if n.isCpsBreak:
        let breakLabel = n.breakLabel
        breakLabel.kind == nnkEmpty or breakLabel == label
      else:
        false

proc restoreBreak(n: NimNode, label: NimNode = newEmptyNode()): NimNode =
  ## restore {.cpsBreak: label.} into break statements
  let match = matchCpsBreak(label)
  proc restorer(n: NimNode): NimNode =
    if match(n):
      newNimNode(nnkBreakStmt, n).add:
        n.breakLabel
    else:
      nil

  filter(n, restorer)

proc restoreContinue(n: NimNode): NimNode =
  ## restore {.cpsContinue.} into continue statements
  proc restorer(n: NimNode): NimNode =
    if n.isCpsContinue:
      newNimNode(nnkContinueStmt, n).add:
        newEmptyNode()
    else:
      nil

  filter(n, restorer)

macro cpsBlock(cont, label, n: typed): untyped =
  ## The block with `label` is tainted by a `cpsJump` and may require a
  ## jump to break out of the block.
  ##
  ## This macro evaluates `n` and replaces all `{.cpsBreak.}` in `n` with
  ## `{.cpsPending.}`.
  expectKind cont, nnkSym
  expectKind label, {nnkSym, nnkEmpty}

  #debug("cpsBlock", n, Original)

  # make `n` safe to modify
  var n = normalizingRewrites:
    # we always wrap the input because there's no reason not to
    newStmtList n

  result = n.replace(matchCpsBreak(label), newCpsPending())
  result = workaroundRewrites result

  #debug("cpsBlock", result, Transformed, n)

macro cpsBlock(cont, n: typed): untyped =
  ## A block statement tainted by a `cpsJump` and may require a jump to
  ## enter `after`.
  ##
  ## This is just an alias to cpsBlock with an empty label.
  result = getAst(cpsBlock(cont, newEmptyNode(), n))

macro cpsWhile(cont, cond, n: typed): untyped =
  ## A while statement tainted by a `cpsJump` and may require a jump to
  ## exit the loop.
  ##
  ## This macro evaluates `n` and replaces all `{.cpsPending.}` with a
  ## jump to the loop condition and `{.cpsBreak.}` with `{.cpsPending.}`
  ## to the next control-flow.
  expectKind cont, nnkSym

  #debug("cpsWhile", newTree(nnkWhileStmt, cond, n), Original, cond)

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

  #debug("cpsWhile", result, Transformed, cond)

macro cpsTryExcept(cont, ex, n: typed): untyped =
  ## A try statement tainted by a `cpsJump` and may require a jump to enter
  ## any handler.
  ##
  ## Only rewrite try with except branches.
  {.warning: "compiler workaround here".}
  let cont = cont

  expectKind cont, nnkSym
  expectKind ex, nnkDotExpr
  expectKind n, nnkTryStmt

  result = newStmtList()

  var
    n = normalizingRewrites n
    ex = normalizingRewrites ex

  # the try statement that will be used to wrap all child of `n`
  let tryTemplate = copyNimNode n

  # use an empty node as a stand-in for the real body
  tryTemplate.add newEmptyNode()

  # Turn each handler branch into a continuation leg
  # For finally we stash it into a variable for rewrite later
  for i in 1 ..< n.len:
    case n[i].kind
    of nnkExceptBranch:
      let newExcept = copyNimNode n[i]
      # add `Type` or `Type as e`
      newExcept.add n[i][0]
      # create a new body
      newExcept.add newStmtList()

      # get old handler body
      var handlerBody = newStmtList(n[i][1])

      # if it's a `except Type as e`
      if newExcept[0].kind == nnkInfix:
        # assign the exception to `ex`
        let exSym = n[i][0][2]
        newExcept.last.add newAssignment(ex, exSym)
        # replace all occurance of `exSym` with `ex`
        handlerBody = n[i][1].resym(exSym, ex)
      else:
        # assign the exception into `ex`
        newExcept.last.add newAssignment(ex, newCall(bindSym"getCurrentException"))

      # set exception to the stashed one before executing any other code
      #
      # if not isNil(ex):
      #   setCurrentException(ex)
      handlerBody.insert(0):
        nnkIfStmt.newTree:
          nnkElifBranch.newTree(newCall(bindSym"not", newCall(bindSym"isNil", ex))):
            newStmtList():
              newCall(bindSym"setCurrentException", ex)

      # turn the exception body into a continuation
      let handler = makeContProc(genSym(nskProc, "except"), cont, handlerBody)
      # add the continuation to the top
      result.add handler
      # add a tail to the new continuation in the except branch
      newExcept.last.add tailCall(cont, handler.name)

      # add this into the template
      tryTemplate.add newExcept
    else: result.add errorAst(n[i], "unexpected node in cpsTryExcept")

  proc wrapTry(contSym, n: NimNode): NimNode =
    ## Wrap `n` with the `try` template, and replace all `cont` with `contSym`
    proc wrapCont(n: NimNode): NimNode =
      ## Wrap the body of continuation `n` with `try` template
      if n.isCpsCont:
        result = n
        result.body = wrapTry(getContSym(result), result.body)

    result = copyNimTree tryTemplate
    # replace the body of the template with `n`
    result[0] = newStmtList:
      # wrap all continuations of `n` with the template
      filter(n, wrapCont)
    # replace `cont` with `contSym`
    result = result.resym(cont, contSym)

  # wrap the try body and everything in it
  result.add wrapTry(cont, n[0])

  result = workaroundRewrites result

macro cpsTryFinally(cont, ex, n: typed): untyped =
  ## A try statement tainted by a `cpsJump` and may require a jump to enter finally.
  ##
  ## Only rewrite try with finally.
  doAssert false, "not implemented"

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
      if n.kind == nnkTryStmt:
        discard "children of this node are separated execution branches"
      elif nc.isCpsBlock and not nc.isCpsCall:
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
      return makeReturn result:
        env.rewriteReturn nc

    of nnkVarSection, nnkLetSection:
      if nc.len != 1:
        # our rewrite pass should prevent this guard from triggering
        result.add:
          nc.errorAst "unexpected section size"
        return

      if isCpsCall(nc.last.last):
        result.add:
          nc.last.errorAst "shim is not yet supported"
      elif isCpsBlock(nc.last.last):
        result.add:
          nc.last.errorAst "only calls are supported here"
      else:
        # add definitions into the environment
        env.localSection(nc, result)

    of nnkForStmt:
      if nc.isCpsBlock:
        result.add:
          errorAst("for loop with a cps call inside is not supported", nc)
      else:
        var transformed = env.saften(nc)
        # this is not a cps block, so all the break and continue should be preserved
        transformed = restoreBreak transformed
        transformed = restoreContinue transformed
        result.add transformed

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
        # add condition
        jumpCall.add:
          env.saften nc[0]
        # add body
        jumpCall.add:
          env.saften newStmtList(nc[1])
        result.add jumpCall
        return
      else:
        var transformed = env.saften(nc)
        # this is not a cps block, so all the break and continue should be preserved
        transformed = restoreBreak transformed
        transformed = restoreContinue transformed
        result.add transformed

    of nnkTryStmt:
      if nc.isCpsBlock:
        let final = nc.findChild(it.kind == nnkFinally)
        if final.isNil:
          let jumpCall = newCall(bindSym"cpsTryExcept")
          # add continuation sym
          jumpCall.add env.first
          # add exception access
          jumpCall.add env.getException()
          # add body
          jumpCall.add:
            newStmtList env.saften(nc)
          result.add jumpCall
        else:
          let jumpCall = newCall(bindSym"cpsTryFinally")
          jumpCall.add env.first
          # add exception access
          jumpCall.add env.getException()
          if not nc.findChild(it.kind == nnkExceptBranch).isNil:
            # create a copy of `nc` without finally
            let newTry = copyNimNode nc
            newTry.add nc[0 .. ^2]

            # wrap the try-finally outside of `nc`
            let tryFinally = copyNimNode nc
            tryFinally.add newStmtList(newTry)
            tryFinally.add final

            # add try-finally
            jumpCall.add:
              newStmtList env.saften(tryFinally)
          else:
            # add try-finally
            jumpCall.add:
              newStmtList env.saften(nc)

          result.add jumpCall

        return
      else:
        result.add env.saften(nc)

    # not a statement cps is interested in
    else:
      result.add env.saften(nc)

proc cloneProc(n: NimNode, body: NimNode = nil): NimNode =
  ## create a copy of a typed proc which satisfies the compiler
  assert n.kind == nnkProcDef
  result = nnkProcDef.newTree(
    ident(repr n.name),           # repr to handle gensymbols
    newEmptyNode(),
    newEmptyNode(),
    n.params,
    newEmptyNode(),
    newEmptyNode(),
    if body == nil: copy n.body else: body)

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
    if n.kind == nnkPragma and n.len == 1 and
      (n.hasPragma("cpsContinue") or n.hasPragma("cpsBreak") or n.hasPragma("cpsPending")):
      errorAst(n, "cps error: un-rewritten cps control-flow")
    else: n

  filter(n, dangle)

macro cpsResolver(T: typed, n: typed): untyped =
  ## resolve any left over cps control-flow annotations
  ##
  ## this is not needed, but it's here so we can change this to
  ## a sanity check pass later.
  expectKind n, nnkProcDef
  #debug(".cpsResolver.", n, Original)

  # grabbing the first argument to the proc as an identifier
  let cont = desym n.params[1][0]

  # make `n` safe for modification
  let n = normalizingRewrites n
  # replace all `pending` with the end of continuation
  result = replacePending n:
    tailCall(cont, newNilLit())
  result = danglingCheck result
  result = workaroundRewrites result

  #debug(".cpsResolver.", result, Transformed, n)

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
  #debug(".cpsFloater.", n, Original)

  var n = copyNimTree n
  result = xfrmFloat n

  #debug(".cpsFloater.", result, Transformed, n)

proc cpsXfrmProc*(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style

  # Some sanity checks
  n.expectKind nnkProcdef
  if not n.params[0].isEmpty:
    error "cps functions can not have a return type", n

  # enhanced spam before it all goes to shit
  debug(".cps.", n, Original)

  # make the AST easier for us to consume
  var n = normalizingRewrites n
  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()

  # creating the env with the continuation type,
  # and adding proc parameters to the env
  var env = newEnv(ident"continuation", types, T)

  # add parameters into the environment
  for defs in n.params[1 .. ^1]:
    if defs[1].kind == nnkVarTy:
      error "cps does not support var parameters", n
    env.localSection(defs)

  ## Generate the bootstrap
  var booty = cloneProc(n, newStmtList())
  booty.params[0] = T
  booty.body.doc "This is the bootstrap to go from Nim-land to CPS-land"
  booty.body.add:
    newAssignment(ident"result", env.newContinuation booty.name)

  # we can't mutate typed nodes, so copy ourselves
  n = cloneProc n

  # do some pruning of these typed trees.
  for p in [booty, n]:
    p.name = desym(p.name)
    while len(p) > 7:
      p.del(7)

  # Replace the proc params: its sole argument and return type is T:
  #   proc name(continuation: T): T
  n.params = nnkFormalParams.newTree(T, env.firstDef)

  # perform sym substitutions (or whatever)
  n.body = env.rewriteSymbolsIntoEnvDotField(newStmtList n.body)

  # transform defers
  n.body = rewriteDefer n.body

  # ensaftening the proc's body
  n.body = env.saften(n.body)

  # add in a pragma so other cps macros can identify this as a cps call
  n.addPragma ident"cpsCall"

  # run other stages
  n.addPragma bindSym"cpsFloater"
  n.addPragma nnkExprColonExpr.newTree(bindSym"cpsResolver", T)

  # "encouraging" a write of the current accumulating type
  env = env.storeType(force = off)

  # lifting the generated proc bodies
  result = lambdaLift(types, n)

  # adding in the bootstrap
  result.add booty

  # spamming the developers
  debug(".cps.", result, Transformed)
