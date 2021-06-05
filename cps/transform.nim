import std/[macros, sequtils]
import cps/[spec, environment, hooks, returns, defers, rewrites, help,
            normalizedast]
export Continuation, ContinuationProc, cpsCall, cpsMustJump

#{.experimental: "strictNotNil".}

when CallNodes - {nnkHiddenCallConv} != nnkCallKinds:
  {.error: "i'm afraid of what you may have become".}

proc isCpsCall(n: NimNode): bool =
  ## true if this node holds a call to a cps procedure
  if n.len > 0:
    if n.kind in nnkCallKinds:
      let callee = n[0]
      if not callee.isNil and callee.kind == nnkSym:
        # what we're looking for here is a jumper; it could
        # be a magic or it could be another continuation leg
        # or it could be a completely new continuation
        result = callee.getImpl.hasPragma("cpsMustJump")

proc isCpsBlock(n: NimNode): bool =
  ## `true` if the block `n` contains a cps call anywhere at all;
  ## this is used to figure out if a block needs tailcall handling...
  case n.kind
  of nnkForStmt, nnkBlockStmt, nnkElse, nnkOfBranch, nnkExceptBranch,
     nnkFinally:
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

proc annotate(parent: var Env; n: NimNode): NimNode

proc makeContProc(name, cont, source: NimNode): NimNode =
  ## creates a continuation proc from with `name` using continuation
  ## `cont` with the given body.
  let
    contParam = desym cont
    contType = getTypeInst cont

  result = newProc(name, [contType, newIdentDefs(contParam, contType)])
  result.copyLineInfo source        # grab lineinfo from the source body
  result.body = newStmtList()       # start with an empty body
  result.introduce {Coop, Pass, Head, Tail, Trace, Alloc, Dealloc}
  result.body.add:                  # insert a hook ahead of the source,
    Trace.hook contParam, result    # hooking against the proc (minus body)
  result.body.add:                  # perform convenience rewrites on source
    normalizingRewrites newStmtList(source)

  # replace `cont` in the body with the parameter of the new proc
  result.body = resym(result.body, cont, contParam)
  # if this body doesn't have any continuing control-flow,
  if result.body.firstReturn.isNil:
    # annotate it so that outer macros can fill in as needed.
    result.body.add newCpsPending()
  # tell cpsFloater that we want this to be lifted to the top-level
  result.addPragma bindSym"cpsLift"
  result.addPragma ident"nimcall"
  # let other macros know this is a continuation
  result.addPragma bindSym"cpsCont"

macro cpsJump(cont, call, n: typed): untyped =
  ## Rewrite `n` into a tail call via `call` where `cont` is the symbol of
  ## the continuation and `fn` is the identifier/symbol of the function
  ## field.
  ##
  ## All AST rewritten by cpsJump should end in a control-flow statement.
  let call = normalizingRewrites call
  let name = nskProc.genSym"afterCall"
  debugAnnotation cpsJump, n:
    it = newStmtList:
      makeContProc(name, cont, it)
    if getImpl(call[0]).hasPragma "cpsBootstrap":
      let c = nskLet.genSym"c"
      it.add:
        # install the return point in the current continuation
        newAssignment(newDotExpr(cont, ident"fn"), name)
      it.add:
        # instantiate a new child continuation with the given arguments
        newLetStmt c:
          newCall newCall(ident"typeof", cont):
            newCall(ident"whelp", cont, call)
      # NOTE: mom should now be set via the tail() hook from whelp
      #
      # return the child continuation
      it = it.makeReturn:
        Pass.hook(cont, c)    # we're basically painting the future
    else:
      it.add:
        jumperCall(cont, name, call)

macro cpsJump(cont, call: typed): untyped =
  ## a version of cpsJump that doesn't take a continuing body.
  result = getAst(cpsJump(cont, call, newStmtList()))

macro cpsMayJump(cont, n, after: typed): untyped =
  ## The block in `n` is tainted by a `cpsJump` and may require a jump
  ## to enter `after`.
  ##
  ## This macro evaluates `n` and replaces all `{.cpsPending.}` in `n`
  ## with tail calls to `after`.
  let name = nskProc.genSym"done"
  let tail = tailCall(desym cont, name)
  debugAnnotation cpsMayJump, n:
    it = it.replace(isCpsPending, tail)
    if it.firstReturn.isNil:
      it.add tail
    it = newStmtList [makeContProc(name, cont, after), it]

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
  debugAnnotation cpsBlock, n:
    it = it.replace(matchCpsBreak(label), newCpsPending())

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
  let name = nskProc.genSym"whileLoop"
  let tail = tailCall(desym cont, name)
  debugAnnotation cpsWhile, n:
    # a key first step is to ensure that the loop body, uh, loops, by
    # issuing a tailcall back to the while proc instead falling through
    it.add tail
    var body = newStmtList nnkIfStmt.newTree [  # a conditional test up top,
      nnkElifBranch.newTree [cond, it],         # runs the body if it's true,
      nnkElse.newTree newCpsBreak(n)            # else, runs a break clause
    ]
    body = body.multiReplace(
      (isCpsPending.Matcher, tail),
      (isCpsContinue.Matcher, tail),
      (matchCpsBreak(newEmptyNode()), newCpsPending())
    )
    # we return the while proc and a tailcall to enter it the first time
    it = newStmtList [makeContProc(name, cont, body), tail]

proc rewriteExcept(cont, ex, n: NimNode): tuple[cont, excpt: NimNode] =
  ## Rewrite the exception branch `n` to use `ex` as the "current"
  ## exception and move its body into a continuation
  result.excpt = copyNimNode n

  var body = newStmtList(n.last)
  # this is `except Type: body`
  if n.len > 1:
    # this is `except Type as e: body`
    if n[0].kind == nnkInfix:
      # replace all occurance of `e` with `ex`
      body = body.resym(n[0][2], ex)

      # add `Type` to our new except clause
      result.excpt.add n[0][1]
    else:
      # add `Type`
      result.excpt.add n[0]

  proc setException(contSym, n: NimNode): NimNode =
    ## Set the exception to `ex` before running any other code
    proc setContException(n: NimNode): NimNode =
      ## If `n` is a continuation, set the exception to `ex` before
      ## running any other code
      # XXX: need a way to prevent multiple insertions of this
      if n.isCpsCont:
        result = n
        result.body = setException(getContSym(n), result.body)

    result = newStmtList():
      newCall(bindSym"setCurrentException", ex.resym(cont, contSym))

    for i in n.items:
      result.add i.filter(setContException)

    proc consumeException(n: NimNode): NimNode =
      ## Prepend all cpsPending with `setCurrentException nil`
      # XXX: need a way to prevent multiple insertion of this
      if n.isScopeExit:
        result = newStmtList()
        # set the global exception to nil
        result.add newCall(bindSym"setCurrentException", newNilLit())
        # set our exception symbol to nil too
        result.add newAssignment(ex.resym(cont, contSym), newNilLit())
        result.add n
      elif n.kind == nnkReturnStmt:
        # we are moving to the next continuation
        result = newStmtList()
        # set the global exception to nil to prevent leaking it outside
        result.add newCall(bindSym"setCurrentException", newNilLit())
        result.add n

    result = result.filter(consumeException)

  # move the exception body into a handler
  result.cont = makeContProc(genSym(nskProc, "except"), cont, body)
  # rewrite the body of the handler to be exception-aware
  #
  # we perform this after the creation of the continuation to catch any
  # added annotations
  result.cont.body = setException(desym cont, result.cont.body)

  # create the new body for the except clause
  result.excpt.add newStmtList()
  # capture the exception into the continuation
  result.excpt.last.add newAssignment(ex, newCall(bindSym"getCurrentException"))
  # add the tail call to the created handler
  result.excpt.last.add tailCall(cont, result.cont.name)

  discard "workaround for NimNode.add() returning something"

macro cpsTryExcept(cont, ex, n: typed): untyped =
  ## A try statement tainted by a `cpsJump` and
  ## may require a jump to enter any handler.
  ##
  ## Only rewrite try with except branches.
  {.warning: "compiler workaround here".}
  let cont = cont
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
    let child = n[i]
    case child.kind
    of nnkExceptBranch:
      let (cont, excpt) = rewriteExcept(cont, ex, child)
      # push the handler continuation to the top
      result.add cont

      # add the rewritten except branch to the template
      tryTemplate.add excpt
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
  ## A try statement tainted by a `cpsJump` and
  ## may require a jump to enter finally.
  ##
  ## Only rewrite try with finally.
  result = newStmtList:
    n.errorAst "try with finally with splitting is not supported"

func newAnnotation(env: Env; n: NimNode; a: static[string]): NimNode =
  result = newCall bindSym(a)
  result.copyLineInfo n
  result.add env.first

proc annotate(parent: var Env; n: NimNode): NimNode =
  ## annotate `input` or otherwise prepare it for conversion into a
  ## mutually-recursive cps convertible form; the `parent` environment
  ## may be mutated as a side-effect, otherwise a new environment will be
  ## created which points to this parent.

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
  result.doc "annotate at " & n.lineAndFile

  # we're going to iterate over the (non-comment) children
  for i, nc in n.pairs:
    if nc.isNil:
      result.add nc
      continue

    # if it's a cps call,
    if nc.isCpsCall:
      let jumpCall = env.newAnnotation(nc, "cpsJump")
      jumpCall.add env.annotate(nc)
      if i < n.len - 1:
        jumpCall.add:
          env.annotate newStmtList(n[i + 1 .. ^1])
      result.add jumpCall
      return

    # we deal with the body of a try when processing the try itself,
    # so check to see if our parent is a try statement, and if not...
    if n.kind != nnkTryStmt:

      # and (only) if it's not the last node, we need to evaluate
      # whether we (may) want to issue a jump at this point
      if i < n.len-1 and nc.isCpsBlock:

        case nc.kind
        of nnkOfBranch, nnkElse, nnkElifBranch, nnkExceptBranch, nnkFinally:
          # these nodes will be handled by their respective parent nodes
          discard
        else:
          # control-flow will end here with a MayJump annotation
          let jumpCall = env.newAnnotation(nc, "cpsMayJump")
          jumpCall.add:
            env.annotate newStmtList(nc)           # this child node
          jumpCall.add:
            env.annotate newStmtList(n[i+1..^1])   # subsequent siblings
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
        # this is not a cps block, so all the break and continue
        # statements should be preserved
        result.add:
          restoreContinue:
            restoreBreak:
              env.annotate nc

    of nnkContinueStmt:
      result.add newCpsContinue(nc)

    of nnkBreakStmt:
      result.add newCpsBreak(nc, nc.breakLabel)

    of nnkBlockStmt:
      if nc.isCpsBlock:
        let jumpCall = env.newAnnotation(nc, "cpsBlock")
        if nc[0].kind != nnkEmpty:
          # add label if it exists
          jumpCall.add nc[0]
        jumpCall.add:
          env.annotate newStmtList(nc[1])
        result.add jumpCall
        return
      else:
        # this is not a cps block, so all the break
        # statements should be preserved
        var transformed = env.annotate(nc)
        transformed = transformed.restoreBreak(nc[0])
        result.add transformed

    of nnkWhileStmt:
      if nc.isCpsBlock:
        let jumpCall = env.newAnnotation(nc, "cpsWhile")
        jumpCall.add env.annotate nc[0]                 # add condition
        jumpCall.add env.annotate newStmtList(nc[1])    # add body
        result.add jumpCall
        return
      else:
        # this is not a cps block, so all the break and
        # continue statements should be preserved
        result.add:
          restoreContinue:
            restoreBreak:
              env.annotate nc

    of nnkTryStmt:
      if nc.isCpsBlock:
        let final = nc.findChild(it.kind == nnkFinally)
        var jumpCall: NimNode
        if final.isNil:                    # no finally!
          jumpCall = env.newAnnotation(nc, "cpsTryExcept")
          jumpCall.add env.getException()  # exception access
          jumpCall.add:
            newStmtList env.annotate(nc)   # try body
        else:
          let body =
            if nc.findChild(it.kind == nnkExceptBranch).isNil:
              nc                           # no exception branch!
            else:
              wrappedFinally(nc, final)    # try/try-except/finally
          jumpCall = env.newAnnotation(nc, "cpsTryFinally")
          jumpCall.add env.getException()  # exception access
          jumpCall.add:
            newStmtList env.annotate(body) # try body
        result.add jumpCall
        return
      else:
        # try statement with no cps complications
        result.add env.annotate(nc)

    # not a statement cps is interested in
    else:
      result.add env.annotate(nc)

macro cpsResolver(T: typed, n: typed): untyped =
  ## resolve any left over cps control-flow annotations
  ##
  ## this is not needed, but it's here so we can change this to
  ## a sanity check pass later.
  proc danglingCheck(n: NimNode): NimNode =
    ## look for un-rewritten control-flow then replace them with errors
    proc dangle(n: NimNode): NimNode =
      if n.kind == nnkPragma and n.len == 1 and
        (n.hasPragma"cpsContinue" or
         n.hasPragma"cpsBreak" or
         n.hasPragma"cpsPending"):
        errorAst(n, "cps error: un-rewritten cps control-flow")
      else: n
    filter(n, dangle)

  proc replacePending(n, replacement: NimNode): NimNode =
    ## Replace cpsPending annotations with something else, usually
    ## a jump to an another location. If `replacement` is nil, remove
    ## the annotation.
    proc resolved(n: NimNode): NimNode =
      if n.isCpsPending:
        result =
          if replacement.isNil:
            newEmptyNode()
          else:
            copyNimTree replacement
    result = filter(n, resolved)

  # grabbing the first argument to the proc as an identifier
  let cont = desym n.params[1][0]

  debugAnnotation cpsResolver, n:
    # replace all `pending` with the end of continuation
    it = replacePending it:
      tailCall cont:
        Dealloc.hook(T, cont)
    it = danglingCheck it

macro cpsFloater(n: typed): untyped =
  ## float all `{.cpsLift.}` to top-level
  proc floater(n: NimNode): NimNode =
    var floated = newStmtList()
    proc float(n: NimNode): NimNode =
      if not n.isNil and n.hasPragma("cpsLift"):
        var n = n.stripPragma("cpsLift")
        floated.add:
          floater n
        result = newEmptyNode()
    result = newStmtList [floated, filter(n, float)]

  result = floater:
    copyNimTree n

proc cpsTransformProc*(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style

  # make the AST easier for us to consume
  var n = normalizeProcDef n
  # establish a new environment with the supplied continuation type;
  # accumulates byproducts of cps in the types statement list
  var types = newStmtList()

  # creating the env with the continuation type,
  # and adding proc parameters to the env
  var env = newEnv(ident"continuation", types, T, n.returnParam)

  # add parameters into the environment
  for defs in n.callingParams:
    if defs[1].kind == nnkVarTy:
      error "cps does not support var parameters", n
    env.localSection(defs)

  # make a name for the new procedure that won't clash
  let name = genSym(nskProc, $n.name)

  # we can't mutate typed nodes, so copy ourselves
  n = clone n
  n.addPragma ident"used"  # avoid gratuitous warnings

  # the whelp is a limited bootstrap that merely makes
  # the continuation without invoking it in a trampoline
  let whelp = env.createWhelp(n, name)

  # setup the bootstrap using the old proc name,
  # but the first leg will be the new proc name
  let booty = env.createBootstrap(n, name)

  # we store a pointer to the whelp on the bootstrap
  booty.addPragma(bindSym"cpsBootstrap", whelp.name)

  # like magics, the bootstrap must jump
  booty.addPragma ident"cpsMustJump"

  # now we'll reset the name of the new proc
  n.name = name

  # do some pruning of these typed trees.
  for p in [booty, n]:
    p.name = desym(p.name)
    while len(p) > 7: p.del(7)     # strip out any extra result field

  # Replace the proc params: its sole argument and return type is T:
  #   proc name(continuation: T): T
  n.params = nnkFormalParams.newTree(T, env.firstDef)

  var body = newStmtList()     # a statement list will wrap the body
  body.introduce {Coop, Pass, Trace, Head, Tail, Alloc, Dealloc}
  body.add:
    Trace.hook env.first, n    # hooking against the proc (minus cloned body)
  body.add n.body              # add in the cloned body of the original proc

  # perform sym substitutions (or whatever)
  n.body = env.rewriteSymbolsIntoEnvDotField body

  # transform defers
  n.body = rewriteDefer n.body

  # rewrite non-yielding cps calls
  n.body = env.rewriteVoodoo n.body

  # annotate the proc's body
  n.body = env.annotate n.body

  if n.body.firstReturn.isNil:
    # fixes https://github.com/disruptek/cps/issues/145
    # by ensuring that we always rewrite termination
    n.body.add newCpsPending()

  # run other stages
  n.addPragma(bindSym"cpsFloater")
  n.addPragma(bindSym"cpsResolver", env.identity)

  # "encouraging" a write of the current accumulating type
  env = env.storeType(force = off)

  # generated proc bodies, remaining proc, whelp, bootstrap
  result = newStmtList(types, n, whelp, booty)
  result = workaroundRewrites result
