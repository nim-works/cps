import std/[macros, sequtils, sets, tables, hashes]
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
  let name = nskProc.genSym"Post Call"
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
  let name = nskProc.genSym"Finish"
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
  let name = nskProc.genSym"While Loop"
  let tail = tailCall(desym cont, name)
  tail.copyLineInfo n
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
    body.copyLineInfo n
    # we return the while proc and a tailcall to enter it the first time
    it = newStmtList [makeContProc(name, cont, body), tail]

proc mergeExceptBranches(n, ex: NimNode): NimNode =
  ## Rewrite the try statement `n` so that all `except` branches are merged
  ## into one branch, with `ex` assumed to store the exception being handled.
  ##
  ## In the case where there is only one except branch and it's a catch-all
  ## branch, no rewrite will happen.
  result = n
  # If there is at least one except branch with at least one catching constraint
  if n.findChild(it.kind == nnkExceptBranch and it.len > 1) != nil:
    # Copy the try statement and the try body
    result = copyNimNode(n)
    result.add n[0]

    # To achieve the merge, we perform it like so:
    #
    # Assume the AST
    #
    # try:
    #   body
    # except T:
    #   handlerT
    # except:
    #   handler
    #
    # Turn all `except T` branches into: `elif ex of T`.
    # Turn all `except` branches into: `else`.
    # Then place all of them under an `if`, like this:
    #
    # if ex of T:
    #   handlerT
    # else:
    #   handler
    #
    # Then rewrite the AST into:
    #
    # try:
    #   body
    # except:
    #   if ex of T:
    #     handlerT
    #   else:
    #     handler
    let ifStmt = newNimNode(nnkIfStmt)

    for idx in 1 ..< n.len:
      let branch = n[idx]
      # Branch has a matching constraint
      if branch.len > 1:
        let elifBranch = newNimNode(nnkElifBranch, branch)

        # Create the matching condition
        var cond: NimNode
        # Collect the exception constraints, which are the nodes before
        # the very last node
        for idx in 0 ..< branch.len - 1:
          # In normalized AST, the matching constraint is the exception
          # type.
          #
          # To check if the exception is of the matched type, we generates:
          #
          #     ex of (ref T)
          #
          # The reason for using `ref T` is because Nim exception types are
          # defined as object types.
          let match =
            # ex of
            nnkInfix.newTree(bindSym"of", ex):
              # ref T
              nnkRefTy.newTree:
                branch[idx]

          # If there aren't any condition
          if cond.isNil:
            # Set the matching constraint as the condition
            cond = match
          else:
            # Push the matching constraint as an alternative match
            cond = nnkInfix.newTree(bindSym"or", cond, match)

        # Add the matching condition
        elifBranch.add cond
        # Add the handler body
        elifBranch.add branch.last
        # Add the branch to the if statement
        ifStmt.add elifBranch
      else:
        # This is a plain except branch
        let elseBranch = newNimNode(nnkElse, branch)
        # Add the handler body
        elseBranch.add branch.last
        # Add the branch to the if statement
        ifStmt.add elseBranch

    # In the case where there are no `else` branch (due to the lack of
    # an empty except branch)
    if ifStmt.last.kind != nnkElse:
      # Since there are no handler for other cases, we reraise the exception
      ifStmt.add:
        # else:
        #   raise ex
        nnkElse.newTree:
          newStmtList:
            nnkRaiseStmt.newTree:
              ex

    # Add the merged except branch to our try statement
    result.add:
      nnkExceptBranch.newTree:
        newStmtList:
          ifStmt

proc wrapContinuationWith(n, cont, replace, templ: NimNode): NimNode =
  ## Given the StmtList `n`, return `templ` with children matching `replace`
  ## replaced with the `n`.
  ##
  ## `cont` is the continuation symbol of `n`.
  ##
  ## Perform the replacements on bodies of continuations within `n` as well.
  proc wrapCont(n: NimNode): NimNode =
    ## Wrap the body of continuation `n` with the template
    if n.isCpsCont:
      result = n
      let nextCont = n.getContSym
      result.body = result.body.wrapContinuationWith(nextCont, replace):
        templ.resym(cont, nextCont)

  result = copyNimTree templ
  # perform body replacement
  result = result.replace(proc(x: NimNode): bool = x == replace):
    newStmtList:
      # wrap all continuations of `n` with the template
      filter(n, wrapCont)

proc withException(n, cont, ex: NimNode): NimNode =
  ## Given the exception handler continuation `n`, set the global exception of
  ## continuation `cont` in the scope of `n` to `ex` before any code is run.
  ##
  ## At the end of this or its child continuations, the exception will be
  ## restored to what it was before the continuation was run.
  proc rewriter(n: NimNode): NimNode =
    # If n is an unmanaged continuation
    if n.isCpsCont and not n.hasPragma("cpsHasException"):
      # Create a copy of this continuation and rename it
      let inner = n.ProcDef.clone(n.body).NimNode
      inner.name = nskProc.genSym("Managed_" & inner.name.strVal)
      # Copy the continuation pragmas
      inner.pragma = copyNimTree n.pragma
      # Mark as managed
      inner.addPragma bindSym"cpsHasException"
      # Rewrite the continuations contained in `inner` to use `ex` as its
      # exception
      inner.body = inner.body.withException(cont, ex)

      # Make an another clone, but with an empty body instead
      #
      # This will be the manager and will replace `n`
      result = n.ProcDef.clone(newStmtList())
      # Obtain the continuation name to replace it
      result.name = n.name
      # Take its pragmas too
      result.pragma = n.pragma
      # Mark as managed since it's the manager
      result.addPragma bindSym"cpsHasException"

      # Desym the continuation symbol
      result.params[1][0] = desym result.params[1][0]

      # Now let's go down to business
      # Add the inner continuation inside the manager
      result.body.add inner

      # TODO: the desym here is because generateContinuation will end up with
      # the same symbols on every generated continuation, causing a capturing
      # issue.
      let
        oldException = desym nskLet.genSym"oldException"
        managerCont = result.getContSym
        managerEx = ex.resym(cont, managerCont)
        innerFn = inner.name
      result.body.add:
        genAst(oldException, managerCont, managerEx, innerFn, result = ident"result"):
          # Save the old exception from the enviroment
          let oldException = getCurrentException()
          # Set the exception to what is in the continuation
          setCurrentException(managerEx)
          # Run our continuation
          result = innerFn(managerCont)
          # Restore the exception
          setCurrentException(oldException)

  filter(n, rewriter)

macro cpsTryExcept(cont, ex, n: typed): untyped =
  ## A try statement tainted by a `cpsJump` and
  ## may require a jump to enter any handler.
  ##
  ## Only rewrite try with except branches.
  result = newStmtList()

  var
    n = normalizingRewrites n
    ex = normalizingRewrites ex

  # merge all except branches into one
  n = n.mergeExceptBranches(ex)

  # grab the merged except branch and collect its body
  let handlerBody = n.findChild(it.kind == nnkExceptBranch)[0]
  var handler = makeContProc(genSym(nskProc, "Except"), cont, handlerBody)

  # rewrite the handler to use the correct exception
  handler = handler.withException(cont, ex)

  # add our handler before the body
  result.add handler

  # write a try-except clause to wrap on all children continuations so that
  # they jump to the handler upon an exception
  let placeholder = genSym(nskUnknown, "placeholder")
  let tryTemplate = copyNimNode n

  # add the placeholder as the body
  tryTemplate.add placeholder

  # add an except node that captures the exception into `ex`, then
  # jump to handler
  let jumpBody = newStmtList()
  jumpBody.add:
    # captures the exception to ex
    newAssignment(ex, newCall(bindSym"getCurrentException"))
  jumpBody.add:
    # jump to the handler
    cont.tailCall(handler.name)

  # add this to our template as an except branch
  tryTemplate.add:
    nnkExceptBranch.newTree:
      jumpBody

  # wrap the try body and everything in it with the template
  result.add n[0].wrapContinuationWith(cont, placeholder, tryTemplate)

  result = workaroundRewrites result

macro cpsTryFinally(cont, ex, n: typed): untyped =
  ## A try statement tainted by a `cpsJump` and
  ## may require a jump to enter finally.
  ##
  ## Only rewrite try with finally.
  let ex = normalizingRewrites ex
  debugAnnotation cpsTryFinally, n:
    # rewriteIt will wrap our try-finally in a StmtList, so we take it out
    let tryFinally = it[0]
    # use a fresh StmtList as our result
    it = newStmtList()

    # The previous pass should give us a try-finally block, thus the last
    # child is our finally, and the last child of finally is its body.
    let finallyBody = tryFinally.last.last

    # Turn the finally into a continuation leg.
    var final = makeContProc(nskProc.genSym("Finally"), cont, finallyBody)

    # A property of `finally` is that it inserts itself in the middle
    # of any scope exit attempt before performing the scope exit.
    #
    # To achieve this, we generates specialized finally legs that
    # dispatches to each exit points.
    #
    # XXX: While this generator works perfectly for our usage, it is not
    # the best way to template. Preferrably we would use generics, which
    # is blocked by nim-lang/Nim#18254.
    proc generateContinuation(templ, replace, replacement: NimNode): NimNode =
      ## Given a continuation template `templ`, replace all `replace` with
      ## `replacement`, then generate a new set of symbols for all
      ## continuations within.
      # A simple mapping of node and what to replace it with
      var replacements: Table[NimNode, NimNode]
      proc generator(n: NimNode): NimNode =
        # If it's a continuation
        if n.isCpsCont:
          result = copyNimTree(n)
          # Desym the continuation to unshare it with other copies
          let cont = result.getContSym
          replacements[cont] = desym cont
          # Make sure that we also desym the parameter
          result.params = result.params.filter(generator)
          # Generate a new name for this continuation, then add it to our
          # replacement table
          replacements[n.name] = genSym(n.name.symKind, n.name.strVal)
          result.name = replacements[n.name]
          # Rewrite the body to update the references within it
          result.body = result.body.filter(generator)

        elif n in replacements:
          result = copyNimTree(replacements[n])

      replacements[replace] = replacement
      result = filter(templ, generator)

    # Create a symbol to use as the placeholder for the finally leg next jump.
    let nextJump = nskUnknown.genSym"nextJump"

    # Replace all cpsPending within the final leg with this placeholder
    final.body = final.body.replace(isCpsPending, nextJump)

    # Set the global exception to `ex` before running any other code
    # in the finally leg
    final = final.withException(cont, ex)

    # This is a table of exit points, the key being the exit annotation, and
    # the value being the finally leg specialized to that exit.
    var exitPoints: OrderedTable[NimNode, NimNode]
    proc redirectExits(n, cont, final: NimNode): NimNode =
      ## Redirect all scope exits in `n` so that they jump through a
      ## specialized version of `final`.
      proc redirector(n: NimNode): NimNode =
        if n.isCpsCont:
          let nextCont = n.getContSym
          result = n
          result.body = result.body.redirectExits(nextCont, final)
        elif n.isScopeExit:
          # If this exit point is not recorded yet
          if n notin exitPoints:
            # Create a new continuation containing the exit point
            exitPoints[n] = final.generateContinuation(nextJump):
              n

          # Replace it with a tail call to finally with the exit point as the
          # destination.
          result = cont.tailCall exitPoints[n].name

      n.filter(redirector)

    # Redirect all exits within the finally
    let body = tryFinally[0].redirectExits(cont, final)

    # Add the specialized finally legs we generated to the AST
    for exit in exitPoints.values:
      it.add exit

    # While we covered all the exits explicitly written the body, we haven't
    # covered the exits that are not explicitly written in the body, which are
    # unhandled exceptions.
    #
    # Generate a finally leg that raises the current exception. This is used to
    # re-raise an unhandled exception.
    let reraise = final.generateContinuation(nextJump):
      newStmtList:
        nnkRaiseStmt.newTree:
          # de-sym our `ex` so that it uses the continuation of where it is
          # replaced into
          ex.resym(cont, desym cont)

    # Add this leg to the AST
    it.add reraise

    # Now we create a try-except template to catch any unhandled exception that
    # might occur in the body, then jump to our finally with reraise as
    # the destination
    let
      tryTemplate = copyNimNode(tryFinally)
      placeholder = genSym()

    # Use the placeholder for the actual body
    tryTemplate.add placeholder

    # Add an except branch to jump to our finally
    let reraiseJump = newStmtList()
    reraiseJump.add:
      # Stash the exception to `ex`
      newAssignment(ex, newCall(bindSym"getCurrentException"))

    reraiseJump.add:
      # Jump to finally with continuation target `reraise`
      tailCall(cont, reraise.name)

    tryTemplate.add:
      nnkExceptBranch.newTree:
        reraiseJump

    # Wrap the body with this template and we are done
    it.add body.wrapContinuationWith(cont, placeholder, tryTemplate)

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
      # stored in the environment; note that we're adding a new
      # return statement without regard to the contents of `result`
      # because it may hold, eg. `ElifBranch ...` or similar.
      result.add:
        env.rewriteReturn nc
      return

    of nnkVarSection, nnkLetSection:
      let section = expectVarLet(nc)
      if isCpsCall(section.val):
        result.add:
          nc.last.errorAst "shim is not yet supported"
      elif isCpsBlock(section.val):
        result.add:
          nc.last.errorAst "only calls are supported here"
      else:
        # add definitions into the environment
        env.localSection(section, result)

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
          jumpCall.add env.genException()  # exception access
          jumpCall.add:
            newStmtList env.annotate(nc)   # try body
        else:
          let body =
            if nc.findChild(it.kind == nnkExceptBranch).isNil:
              nc                           # no exception branch!
            else:
              wrappedFinally(nc, final)    # try/try-except/finally
          jumpCall = env.newAnnotation(nc, "cpsTryFinally")
          jumpCall.add env.genException()  # exception access
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
      if n.isScopeExit:
        errorAst(n, "cps error: un-rewritten cps control-flow")
      else: n
    filter(n, dangle)

  # grabbing the first argument to the proc as an identifier
  let cont = desym n.params[1][0]

  debugAnnotation cpsResolver, n:
    # replace all `pending` and `terminate` with the end of continuation
    it = replace(it, proc(x: NimNode): bool = x.isCpsPending or x.isCpsTerminate):
      if n.firstReturn.isNil:
        terminator(cont, T)
      else:
        doc"omitted a return in the resolver"

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

  # keep the original symbol of the proc
  let originalProcSym = n[0]
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

  # give the booty the sym we got from the original, which
  # causes the bootstrap symbol to adopt the original procedure's symbol;
  # this is a workaround for nim bugs:
  # https://github.com/nim-lang/Nim/issues/18203 (used hints)
  # https://github.com/nim-lang/Nim/issues/18235 (export leaks)
  booty.name = originalProcSym

  # now we'll reset the name of the new proc
  # XXX: not sure why we need to desym, but we have to or it won't be
  # matched inside booty
  n.name = desym(name)

  # do some pruning of these typed trees.
  for p in [booty, n]:
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
