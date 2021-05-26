import std/[macros, sequtils, algorithm]
import cps/[spec, environment, hooks]
export Continuation, ContinuationProc, cpsCall
export cpsDebug

#{.experimental: "strictNotNil".}

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

proc isVoodooCall(n: NimNode): bool =
  ## true if this node holds a call to a cps procedure
  if n != nil and len(n) > 0:
    if n.kind in nnkCallKinds:
      let callee = n[0]
      # all cpsCall are normal functions called via a generated symbol
      if not callee.isNil and callee.kind == nnkSym:
        result = callee.getImpl.hasPragma("cpsVoodooCall")

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
        hook Coop:
          n
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
  of nnkForStmt, nnkBlockStmt, nnkBlockExpr, nnkElse, nnkElseExpr,
     nnkOfBranch, nnkExceptBranch, nnkFinally:
    return n.last.isCpsBlock
  of nnkStmtList, nnkStmtListExpr, nnkIfStmt, nnkIfExpr, nnkCaseStmt,
     nnkWhileStmt, nnkElifBranch, nnkElifExpr, nnkTryStmt:
    for n in n.items:
      if n.isCpsBlock:
        return true
  of nnkCallKinds:
    return n.isCpsCall
  else:
    return false

proc isCpsExpr(n: NimNode): bool =
  ## `true` if the `n` is an expression with a cps call in it
  n.isExpr and n.isCpsBlock

proc rewriteCpsExpr(n: NimNode): NimNode =
  ## flatten all cps expression inside `n` so that they can
  ## be rewritten into continuations
  proc needRewrite(n: NimNode): bool =
    ## whether `n` contains a cps expression that might have to be moved
    ## outside
    case n.kind
    of nnkIdentDefs:
      result = n.last.needRewrite
    of nnkVarSection, nnkLetSection, nnkIfStmt, nnkIfExpr, nnkCaseStmt,
       nnkElifBranch, nnkElifExpr, nnkElse, nnkElseExpr, CallNodes:
      for child in n.items:
        if child.needRewrite:
          return true
    of nnkWhileStmt:
      result = n[0].needRewrite
    else:
      result = n.isCpsExpr

  proc flatten(n: NimNode): NimNode =
    if n.needRewrite:
      case n.kind
      of nnkVarSection, nnkLetSection:
        # this var/let section have at least one declaration requiring
        # flattening
        result = newStmtList()

        let newSection = copyNimNode n
        for idx, defs in n.pairs:
          let value = defs[^1]
          # if this definition has a cps expression block but
          # not a call (we rewrite those in saften)
          if value.needRewrite and not value.isCpsCall:
            if newSection.len > 0:
              # there are more than one ident defs in here
              # push the new var/let section to the body
              result.add newSection

              # create a new one containing the rest, flatten it,
              # then add to the body
              result.add:
                rewriteCpsExpr:
                  copyNimNode(newSection).add n[idx .. ^1]

              # we are done here
              return

            # flatten then rewrite the expression
            let (newValue, stmt) =
              exprAsSym:
                rewriteCpsExpr value
            # move it to before the var/let section
            result.add stmt

            # make a new IdentDefs
            let newDefs = copyNimNode defs
            # copy everything but the value
            for idx in 0 ..< defs.len - 1:
              newDefs.add defs[idx]
            # set the value to what we want
            newDefs.add newValue

            # add the new defs to the section
            newSection.add newDefs
          else:
            # add as-is
            newSection.add defs

        result.add newSection

      of nnkAsgn, CallNodes:
        result = newStmtList()

        # find the last position that require rewriting
        var lastRewriteAt = 0
        for idx, child in n.pairs:
          if child.needRewrite:
            lastRewriteAt = idx

        let newNode = copyNimNode n

        # rewrite everything before that
        #
        # this will create a lot of locals, but it has to be done to
        # preserve the evaluation order
        #
        # TODO: perform analysis to figure out whether we can skip
        # creating locals
        for idx in 0 .. lastRewriteAt:
          let child = n[idx]

          if idx == 0:
            # TODO: This is not the right thing, but we can't
            # do it any other way atm
            newNode.add child
          elif child.kind in nnkLiterals:
            # we can skip locals for literals, since they are constant
            newNode.add child
          else:
            let (val, stmt) =
              exprAsSym:
                rewriteCpsExpr child

            result.add stmt

            # substitute the expression with the temporary
            newNode.add val

        # add the remaining nodes
        for idx in lastRewriteAt + 1 ..< n.len:
          newNode.add n[idx]

        result.add newNode

      of nnkWhileStmt:
        # the condition of the while statement is an expr
        # create an infinite loop (while true)
        result = newNimNode(n.kind)
        result.add newLit(true)

        # rewrite the body into:
        # if <cond>:
        #   <body>
        # else:
        #   break
        #
        # then flatten it
        result.add:
          rewriteCpsExpr:
            nnkIfStmt.newTree [
              nnkElifBranch.newTree [
                n[0],
                n[1]
              ],

              nnkElse.newTree [
                newStmtList [
                  nnkBreakStmt.newTree(newEmptyNode())
                ]
              ]
            ]

      of nnkCaseStmt:
        # if the evaluated expression is a cps expression or there are
        # elif branches with a cps expression as the condition.
        if n[0].needRewrite or
           n.findChild(it.kind in {nnkElifBranch, nnkElifExpr} and
                       it[0].needRewrite) != nil:
          result = newStmtList()
          let newCase = copyNimNode(n)

          # rewrite the expression to assign to the temporary and
          # push it above the case statement
          if n[0].needRewrite:
            let (sym, decl) =
              exprAsSym:
                rewriteCpsExpr n[0]

            result.add decl
            newCase.add sym
          else:
            result.add n[0]

          # rewrite all branches nodes
          for idx in 1 ..< n.len:
            let child = n[idx]
            # if this is an elif branch with an condition need rewriting
            if child.kind in {nnkElifBranch, nnkElifExpr} and
               child[0].needRewrite:
              let ifStmt = newNimNode nnkIfStmt
              # collect this and the remaining elif branches to add to the
              # if statement
              #
              # there can't be any `of` branch after this, so we can
              # safely collect everything to the if
              ifStmt.add n[idx .. ^1]

              # rewrite and add the new if statement as the else branch of
              # this case
              newCase.add:
                nnkElse.newTree:
                  rewriteCpsExpr ifStmt

              # we are done
              break
            else:
              newCase.add rewriteCpsExpr(child)

          result.add newCase

      of nnkIfStmt, nnkIfExpr:
        # if there are branches with condition being a cps expr
        if n.findChild(it.kind in {nnkElifBranch, nnkElifExpr} and
                       it[0].needRewrite) != nil:
          result = newStmtList()

          let newIf = copyNimNode n

          for idx, child in n.pairs:
            # if this branch has a cps expression as condition
            if child.kind in {nnkElifBranch, nnkElifExpr} and
               child[0].needRewrite:
              # if there are some branches the new if statement
              if newIf.len > 0:
                # move this and all later branches into a new if
                let leftovers = copyNimNode(newIf).add n[idx .. ^1]

                # rewrite this if then add it as the `else` branch
                newIf.add:
                  nnkElse.newTree:
                    rewriteCpsExpr(leftovers)

                # we are done here
                break

              # create a variable to assign the condition result to
              let (cond, decl) =
                exprAsSym:
                  rewriteCpsExpr child[0]
              result.add decl

              let newBranch = copyNimNode child
              # evaluate the temporary instead
              newBranch.add cond
              # add the rewrite of the body
              newBranch.add rewriteCpsExpr(child[1])

              # add the branch into the if statement
              newIf.add newBranch
            else:
              # add the rewrite of this else branch
              newIf.add rewriteCpsExpr(child)

          # add the new if to the result
          result.add newIf

      else:
        discard

  filter(n, flatten)

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

proc saften(parent: var Env; n: NimNode): NimNode

proc makeContProc(name, cont, source: NimNode): NimNode =
  ## creates a continuation proc from with `name` using continuation
  ## `cont` with the given body.
  let
    contParam = desym cont
    contType = getTypeInst cont

  result = newProc(name, [contType, newIdentDefs(contParam, contType)])
  result.copyLineInfo source        # grab lineinfo from the source body
  result.body = newStmtList()       # start with an empty body
  result.introduce {Coop, Trace, Alloc, Dealloc}    # mix any hooks in
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

    # FIXME: fix this to use makeReturn

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

proc rewriteExcept(cont, ex, n: NimNode): tuple[cont, excpt: NimNode] =
  ## Rewrite the exception branch `n` to use `ex` as the "current" exception
  ## and move its body into a continuation
  doAssert n.kind == nnkExceptBranch

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
  result = newStmtList()
  result.add errorAst("try with finally with splitting is not supported", n)

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
      let jumpCall = newCall(bindSym"cpsJump", env.first)
      jumpCall.add:
        env.saften nc
      if i < n.len - 1:
        jumpCall.add:
          env.saften newStmtList(n[i + 1 .. ^1])
      result.add jumpCall
      return

    # we deal with the body of a try when processing the try itself,
    # so check to see if our parent is a try statement, and if not...
    if n.kind != nnkTryStmt:

      # and (only) if it's not the last node, we need to evaluate
      # whether we (may) want to issue a jump at this point
      if i < n.len-1 and nc.isCpsBlock:

        case nc.kind
        of nnkOfBranch, nnkElse, nnkElifBranch, nnkExceptBranch, nnkFinally,
           nnkElifExpr, nnkElseExpr:
          # these nodes will be handled by their respective parent nodes
          discard
        else:
          # control-flow will end here with a MayJump annotation
          let jumpCall = newCall(bindSym"cpsMayJump", env.first)
          jumpCall.add:
            env.saften newStmtList(nc)               # this child node
          jumpCall.add:
            env.saften newStmtList(n[i + 1 .. ^1])   # subsequent siblings
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
        # this is not a cps block, so all the break and continue
        # statements should be preserved
        transformed = restoreBreak transformed
        transformed = restoreContinue transformed
        result.add transformed

    of nnkContinueStmt:
      result.add newCpsContinue()
      result[^1].copyLineInfo nc

    of nnkBreakStmt:
      result.add newCpsBreak(nc.breakLabel)
      result[^1].copyLineInfo(nc)

    of nnkBlockStmt, nnkBlockExpr:
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
        # this is not a cps block, so all the break
        # statements should be preserved
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
        # this is not a cps block, so all the break and
        # continue statements should be preserved
        var transformed = env.saften(nc)
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
        # try statement with no cps complications
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
  result.copyLineInfo n

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
    tailCall cont:
      Dealloc.hook(T, cont)
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

proc rewriteVoodoo(n: NimNode, env: Env): NimNode =
  ## Rewrite non-yielding cpsCall calls by inserting the continuation as
  ## the first argument
  proc aux(n: NimNode): NimNode =
    if n.isVoodooCall:
      result = n.copyNimTree
      result[0] = desym result[0]
      result.insert(1, env.first)
  n.filter(aux)

proc cpsXfrmProc*(T: NimNode, n: NimNode): NimNode =
  ## rewrite the target procedure in Continuation-Passing Style

  # Some sanity checks
  n.expectKind nnkProcdef
  if not n.params[0].isEmpty:
    error "do not specify a return-type for .cps. functions", n

  if T.isNil:
    error "specify a type for the continuation with .cps: SomeType", n

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
  booty.introduce {Alloc, Dealloc}    # we may actually dealloc here
  booty.body.add:
    env.createContinuation booty.name

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

  var body = newStmtList()     # a statement list will wrap the body
  body.introduce {Trace, Alloc, Dealloc}   # prepare hooks
  body.add:
    Trace.hook env.first, n    # hooking against the proc (minus cloned body)
  body.add n.body              # add in the cloned body of the original proc

  # perform sym substitutions (or whatever)
  n.body = env.rewriteSymbolsIntoEnvDotField body

  # transform defers
  n.body = rewriteDefer n.body

  # rewrite cps exprs
  n.body = rewriteCpsExpr n.body

  # rewrite non-yielding cps calls
  n.body = rewriteVoodoo(n.body, env)

  # ensaftening the proc's body
  n.body = env.saften(n.body)

  # add in a pragma so other cps macros can identify this as a cps call
  n.addPragma ident"cpsCall"

  # run other stages
  n.addPragma bindSym"cpsFloater"
  n.addPragma nnkExprColonExpr.newTree(bindSym"cpsResolver", env.identity)

  # "encouraging" a write of the current accumulating type
  env = env.storeType(force = off)

  # lifting the generated proc bodies
  result = lambdaLift(types, n)

  # adding in the bootstrap
  result.add booty

  # spamming the developers
  debug(".cps.", result, Transformed)
