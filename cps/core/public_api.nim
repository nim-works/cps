import
  std/[macros, options],
  ./transform,
  ./spec

export cpsCall, Continuation, Coroutine

const cpsMutant* = true # We are all mutants

# Type Erasure
# --------------------------------------------------------------------------------------------
type
  ContinuationOpaque* = object
    ## A type erased continuation
    ## This can be used by schedulers
    # Gimme gimme gimme a VTable after midnight
    fn: proc(c: var ContinuationOpaque) {.nimcall.}
    frame: ref RootObj

    # Question:
    # - where is the RTTI info?
    # - casting "ref object" to "ref object of RootObj" defined behavior?

proc typeEraser(typedCont: var Continuation): var ContinuationOpaque {.inline.}=
  ## Type-erase a continuation
  # This is safe as ContinuationOpaque as the same size as the base continuation
  # and the GC has the RTTI of the frame field.
  # TODO: solve {.union.}
  # - If continuation is ref it's OK, but we need to cast to ref of RootObj, safe?
  # - runtimes who wants to manually manage memory can sizeof() the type.
  `=sink`(result, cast[var ContinuationOpaque](typedCont.addr))

# Internals
# --------------------------------------------------------------------------------------------

macro cps(T: typed, n: typed): untyped =
  # I hate doing stuff inside macros, call the proc to do the work
  result = cpsXfrm(T, n)

macro cpsMagic(n: untyped{nkProcDef}): untyped =
  ## upgrade cps primitives to generate errors out of context
  ## and take continuations as input inside {.cps.} blocks
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
  elif true:
    m.body.add nnkPragma.newNimNode(n).add newColonExpr(ident"warning",
                                                        msg.newLit)
  else:
    m.body.add nnkCall.newNimNode(n).newTree(ident"error", msg.newLit)
  # add it to our statement list result
  result.add m

  when not defined(nimdoc):
    # manipulate the primitive to take its return type as a first arg
    when not cpsMagicExists:
      n.params.insert(1, newIdentDefs(ident"c", n.params[0]))
    result.add n

proc coroYield[T](coro: var Coroutine[T], yieldedOut: T) {.inline.}=
  coro.promise = some yieldedOut
  # TODO:
  # If the continuation has no further yield
  # we need to set finish to true.
  # - How to do that? We do know at compiletime
  #   all the procs with no cpsCall and can compare
  #   the continuation with those.
  # - Or we use an option type, it would be nice
  #   if in the codegen the bool is put after the value
  #   to not waste space due to alignment.
  #
  #   Note: Option leads to unsatisfactory ergonomic
  #   let maybeA = counter.resume()
  #   if maybeA.isSome():
  #     ...
  #   else:
  #     ...
  #   doing a
  #   while not counter.hasFinished:
  #     ...
  #   is more natural.
  # - Or we make the finish check try to run a coro iteration
  #   if none were yielded but it doesn't return.
  #   and resume only returns it.
  # coro.hasFinished = coro.fn.hasCpsCall()

proc defFrame(name: string): NimNode =
  ## Add the base frame object
  # TODO: {.union.} types

  # TODO: should be gensym'ed or derived from
  # the proc signature to ensure type unicity

  return nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      ident("cpsFrame_" & name), # TODO: unique ID
      newEmptyNode(),
      nnkObjectTy.newTree(
        newEmptyNode(),
        nnkOfInherit.newTree ident"RootObj",
        newEmptyNode()
      )
    )
  )

proc frameValueOrRef*(
       name: string,
       escapesScope = true, isTrivial = false): NimNode =
  if escapesScope or not isTrivial:
    nnkRefTy.newTree(ident(name))
  else:
    ident(name)

proc defBaseContinuation(name: string): NimNode =
  ## Typedef the base continuation
  # TODO: should be gensym'ed or derived from
  # the proc signature to ensure type unicity
  let frameName = "cpsFrame_" & name
  let name = ident(name)

  nnkTypeSection.newTree(
    nnkTypeDef.newTree(
      name,
      newEmptyNode(),
      nnkObjectTy.newTree(
        newEmptyNode(),
        newEmptyNode(),
        nnkRecList.newTree(
          # fn: proc(c: var ContinuationName) {.nimcall.}
          newIdentDefs(
            ident"fn",
            nnkProcTy.newTree(
              nnkFormalParams.newTree(
                newEmptyNode(),
                newIdentDefs(ident"c", nnkVarTy.newTree(name))
              ),
              nnkPragma.newTree(ident"nimcall")
            )
          ),
          newIdentDefs(ident"frame", frameValueOrRef(frameName))        )
      )
    )
  )

proc resumableImpl(def: NimNode): NimNode =
  ## CPS transforms a proc definition:
  ## 1. Associate an unique Continuation type
  ## 2. CPS-transform the proc
  ## 3. Generate en entry point to the CPS world
  ## 4. ....
  ## 5. Profit!
  # Generate a type for this proc
  let typeName = "Continuation_" & $def.name

  result = newStmtList()
  result.add defFrame(typeName)
  result.add defBaseContinuation(typeName)

  # Seems like cpsXfrm wants typed body,
  # so we just replace {.resumable.} with {.cps:Type.}
  # result.add cpsXfrm(ident(typeName), def)
  let cpsMacro = bindSym"cps"
  def.addPragma(nnkExprColonExpr.newTree(cpsMacro, ident(typeName)))
  result.add def

proc suspendProcDefImpl(def: NimNode): NimNode =
  ## Inserts the continuation as the first param
  ## Provides a call `bindCallerContinuation`
  ## to capture the caller continuation.
  ## The caller will be suspended on this function exit.

  let cont = genSym(nskParam, "cont")

  result = copyNimTree(def)
  result.params.insert 1, newIdentDefs(cont, nnkVarTy.newTree ident"Continuation")

  let typeEraser = bindSym"typeEraser"

  result.body = newStmtList()
  result.body.add quote do:
    template bindCallerContinuation(): untyped =
      `typeEraser`(`cont`) # how to enforce move?
  def.body.copyChildrenTo result.body

  result.addPragma bindSym"cpsMagic"

# Proc definitions
# --------------------------------------------------------------------------------------------

macro resumable*(def: untyped): untyped =
  ## Create a resumable procedure.
  ## A resumable procedure can suspend its execution
  ## and return control to its current caller.
  ## A resumable procedure starts suspended and returns a handle
  ## to be able to resume it.
  ##
  ## A resumable procedure may call `{.suspend.}` procedures,
  ## that will suspend and return control to the caller.
  ##
  ## A resumable procedure handle cannot be copied, only moved
  ## and MUST be mutable. It can be moved and resumed from any thread.
  ##
  ## Suspension points are called with `suspend`.
  ## A resumable procedure cannot have a result type, use a coroutine instead.
  ##
  ## If a resumable procedure captures resources that are non-trivial
  ## to release, cancellation MUST be cooperative,
  ## the resumable function should use a channel as "cancellation token"
  ## that would be checked after each suspension point.
  ##
  ## Due to non-linear, movable and interruptible control flow, there are important caveats:
  ## - Using {.threadvar.} will result in undefined behavior.
  ##   Resumable functions have their own local storage.
  ## - `alloca` will not be preserved across suspension points.
  ## - `setjmp`/`longjmp` across suspension point will result in undefined behavior.
  ##   Nim exceptions will be special-cased.
  def.expectKind(nnkProcDef)
  return def.resumableImpl()

macro suspend*(def: untyped): untyped =
  ## Tagging a proc {.suspend.}:
  ## - makes the proc suitable to suspend its caller.
  ## - allows capture of the raw caller continuation,
  ##   for example to store it in a scheduler queue
  ##   and resume it at a more opportune time
  ## - Suspending proc can only be called within a `{.resumable.}` proc
  ##   or another `{.suspend.}` proc.
  ## - Suspending proc MUST be called with "suspendAfter myProc"
  ##
  ## The caller continuation can be saved with `bindCallerContinuation`
  ## Not running a saved continuation is equivalent to cancelling it.
  ##
  ## If a resumable procedure captures resources that are non-trivial
  ## to release, cancellation MUST be cooperative,
  ## the resumable function should use a channel as "cancellation token"
  ## that would be checked after each suspension point.
  ##
  ## Due to non-linear, movable and interruptible control flow, there are important caveats:
  ## - Using {.threadvar.} will result in undefined behavior.
  ##   Resumable functions have their own local storage.
  ## - `alloca` will not be preserved across suspension points.
  ## - `setjmp`/`longjmp` across suspension point will result in undefined behavior.
  ##   Nim exceptions will be special-cased.
  def.expectKind nnkProcDef
  return suspendProcDefImpl(def)

macro coro*(def: untyped): untyped =
  ## Create a coroutine
  ## A coroutine can suspend its execution
  ## and return control to its current caller.
  ## A coroutine starts suspended and returns a handle
  ## to be able to resume it.
  ##
  ## A coroutine handle cannot be copied, only moved
  ## and MUST be mutable.
  ##
  ## A coroutine cannot yield without a result, use a resumable procedure instead.
  ##
  ## Suspension points are called with `yield` and the value must be properly typed.
  ##
  ## Due to non-linear, movable and interruptible control flow, there are important caveats:
  ## - Using {.threadvar.} will result in undefined behavior.
  ##   Resumable functions have their own local storage.
  ## - `alloca` will not be preserved across suspension points.
  ## - `setjmp`/`longjmp` across suspension point will result in undefined behavior.
  ##   Nim exceptions will be special-cased.
  def.expectKind({nnkProcDef,nnkFuncDef})

# Calls
# --------------------------------------------------------------------------------------------

# proc resume*[T](coro: var Coroutine[T]): Option[T] {.inline.} =
#   ## Resume a coroutine until its next `yield`
#   while coro.fn != nil and not coro.hasYielded:
#     coro.fn(coro)
#   if coro.promise.isSome():
#     result = move coro.promise
#     # TODO: set hasFinished here or we will be one iteration late.
#     # - a naive solution would be to run the loop here.
#     # - another is to check if coro.fn is nil or without cpsCall.
#   else:
#     coro.hasFinished = true

proc resume*(cont: var (Continuation|ContinuationOpaque)) {.inline.} =
  ## Resume a continuation until its next `suspend`
  static: doAssert not (cont is Coroutine), "Dispatch overload bug"
  while cont.fn != nil:
    cont.fn(cont)

template suspendAfter*(procCall: untyped): untyped =
  ## Call a suspending function.
  ## Suspending function are defined with {.suspend.}
  procCall
