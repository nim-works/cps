import std/[macros]
import cps/[spec, xfrm]
export Continuation, ContinuationProc, cpsCall
export cpsDebug, cpsTrace

type
  State* {.pure.} = enum
    ## Representation of the state of a continuation.
    Running    ## The continuation is active and running and can be resumed
    Dismissed  ## The continuation is currently somewhere else
    Finished   ## The continuation is finished and can no longer be resumed

proc state*(c: Continuation): State =
  ## Get the current state of a continuation
  if c == nil:
    Dismissed
  elif c.fn == nil:
    Finished
  else:
    Running

macro cps*(T: typed, n: typed): untyped =
  ## This is the .cps. macro performing the proc transformation
  when defined(nimdoc):
    result = n
  else:
    result = cpsXfrmProc(T, n)
    result = workaroundRewrites(result)

macro cpsMagic*(n: untyped): untyped =
  ## Upgrades a procedure to serve as a CPS primitive, generating
  ## errors out of `.cps.` context and taking continuations as input.
  expectKind(n, nnkProcDef)

  # Add .cpsCall. pragma to the proc
  n.addPragma ident"cpsCall"

  # create a Nim-land version of the proc that throws an exception when called
  # from outside of CPS-land.
  var m = copyNimTree n
  m.params[0] = newEmptyNode()
  del(m.params, 1)
  m.body = newStmtList:
    nnkRaiseStmt.newTree:
      newCall(
        bindSym"newException",
        nnkBracketExpr.newTree(bindSym"typedesc", bindSym"Defect"),
        newLit($n.name & "() is only valid in {.cps.} context")
      )

  result = newStmtList()
  when not defined(nimdoc):
    result.add n
  result.add m

template running*(c: Continuation): bool =
  ## `true` if the continuation is running.
  c.state == Running

template finished*(c: Continuation): bool =
  ## `true` if the continuation is finished.
  c.state == Finished

template dismissed*(c: Continuation): bool =
  ## `true` if the continuation was dimissed.
  c.state == Dismissed
