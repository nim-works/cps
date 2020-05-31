import macros
import nativesockets
import strutils
import eventqueue

# Any call to a proc starting with "cps_" is a CPS call
proc isCpsCall(n: NimNode): bool = 
  n.kind == nnkCall and ($n[0]).find("cps_") == 0

# Every block with calls to CPS procs is a CPS block
proc isCpsBlock(n: NimNode): bool =
  if n.kind != nnkProcDef:
    for nc in n:
      if nc.isCpsCall() or isCpsBlock(nc):
        return true



proc split(n: Nimnode): NimNode =

  let name = $n[0]
  var contTypes = newStmtList() # Continuation types for split procs
  var contProcs = newStmtList() # Forward declaratiosn of all split procs
  var id = 0

  # Create a new CPS proc with prelude and the given body
  proc addProc(body: NimNode): NimNode =
    inc id
    let procId = ident("cps_" & name & "_" & $id)
    let contId = ident("cont")
    let contT = ident("Cont_" & name & "_" & $id)

    # hack: the prelude now has one hardcoded val
    let prelude = nnkVarSection.newTree(
      nnkIdentDefs.newTree(
        newIdentNode("j"),
        newEmptyNode(),
        newDotExpr(newDotExpr(contId, contT), ident("j"))
      )
    )

    contTypes.add quote do:
      type `contT` = ref object of Cont
        # hack: continuation types should be derived from lambda lifting
        j: int

    contProcs.add quote do:
      proc `procId$`(`contId`: Cont): Cont

    result = quote do:
      result = `contT`(fn: `procId`, j: j)
      proc `procId`(`contId`: Cont): Cont =
        `prelude`
        `body`


  # Split on 'while' CPS blocks
  proc auxWhile(n: Nimnode): NimNode =
    if n.kind == nnkWhileStmt and n[1].isCpsBlock():
      let (expr, stmt) = (n[0], auxWhile(n[1]))
      let body = quote do:
        if `expr`:
          `stmt`
      result = addProc(body)
      # Hack in call to self at end of body block. This is nasty
      result[1][6][1][0][1][0].add result[0]
    else:
      result = copyNimNode(n)
      for nc in n:
        result.add auxWhile(nc)
          

  # Split on CPS calls
  proc auxSplit(n: Nimnode): NimNode =

    if n.kind == nnkStmtList and n.isCpsBlock():
      
      type State = enum sPre, sCPSCalls, sPost
      var state = sPre
      var a0 = newStmtList()
      var a1 = newStmtList()
      
      for nc in n:
        case state
        of sPre:
          a0.add auxSplit(nc)
          if nc.isCpsCall():
            state = sCPSCalls
        of sCPSCalls:
          if nc.isCpsCall():
            a0.add auxSplit(nc)
          else:
            state = sPost
            a1.add auxSplit(nc)
        of sPost:
            a1.add auxSplit(nc)
      
      result = copyNimNode(n)
      result.add a0
      if a1.len > 0:
        result.add addProc auxSplit(a1)

    else:
      result = copyNimNode(n)
      for nc in n:
        result.add auxSplit(nc)


  # Move all procs to top level
  proc auxToplevel(n: NimNode): NimNode =
    var procs = newStmtList()
    proc aux(n: NimNode): NimNode =
      result = copyNimNode(n)
      for nc in n:
        if nc.kind == nnkProcDef:
          procs.add aux(nc)
        else:
          result.add aux(nc)
    procs.add aux(n)
    return procs

  # hack: chain csp_sleep() calls
  var chainNode: NimNode
  proc auxChain(n: NimNode): NimNode =
    result = copyNimNode(n)
    for nc in n:
      if nc.kind == nnkCall and ($nc[0]).find("cps_") == 0:
        chainNode = nc
      else:
        if chainNode != nil:
          let rv = nc[0][1]
          result.add quote do:
            result = cps_sleep(`rv`, 0.3)
          chainNode = nil
        else:
          result.add auxChain(nc)

  var body = n.auxWhile.auxSplit.auxToplevel.auxChain

  result = newStmtList(contTypes, contProcs, body)


proc xfrmCps(n: NimNode): NimNode =
  result = split(n)
  echo "====================="
  echo n.repr
  echo "====================="
  echo result.repr
  echo "====================="


macro cps(n: untyped) =
  xfrmCps(n)


proc cps_sleep(cont: Cont, t: float): Cont =
  discard addTimer(t, cont)
  return Cont()

# This is the proc we are converting to CPS form

proc tocker(cont: Cont): Cont {.cps.} =
  var j = cont.Cont_tocker_1.j
  echo "start"
  while true:
    cps_sleep(0.3)
    echo "tick ", j
    inc j
    cps_sleep(0.3)
    echo "tock ", j
    inc j

# Instantiate two parallel tockers, starting at different numbers

Cont_Tocker_1(fn: tocker, j:   0).run()
Cont_Tocker_1(fn: tocker, j: 100).run()

# Forever run the event queue

run()
