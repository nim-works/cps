import macros
import nativesockets
import strutils
import eventqueue

# Any call to a proc starting with "cps_" is a CPS call
proc isCpsCall(n: NimNode): bool = 
  return n.kind == nnkCall and ($n[0]).find("cps_") == 0

# Every block with calls to CPS procs is a CPS block
proc isCpsBlock(n: NimNode): bool =
  if n.kind != nnkProcDef:
    for nc in n:
      if nc.isCpsCall() or isCpsBlock(nc):
        return true



proc split(n: Nimnode): NimNode =

  let name = $n[0]
  var contTypes = newStmtList()
  var contProcs = newStmtList()
  var id = 0

  # Create a new CPS proc with the given body
  proc addProc(body: NimNode): NimNode =
    inc id
    let procId = ident("cps_" & name & "_" & $id)
    let contId = ident("cont")
    let contT = ident("Cont_" & name & "_" & $id)

    let prelude = nnkVarSection.newTree(
      nnkIdentDefs.newTree(
        newIdentNode("j"),
        newEmptyNode(),
        newDotExpr(newDotExpr(contId, contT), ident("j"))
      )
    )

    contTypes.add quote do:
      type `contT` = ref object of Cont
        j: int

    contProcs.add quote do:
      proc `procId$`(`contId`: Cont): Cont

    result = quote do:
      return `contT`(fn: `procId`, j: j)
      proc `procId`(`contId`: Cont): Cont =
        `prelude`
        `body`


  # Split on 'while'
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
      
      result = copyNimNode(n)

      type State = enum sPre, sCPSCalls, sPost
      var state = sPre
      var postblock = newStmtList()
      
      for nc in n:
        case state
        of sPre:
          result.add auxSplit(nc)
          if nc.isCpsCall():
            state = sCPSCalls
        of sCPSCalls:
          if nc.isCpsCall():
            result.add auxSplit(nc)
          else:
            state = sPost
            postblock.add auxSplit(nc)
        of sPost:
            postblock.add auxSplit(nc)
            
      if postblock.len > 0:
        result.add addProc auxSplit(postblock)

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


  var body = n.auxWhile.auxSplit.auxToplevel

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


proc cps_sleep() = discard # stub

proc tocker(cont: Cont): Cont {.cps.} =
  var j = cont.Cont_tocker_1.j
  echo "start"
  while true:
    cps_sleep()
    echo "tock ", j
    inc j

Cont_Tocker_1(fn: tocker, j: 0).run()
run()
