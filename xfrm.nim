import macros
import nativesockets
import strutils

# Any call to a proc starting with cps_ is a CPS call
proc isCpsCall(n: NimNode): bool = 
  return n.kind == nnkCall and ($n[0]).find("cps_") == 0

# Every block with calls to CPS procs is a CPS block
proc isCpsBlock(n: NimNode): bool =
  if n.kind != nnkProcDef:
    for nc in n:
      if nc.isCpsCall() or isCpsBlock(nc):
        result = true

# Every proc calling a CPS proc is a CPS proc
proc isCpsProc(n: NimNode): bool =
  if n.kind == nnkProcDef:
    return isCpsBlock n[6]


type Label = object
  id: int

var idSeq {.compileTime.} = 0

proc newId(): int =
  inc idSeq
  return idSeq

proc label(id: int, prefix: string): NimNode =
  ident(prefix & "_" & $id)


proc split(n: Nimnode): NimNode =

  var contTypes: seq[NimNode]
  var contProcs: seq[NimNode]

  #var id: 0

  #proc mkProc(): NimNode =
  #  inc id
  #  procId = ident("cps_" & $id)
  #  contId = ident("Cont_" & $id)

  #  contTypes.add quote do:
  #    type `contType` = object of Cont
  #      j: int

  #  contProcs.add quote do:
  #    proc `procId`(cont: `contTtype`): Cont

  #  procId

  proc aux(n: Nimnode): NimNode =

    if n.kind == nnkWhileStmt and n[1].isCpsBlock():

      let id = newId()
      let procId = id.label("cps")
      let contT = id.label("Cont")

      let (exp, body) = (n[0], aux(n[1]))

      contProcs.add quote do:
        proc `procId`(cont: `contT`): Cont

      result = quote do:
        `procId`()
        proc `procId`(cont: `contT`): Cont =
          if `exp`:
            `body`
            `procId`()

    elif n.kind == nnkStmtList and n.isCpsBlock():
      
      result = copyNimNode(n)

      type State = enum sPre, sCPSCalls, sPost
      var state = sPre
      var postblock = newStmtList()
      
      for nc in n:
        case state
        of sPre:
          result.add aux(nc)
          if nc.isCpsCall():
            state = sCPSCalls
        of sCPSCalls:
          if nc.isCpsCall():
            result.add aux(nc)
          else:
            state = sPost
            postblock.add aux(nc)
        of sPost:
            postblock.add aux(nc)
            

      if postblock.len > 0:
        let id = newId()
        let procId = id.label("cps")
        let contT = id.label("Cont")

        contProcs.add quote do:
          proc `procId`(cont: `contT`): Cont

        result.add quote do:
          `procId`()
          proc `procId`(cont: `contT`): Cont =
            `postblock`

    else:
      result = copyNimNode(n)
      for nc in n:
        result.add aux(nc)

  let body = aux(n)

  result = newStmtList()
  for p in contProcs:
    result.add(p)
  result.add body


proc xfrmCps(n: NimNode): NimNode =
  result = split(n)
  echo result.repr


macro cps(n: typed) =
  xfrmCps(n)


proc cps_sleep() = discard # stub

cps:
  proc tocker() =
    var j: int
    while true:
      cps_sleep()
      echo "tock ", j
      inc j

  proc ticker() =
    var i: int
    while true:
      echo "tick"
      inc i
