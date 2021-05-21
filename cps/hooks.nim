import std/macros

import cps/spec

type
  Hook* = enum
    Coop = "coop"
    Trace = "trace"

proc introduce*(hook: Hook; n: NimNode) =
  ## introduce a hook into the given scope whatfer later use therein
  var n = n
  case n.kind
  of nnkStmtList:
    n.insert(0, nnkMixinStmt.newTree ident($hook))
  of nnkProcDef:
    introduce hook, n.body       # TODO: maybe some pragmas at some point?
  else:
    n.insert(0, n.errorAst "you cannot add a " & $hook & " to a " & $n.kind)

proc introduce*(n: NimNode; hooks: set[Hook]) =
  ## convenience to introduce a set of hooks
  for hook in hooks.items:
    hook.introduce n

proc makeLineInfo(n: LineInfo): NimNode =
  ## turn a compile-time LineInfo object into a runtime LineInfo object
  result = nnkObjConstr.newTree bindSym"LineInfo"
  result.add newColonExpr(ident"filename", n.filename.newLit)
  result.add newColonExpr(ident"line", n.line.newLit)
  result.add newColonExpr(ident"column", n.column.newLit)

proc hook*(hook: Hook; n: NimNode): NimNode =
  ## execute the given hook on the given node
  case hook
  of Coop:
    # coop(continuation)
    newCall(ident $hook, n)
  of Trace:
    # trace("whileLoop_2323", LineInfo(filename: "...", line: 23, column: 44))
    newCall(ident $hook, newLit(repr n.name), makeLineInfo n.lineInfoObj)
