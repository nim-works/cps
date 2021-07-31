import std/macros except newStmtList, newTree

import cps/[spec, normalizedast]

{.experimental: "dynamicBindSym".}

##[

The idea is that you can reimplement one of a few procedures which we will
perform a late binding to by name.

]##

proc introduce*(hook: Hook; n: NormNode) =
  ## introduce a hook into the given scope whatfer later use therein
  var n = n
  case n.kind
  of nnkStmtList:
    n.insert(0, nnkMixinStmt.newTree ident($hook))
  of nnkProcDef:
    introduce hook, asRoutineDef(n).body # TODO: maybe a pragmas at some point?
  else:
    n.insert(0, n.errorAst "you cannot add a " & $hook & " to a " & $n.kind)

proc introduce*(n: NormNode; hooks: set[Hook]) =
  ## convenience to introduce a set of hooks
  for hook in hooks.items:
    hook.introduce n

proc makeLineInfo(n: LineInfo): NimNode =
  ## turn a compile-time LineInfo object into a runtime LineInfo object
  result = nnkObjConstr.newTree bindSym"LineInfo"
  result.add newColonExpr(ident"filename", n.filename.newLit)
  result.add newColonExpr(ident"line", n.line.newLit)
  result.add newColonExpr(ident"column", n.column.newLit)

proc sym*(hook: Hook): Name =
  ## produce a symbol|ident for the hook procedure
  when false:
    # this is where we can experiment with .dynamicBindSym
    bindSym($hook, brForceOpen)
  else:
    # rely on a `mixin $hook` in (high) scope
    asName($hook)

macro etype(e: enum): string =
  ## Coop -> "Coop", not "coop"
  for sym in (getTypeImpl e)[1..^1]:
    if sym.intVal == e.intVal:
      return newLit sym.strVal
  error "unexpected"

proc pickLit(n: NormNode): NimNode =
  case n.kind
  of nnkProcDef:
    newLit $n.name
  of nnkSym, nnkIdent:
    newLit $n
  of nnkCall:
    newLit $n[0]
  else:
    newLit repr(n)

template entrace(hook: static[Hook]; c, n, body: NormNode): NormNode =
  let event = bindSym(etype hook)
  let info = makeLineInfo n.lineInfoObj
  let fun = pickLit n
  newCall(Trace.sym, event, c, "fun".eq fun, "info".eq info, body).NormNode

template entrace(hook: static[Hook]; n, body: NormNode): NormNode =
  entrace(hook, nil.NormNode, n, body)

proc hook*(hook: static[Hook]; n: NormNode): NormNode =
  ## execute the given hook on the given node
  case hook
  of Boot:
    # hook(continuation)
    Boot.entrace n:
      newCall(hook.sym, n)
  of Coop:
    # hook(continuation)
    Coop.entrace n:
      newCall(hook.sym, n)
  of Head:
    # hook(continuation)
    Head.entrace n:
      newCall(hook.sym, n)
  else:
    # cast to `Call` avoids type mismatch as converters can't figure this out
    Call n.errorAst "the " & $hook & " hook doesn't take one argument"

proc hook*(hook: static[Hook]; a, b: NormNode): NormNode =
  ## execute the given hook with two arguments
  case hook
  of Alloc:
    # hook(Cont, env_234234)
    Alloc.entrace a, b:
      newCall(hook.sym, a, b)
  of Unwind:
    # hook(continuation, Cont)
    let unwind = hook.sym.NimNode
    Unwind.entrace a, b:
      NormNode:
        quote:
          if not `a`.ex.isNil:
            return `unwind`(`a`, `a`.ex).`b`
  of Pass:
    # hook(source, destination)
    Pass.entrace a, b:
      newCall(hook.sym, a, b)
  of Tail:
    # hook(source, destination)
    Tail.entrace a, b:
      newCall(hook.sym, a, b)
  of Trace:
    # trace(Pass, continuation, "whileLoop_2323",
    # LineInfo(filename: "...", line: 23, column: 44)): discard
    Trace.entrace a, b:
      NormNode newNilLit()
  of Dealloc:
    # dealloc(env_234234, continuation)
    Dealloc.entrace a, b:
      newCall(hook.sym, a, b)
  else:
    b.errorAst "the " & $hook & " hook doesn't take two arguments"
