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

proc findColonLit*(n: NimNode; s: string; T: typedesc): T =
  let child =
    n.findChild:
      it.kind == nnkExprColonExpr and it.len == 2 and it[0].strVal == s
  if child.isNil:
    raise ValueError.newException "parse error: " & treeRepr(n)
  else:
    when T is BiggestInt:
      result = child[1].intVal
    elif T is int:
      result = child[1].intVal.int
    elif T is string:
      result = child[1].strVal
    else:
      raise Defect.newException "we can't be friends"

proc makeLineInfo*(n: NimNode): LineInfo =
  ## return a run-time LineInfo into a compile-time LineInfo object
  LineInfo(filename: n.findColonLit("filename", string),
           line: n.findColonLit("line", int),
           column: n.findColonLit("column", int))

template makeLineInfo*(n: LineInfo): NimNode =
  ## turn a compile-time LineInfo object into a runtime LineInfo object
  newLit n

proc sym*(hook: Hook): Name =
  ## produce a symbol|ident for the hook procedure
  when false:
    # this is where we can experiment with .dynamicBindSym
    bindSym($hook, brForceOpen)
  else:
    # rely on a `mixin $hook` in (high) scope
    ident($hook).asName

proc abbreviation(n: NimNode): NimNode =
  ## "abbreviate" a node so it can be passed succinctly
  case n.kind
  of nnkProcDef:
    n.name
  of nnkSym, nnkIdent, nnkDotExpr:
    n
  of nnkCallKinds:
    n[0]
  else:
    n.errorAst "dunno how to abbreviate " & $n.kind

proc nameForNode*(n: NimNode): string =
  ## produce some kind of useful string that names a node
  let abbrev = abbreviation n
  case abbrev.kind
  of nnkSym, nnkIdent:
    $abbrev
  else:
    repr n

when defined(cpsNoTrace):
  template entrace(hook: static[Hook]; c, n, body: NormNode): NormNode =
    let call = NormNode body.nilAsEmpty
    copyLineInfo(call, n)
    call
else:
  template entrace(hook: static[Hook]; c, n, body: NormNode): NormNode =
    let event = bindSym(etype hook)
    let info = makeLineInfo n.lineInfoObj
    let fun = newLit(nameForNode n.NimNode)
    let call = newCall(Trace.sym, event, c.NimNode, abbreviation n.NimNode,
                       "fun".eq fun, "info".eq info, body.NimNode).NormNode
    copyLineInfo(call, n)
    call

proc hook*(hook: static[Hook]; n: NormNode): NormNode =
  ## execute the given hook on the given node
  case hook
  of Boot, Coop, Head:
    # hook(continuation)
    hook.entrace NilNormNode, n:
      newCall(hook.sym, n)
  else:
    # cast to `Call` avoids type mismatch as converters can't figure this out
    Call n.errorAst "the " & $hook & " hook doesn't take one argument"

proc hook*(hook: static[Hook]; a, b: NormNode): NormNode =
  ## execute the given hook with two arguments
  case hook
  of Unwind:
    # hook(continuation, Cont)
    let unwind = hook.sym.NimNode
    Unwind.entrace a, b:
      NormNode:
        quote:
          if not `a`.ex.isNil:
            return `unwind`(`a`, `a`.ex).`b`
  of Pass:
    Pass.entrace a, b:
      newCall(hook.sym, a, b)
  of Tail:
    Tail.entrace a, b:
      newCall(hook.sym, a, b)
  of Alloc:
    Alloc.entrace a, b:
      newCall(hook.sym, a, b)
  of Dealloc:
    Dealloc.entrace a, b:
      newCall(hook.sym, a, b)
  of Stack:
    # hook(source, destination), or
    # dealloc(continuation, env_234234), or
    # alloc(Cont, env_234234), or
    # stack(symbol, continuation)
    when cpsStackFrames:
      Stack.entrace a, b:
        newCall(hook.sym, a, b)
    else:
      b
  of Trace:
    # trace(Pass, continuation, "whileLoop_2323",
    # LineInfo(filename: "...", line: 23, column: 44)): nil
    Trace.entrace a, b:
      NormNode newNilLit()    # FIXME: nnkEmpty more appropriate
  else:
    b.errorAst "the " & $hook & " hook doesn't take two arguments"

proc initFrame*(hook: Hook; fun: string; info: LineInfo): NimNode =
  ## prepare a tracing frame constructor
  result = nnkObjConstr.newTree bindSym"TraceFrame"
  result.add: "hook".colon newCall(bindSym"Hook", hook.ord.newLit)
  result.add: "info".colon info.makeLineInfo
  result.add: "fun".colon fun

proc updateLineInfoForContinuationStackFrame*(c, n: NimNode): NimNode =
  ## `c` holds the continuation symbol, while `n` is a node with info
  when cpsStackFrames:
    newAssignment(c.dot("stack").dot("info"), n.lineInfoObj.makeLineInfo)
  else:
    newEmptyNode()
