import std/macros

import cps/spec

{.experimental: "dynamicBindSym".}

##[

The idea is that you can reimplement one of a few procedures which we will
perform a late binding to by name.

]##

type
  Hook* = enum ##
    ## these are hook procedure names; the string value matches the name
    ## of the symbol we'll call to perform the hook.
    Coop    = "coop"
    Trace   = "trace"
    Alloc   = "alloc"
    Dealloc = "dealloc"
    Pass    = "pass"
    Boot    = "boot"
    Unwind  = "unwind"
    Head    = "head"
    Tail    = "tail"

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

proc sym*(hook: Hook): NimNode =
  ## produce a symbol|ident for the hook procedure
  when false:
    # this is where we can experiment with .dynamicBindSym
    bindSym($hook, brForceOpen)
  else:
    # rely on a `mixin $hook` in (high) scope
    ident($hook)

proc hook*(hook: Hook; n: NimNode): NimNode =
  ## execute the given hook on the given node
  case hook
  of Alloc:
    # hook(typedesc[Continuation])
    newCall(hook.sym, n)
    # hook[env_234234]()
    #newCall(nnkBracketExpr.newTree [hook.sym, n])
  of Boot, Coop, Head:
    # hook(continuation)
    newCall(hook.sym, n)
  of Trace:
    # trace("whileLoop_2323", LineInfo(filename: "...", line: 23, column: 44))
    newCall(hook.sym, newLit(repr n.name), makeLineInfo n.lineInfoObj)
  else:
    n.errorAst "the " & $hook & " hook doesn't take one argument"

proc hook*(hook: Hook; a: NimNode; b: NimNode): NimNode =
  ## execute the given hook with two arguments
  case hook
  of Alloc:
    # hook[Cont](typedesc[env_234234])
    #newCall(nnkBracketExpr.newTree [hook.sym, a], b)
    # Cont(hook(typedesc[env_234234]))
    #newCall(a, newCall(hook.sym, b))
    # hook(Cont, env_234234)
    newCall(hook.sym, a, b)
  of Unwind:
    # hook(continuation, exception)
    newCall(hook.sym, a, b)
  of Pass, Tail:
    # hook(source, destination)
    newCall(hook.sym, a, b)
  of Trace:
    # trace("whileLoop_2323", LineInfo(filename: "...", line: 23, column: 44))
    newCall(hook.sym, a, newLit(repr b.name), makeLineInfo b.lineInfoObj)
  of Dealloc:
    newStmtList [newCall(hook.sym, a, b), newNilLit()]
  else:
    b.errorAst "the " & $hook & " hook doesn't take two arguments"
