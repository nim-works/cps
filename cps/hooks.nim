import std/macros except newStmtList, newTree

import cps/[spec, normalizedast]

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

proc introduce*(hook: Hook; n: NormalizedNode) =
  ## introduce a hook into the given scope whatfer later use therein
  var n = n
  case n.kind
  of nnkStmtList:
    n.insert(0, nnkMixinStmt.newTree ident($hook))
  of nnkProcDef:
    introduce hook, asRoutineDef(n).body # TODO: maybe a pragmas at some point?
  else:
    n.insert(0, n.errorAst "you cannot add a " & $hook & " to a " & $n.kind)

proc introduce*(n: NormalizedNode; hooks: set[Hook]) =
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
    bindName($hook, brForceOpen)
  else:
    # rely on a `mixin $hook` in (high) scope
    asName($hook)

proc hook*(hook: Hook; n: NormalizedNode): NormalizedNode =
  ## execute the given hook on the given node
  case hook
  of Alloc: # (unused; see alloc/2)
    # hook(typedesc[Continuation])
    newCall(hook.sym, n)
  of Boot, Coop, Head:
    # hook(continuation)
    newCall(hook.sym, n)
  of Trace:
    # trace("whileLoop_2323", LineInfo(filename: "...", line: 23, column: 44))
    newCall(hook.sym,
            newLit(repr n.name).NormalizedNode,
            makeLineInfo(n.lineInfoObj).NormalizedNode)
  else:
    n.errorAst("the " & $hook & " hook doesn't take one argument")

proc hook*(hook: Hook; n: Name): NormalizedNode =
  ## execute the given hook on the given node
  ## XXX: work out the correct type class for `n`
  hook(hook, n.NormalizedNode)

proc hook*(hook: Hook; a, b: NormalizedNode): NormalizedNode =
  ## execute the given hook with two arguments
  case hook
  of Alloc:
    # hook(Cont, env_234234)
    newCall(hook.sym, a, b)
  of Unwind:
    # hook(continuation, Cont)
    let unwind = hook.sym.NimNode
    NormalizedNode:
      quote:
        if not `a`.ex.isNil:
          return `unwind`(`a`, `a`.ex).`b`
  of Pass, Tail:
    # hook(source, destination)
    newCall(hook.sym, a, b)
  of Trace:
    # trace("whileLoop_2323", LineInfo(filename: "...", line: 23, column: 44))
    newCall(hook.sym, a,
            newLit(repr b.name).NormalizedNode,
            makeLineInfo(b.lineInfoObj).NormalizedNode)
  of Dealloc:
    newStmtList(newCall(hook.sym, a, b), newNilLit().NormalizedNode)
  else:
    b.errorAst("the " & $hook & " hook doesn't take two arguments")

proc hook*(hook: Hook; a: Name; b: NormalizedNode): NormalizedNode =
  ## execute the given hook with two arguments
  ## XXX: work out the correct type class for `a` and `b`
  hook(hook, a.NormalizedNode, b)
