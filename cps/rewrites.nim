##[
  CPS-specific AST rewriting machinery.
  
  Generic AST utilities (filter, desym, etc.) are now in ast.nim.
  This module contains the CPS normalization and workaround passes.
]##

import std/macros
import cps/ast

# Re-export for backwards compatibility
export NormalCallNodes

# Converter scoped to this module for normalizingRewrites
converter normNodeToNimNode(n: NormNode): NimNode =
  ## scope a converter to this module so it doesn't leak but we keep our sanity
  ## in `filter`, `normalizingRewrites`, etc  below.
  n.NimNode

proc childCallToRecoverResult*(n: NimNode; sym: NimNode; field: NimNode): NormNode =
  ## Rewrite continuation calls into their results
  if sym.kind notin NormalCallNodes:
    raise Defect.newException: "resymCall is for calls, not " & $sym.kind
  proc resymify(n: NimNode): NimNode =
    case n.kind
    of NormalCallNodes:
      if n == sym:
        result = field
    else:
      discard
  filter(n, resymify)

proc childCallToRecoverResult*(n, sym, field: NormNode): NormNode =
  childCallToRecoverResult(n.NimNode, sym.NimNode, field.NimNode)

proc isCallback*(n: NimNode): bool =
  ## true if the node is essentially a callback call
  case n.kind
  of nnkEmpty:
    false
  of nnkDotExpr:
    n.last.isCallback
  of nnkSym:
    if symKind(n) in {nskField}:
      n.getTypeImpl.isCallback
    else:
      false
  of nnkObjectTy:
    n.last.isCallback
  of nnkRecList:
    n.len == 2 and
    n[0].kind == nnkIdentDefs and n[0][0].strVal == "fn" and n[0].isCallback and
    n[1].kind == nnkIdentDefs and n[1][0].strVal == "rs"
  of nnkIdentDefs:
    if n[0].kind in {nnkSym, nnkIdent} and n[0].strVal == "fn":
      n[1].isCallback
    else:
      false
  of nnkProcTy:
    for node in n.pragma:
      if node.kind == nnkCall:
        if node[0].strVal == "cpsCallback":
          return true
    false
  else:
    false

proc normalizingRewrites*(n: NimNode): NormNode =
  ## Rewrite AST into a safe form for manipulation without removing semantic data.
  proc rewriter(n: NimNode): NimNode =
    proc rewriteIdentDefs(n: NimNode): NimNode =
      if n.kind == nnkIdentDefs:
        if n.len == 2:
          n.add newEmptyNode()
        elif n[1].isEmpty:
          n[1] = getTypeInst n[2]
          if n[1].kind == nnkProcTy:
            n[1] = newCall(bindSym"typeOf", n[2])
          n[2] = normalizingRewrites n[2]
        result = n

    proc rewriteVarLet(n: NimNode): NimNode =
      if n.kind in {nnkLetSection, nnkVarSection}:
        result = newStmtList()
        for child in n.items:
          case child.kind
          of nnkVarTuple:
            result.add:
              newNimNode(n.kind, n).add:
                child
          of nnkIdentDefs:
            let defs = rewriteIdentDefs(child)
            for d in defs[0 .. ^3].items:
              result.add:
                newNimNode(n.kind, n).add:
                  newIdentDefs(d, copyNimTree(defs[^2])):
                    normalizingRewrites(defs[^1])
          else:
            result.add:
              NimNode errorAst(child, "unexpected")

    proc rewriteReturn(n: NimNode): NimNode =
      case n.kind
      of nnkReturnStmt:
        if n[0].kind == nnkAsgn:
          if repr(n[0][0]) != "result":
            result = errorAst(n, "unexpected return assignment form").NimNode
          else:
            result = copyNimNode(n)
            result.add normalizingRewrites(n[0][1]).NimNode
        else:
          result = n
      else: discard

    proc rewriteFormalParams(n: NimNode): NimNode =
      case n.kind
      of nnkFormalParams:
        result = nnkFormalParams.newNimNode(n)
        result.add normalizingRewrites(n[0]).NimNode
        for arg in n[1 .. ^1].items:
          case arg.kind
          of nnkIdentDefs:
            let defs = rewriteIdentDefs arg
            for d in defs[0 .. ^3].items:
              result.add:
                newIdentDefs(d, copyNimTree(defs[^2]), copyNimTree(defs[^1]))
          else:
            result.add normalizingRewrites(arg).NimNode
      else:
        discard

    proc rewriteExceptBranch(n: NimNode): NimNode =
      case n.kind
      of nnkExceptBranch:
        if n.len == 2:
          if n[0].kind == nnkInfix:
            let
              typ = n[0][1]
              refTyp = nnkRefTy.newTree(typ)
              ex = n[0][2]
              body = n[1]
            result = copyNimNode(n)
            result.add typ
            result.add:
              newStmtList:
                nnkLetSection.newTree:
                  newIdentDefs ex, refTyp:
                    newCall refTyp:
                      newCall bindSym"getCurrentException"
            result.last.add normalizingRewrites(body).NimNode
      else: discard

    proc rewriteVarargsTypedCalls(n: NimNode): NimNode =
      if n.kind in {nnkCall, nnkCommand}:
        proc isMagicalVarargs(n: NimNode): bool =
          if n.typeKind == ntyVarargs:
            let typ = getTypeInst n
            if typ[1].typeKind in {ntyStmt, ntyExpr}:
              true
            else:
              false
          else:
            false

        if n.len > 1 and n.findChild(it.isMagicalVarargs) != nil:
          if n.len > 2:
            result = errorAst(n, "unsupported call with magical varargs that have more than two parameters").NimNode
          else:
            result = copyNimNode n
            result.add normalizingRewrites(n[0]).NimNode
            let unwrapped = n[1].last
            for i in unwrapped.items:
              result.add i

    proc rewriteCheckedFieldExpr(n: NimNode): NimNode =
      case n.kind
      of nnkCheckedFieldExpr:
        result = normalizingRewrites(n[0]).NimNode
      else:
        discard

    case n.kind
    of nnkIdentDefs:
      rewriteIdentDefs n
    of nnkLetSection, nnkVarSection:
      rewriteVarLet n
    of nnkReturnStmt:
      rewriteReturn n
    of nnkFormalParams:
      rewriteFormalParams n
    of nnkExceptBranch:
      rewriteExceptBranch n
    of CallNodes:
      rewriteVarargsTypedCalls n
    of nnkCheckedFieldExpr:
      rewriteCheckedFieldExpr n
    else:
      nil

  filter(n, rewriter)

proc workaroundRewrites(n: NimNode): NimNode =
  ## Rewrite AST after modification to ensure sem doesn't skip nodes.
  proc rewriteContainer(n: NimNode): NimNode =
    result = n
    if n.kind notin AtomicNodes:
      result = newNimNode(n.kind, n)
      for child in n:
        result.add child

  proc workaroundSigmatchSkip(n: NimNode): NimNode =
    if n.kind in NormalCallNodes:
      result = newNimNode(n.kind, n)
      for child in n.items:
        result.add:
          rewriteContainer:
            workaroundRewrites child

  proc workaroundCompilerBugs(n: NimNode): NimNode =
    proc rewriteHiddenAddrDeref(n: NimNode): NimNode =
      case n.kind
      of nnkHiddenAddr, nnkHiddenDeref:
        workaroundRewrites(n[0])
      else:
        nil

    proc rewriteConv(n: NimNode): NimNode =
      case n.kind
      of nnkConv:
        result = newNimNode(nnkCall, n)
        for child in n:
          result.add workaroundRewrites(child)
      else: discard

    proc rewriteHiddenConv(n: NimNode): NimNode =
      case n.kind
      of nnkHiddenStdConv, nnkHiddenSubConv:
        if n.len == 2 and n[0].kind == nnkEmpty:
          result = workaroundRewrites n[1]
        else:
          raise newException(Defect,
            "unexpected conversion form:\n" & treeRepr(n))
      of nnkHiddenCallConv:
        result = nnkCall.newNimNode(n)
        for child in n.items:
          result.add workaroundRewrites child
      else:
        discard

    case n.kind
    of nnkHiddenAddr, nnkHiddenDeref:
      rewriteHiddenAddrDeref(n)
    of nnkConv:
      rewriteConv(n)
    of nnkHiddenStdConv, nnkHiddenSubConv, nnkHiddenCallConv:
      rewriteHiddenConv(n)
    else:
      nil

  result = filter(n, workaroundCompilerBugs).NimNode
  result = filter(result, workaroundSigmatchSkip).NimNode

proc workaroundRewrites*(n: NormNode): NormNode =
  workaroundRewrites(n.NimNode).NormNode

proc addInitializationToDefault*(n: NimNode): NormNode =
  ## Turn `var x: Foo` into `var x: Foo = default Foo`
  result = NormNode(n)
  if n.kind == nnkIdentDefs:
    if n[2].isEmpty:
      n[2] =
        newCall bindSym"default":
          newCall bindSym"typeOf":
            n[1]

proc addInitializationToDefault*(n: NormNode): NormNode =
  addInitializationToDefault(n.NimNode)
