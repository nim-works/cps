import std/macros
import cps/ast

const
  cpsDebug* {.strdefine.} = "" ## produce gratuitous output
  comments* = cpsDebug != ""   ## embed comments within the transformation

func doc*(s: string): Statement =
   ## generate a doc statement for debugging
   when comments:
     Statement(newCommentStmtNode(s))
   else:
     Statement(newEmptyNode())

proc doc*(n: NimNode; s: string) =
  ## add a doc statement to the ast for debugging
  when comments:
    if n.kind == nnkStmtList:
      n.add doc(s)

when cpsDebug == "":
  template debug*(ignore: varargs[untyped]) = discard
  template lineAndFile*(n: NimNode): string = "(no debug)"
else:
  import std/strutils
  import os

  type
    AstKind* {.pure.} = enum
      ## The type of the passed AST
      Original = "original"
      Transformed = "transformed"

  template lineAndFile*(n: NimNode): string =
    $n.lineInfoObj.line & " of " & extractFilename($n.lineInfoObj.filename)

  proc numberedLines*(s: string; first = 1): string =
    for n, line in pairs(splitLines(s, keepEol = true)):
      result.add "$1  $2" % [ align($(n + first), 3), line ]

  proc snippet*(n: NimNode; name: string): string =
    result &= "----8<---- " & name & "\t" & "vvv"
    result &= "\n" & n.repr.numberedLines(n.lineInfoObj.line) & "\n"
    result &= "----8<---- " & name & "\t" & "^^^"

  func debug*(id: string, n: NimNode, kind: AstKind, info: NimNode = nil) =
    ## Debug print the given node `n`, with `id` is a string identifying the
    ## caller and `info` specifies the node to retrieve the line information
    ## from.
    ##
    ## If `info` is `nil`, the line information will be retrieved from `n`.
    if cpsDebug != id: return
    let info =
      if info.isNil:
        n
      else:
        info

    let lineInfo = info.lineInfoObj

    let procName =
      if info.kind in RoutineNodes:
        repr info.name
      else:
        ""

    debugEcho "=== $1 $2($3) === $4" % [
      id,
      if procName.len > 0: "on " & procName else: "",
      $kind,
      $lineInfo
    ]
    when defined(cpsTree):
      debugEcho treeRepr(n)
    else:
      debugEcho repr(n).numberedLines(lineInfo.line)
