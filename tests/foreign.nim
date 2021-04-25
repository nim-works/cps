import macros
import json

## Helper module for foreign symbol tests

template jsonifyBind*(n: untyped): string =
  ## Turns the given expression into json.
  bind `%`
  bind `$`
  `$` `%` n

macro jsonifyImplicit*(n: untyped): string =
  ## Turns the given expression into json.
  result = newTree(
    nnkCall,
    bindSym"$",
    newTree(
      nnkCall,
      bindSym"%",
      n
    )
  )
