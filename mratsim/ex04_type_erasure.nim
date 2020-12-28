# Example 4: testing type erasure scheme for compat with schedulers.
#
# A Nim "object of RootObj"
# is defined in Nim arc with this header:
# https://github.com/nim-lang/Nim/blob/6d442a4/lib/system.nim#L1752-L1760
#
# TNimTypeV2 {.compilerproc.} = object
#   destructor: pointer
#   size: int
#   align: int
#   name: cstring
#   traceImpl: pointer
#   disposeImpl: pointer
#   typeInfoV1: pointer # for backwards compat, usually nil
# PNimTypeV2 = ptr TNimTypeV2
#
# The question becomes:
# - For a stack continuation used as a generator for example,
#   - do we have this 56 bytes stuff on the stack?
#   - is it on the heap and does it require allocation?
# - When the continuation is stored on the heap
#   - is there a single alloc
#   - is there one for the ref and one for the header
#
# Continuations and coroutines can be the primitives for
# composable tasks, streams, lazy first-class itertors/generators
# but:
# - They need type erasure for ergonomics
# - They need to have zero memory cost for several use cases
#   and allow custom memory management
#   - high performance computing
#   - embedded
#   - kernel and kernel drivers
#   - cryptography
#   - games
#
# Aka: when VTables concept so that we pay for dynamic dispatch
# only when we require it.

# In any case this highlights several problems:
# 1. `=copy` isn't inherited
#    Non-blocking, we can create it for each type, and the default may be unclear.
# 2. `when compiles(` doesn't seem to run the borrow checker?
# 3. Either the borrow checker is wrong or the compiler introduced
#    a copy when it shouldn't, similar to https://github.com/nim-lang/Nim/issues/14118

# Looking at the codegen
#
# It seems like the of RootObj header is a static and
# creating a object of RootObj on the stack merely requires
# a pointer to the approriate header
# (see at the bottom of the C file, section: NIM_merge_TYPE_INIT3)
#
# hence we do get no alloc on the stack.
# The overhead is:
# - 8 bytes for a pointer header
# - 1 instruction for setting that pointer
#   to an descriptor initialized once in Nim "preMain"
#
# On the stack, generics and inheritance generate the same code
# modulo header field nested an extra time for inheritance.
# However

type TestKind = enum
  Inheritance
  Generics
type SchedulerKind = enum
  UseManagedRef
  UseManualPointer

const
  tKind = Inheritance
  # tKind = Generics
  # schedKind = UseManagedRef
  schedKind = UseManualPointer

when tKind == Inheritance:
  type
    Continuation = object of RootObj
      fn: proc(c: var Continuation) {.nimcall.}

    # For non-ref objects on the C backend
    # ----------------------------------------
    ContinuationStack = object of Continuation
      frame: ContinuationStackFrame

    ContinuationStackFrame {.union.} = object
      frame1: CSFrame1
      frame2: CSFrame2

    CSFrame1 = object
      i: int
      j: int

    CSFrame2 = object
      a: float64
      b: float64
      c: float64

    # For ref objects or the JS backend
    # ----------------------------------------

    # we can't have the same frame.frame1 without extra "object of " indirection

    ContinuationHeap1 = object of Continuation
      frame1: CHFrame1

    ContinuationHeap2 = object of Continuation
      frame2: CHFrame2

    CHFrame1 = object
      i: int
      j: int

    CHFrame2 = object
      a: float64
      b: float64
      c: float64

  proc `=copy`(dst: var Continuation, src: Continuation) {.error: "Continuations can only be moved".}

  # when compiles(block: # Note: this lead to bad C codegen :/
  #   # error: extra brace group at end of initializer
  #   # 148 | N_LIB_PRIVATE NIM_CONST tyObject_ContinuationStack__YYr24FYqrPpgr4rY9c71hfA a__8tJ5AkqCWU6zdy9a3eQkosg = {{{(&NTIv2__YYr24FYqrPpgr4rY9c71hfA_)}, NIM_NIL}, {{0, 0}, {0.0, 0.0, 0.0}}}
  #   #     |                                                                                                                                                                     ^
  #   let a = ContinuationStack()
  #   let b = a
  #   echo a
  #   echo b
  # ):
  #   static: {.error: "move enforcement cannot be applied to inherited objects".}

  var schedulerManaged: seq[ref Continuation]
  var schedulerManual: seq[ptr Continuation]

  let a = ContinuationStack(frame: ContinuationStackFrame(frame1: CSFrame1(i: 1, j: 2)))
  let b = ContinuationStack(frame: ContinuationStackFrame(frame2: CSFrame2(a: -2.0, b: -3.0, c: -6.0)))

  let c = ContinuationHeap1(frame1: CHFrame1(i: 1, j: 2))
  let d = ContinuationHeap2(frame2: CHFrame2(a: -2.0, b: -3.0, c: -6.0))

  proc moveToHeap(cont: sink Continuation): ref Continuation =
    var tmp: ref Continuation
    new tmp
    `=sink`(tmp[], cont)
    # Type-erase
    (ref Continuation)(tmp)

  proc moveToManual(cont: sink Continuation): ptr Continuation =
    let tmp = create(Continuation)
    `=sink`(tmp[], cont)
    # Type-erase
    (ptr Continuation)(tmp)

  # Check in codegen if there is indirection for the 'fn' field
  echo "check indirection when using supertype: ", cast[ByteAddress](a.fn)

  when schedKind == UseManagedRef:
    schedulerManaged.add moveToHeap a # borrow checker bug? complains about a copy (or did the compiler introduce one?)
    schedulerManaged.add moveToHeap b
    schedulerManaged.add moveToHeap c
    schedulerManaged.add moveToHeap d
  else:
    schedulerManual.add moveToManual a # borrow checker bug? complains about a copy (or did the compiler introduce one?)
    schedulerManual.add moveToManual b
    schedulerManual.add moveToManual c
    schedulerManual.add moveToManual d

else: # ---------------------------------------------------------------------------------------
  type
    Continuation[Frame] = object of RootObj
      fn: proc(c: var Continuation[Frame]) {.nimcall.}
      frame: Frame

    # For non-ref objects on the C backend
    # ----------------------------------------
    ContinuationStackFrame {.union.} = object
      frame1: CSFrame1
      frame2: CSFrame2

    CSFrame1 = object
      i: int
      j: int

    CSFrame2 = object
      a: float64
      b: float64
      c: float64

    # For ref objects or the JS backend
    # ----------------------------------------
    ContinuationHeap1 = object
      frame1: CHFrame1

    CHFrame1 = object
      i: int
      j: int

    ContinuationHeap2 = object
      frame2: CHFrame2

    CHFrame2 = object
      a: float64
      b: float64
      c: float64

  # proc `=copy`[Frame](dst: var Continuation[Frame], src: Continuation[Frame]) {.error: "Continuations can only be moved".}

  # This actually doesn't compile
  # but "when compiles" detects it as compiling
  # when compiles(block:
  #   let a = Continuation[ContinuationStackFrame]()
  #   let b = a
  #   echo a
  #   echo b
  # ):
  #   static: {.error: "move enforcement cannot be applied to generic objects".}

  var schedulerManaged: seq[ref Continuation[void]]
  var schedulerManual: seq[ptr Continuation[void]]

  let a = Continuation[ContinuationStackFrame](frame: ContinuationStackFrame(frame1: CSFrame1(i: 1, j: 2)))
  let b = Continuation[ContinuationStackFrame](frame: ContinuationStackFrame(frame2: CSFrame2(a: -2.0, b: -3.0, c: -6.0)))

  let c = Continuation[ContinuationHeap1](frame: ContinuationHeap1(frame1: CHFrame1(i: 1, j: 2)))
  let d = Continuation[ContinuationHeap2](frame: ContinuationHeap2(frame2: CHFrame2(a: -2.0, b: -3.0, c: -6.0)))

  proc moveToHeap[Frame](cont: sink Continuation[Frame]): ref Continuation[void] =
    var tmp: ref Continuation[Frame]
    new tmp
    `=sink`(tmp[], cont)
    # Type-erase
    (ref Continuation[void])((ref RootObj)((tmp)))

  proc moveToManual[Frame](cont: sink Continuation[Frame]): ptr Continuation[void] =
    let tmp = create(Continuation[Frame])
    `=sink`(tmp[], cont)
    # Type-erase
    cast[ptr Continuation[void]](tmp)

  # Check in codegen if there is indirection for the 'fn' field
  echo "check indirection when using supertype: ", cast[ByteAddress](a.fn)

  when schedKind == UseManagedRef:
    schedulerManaged.add moveToHeap a # borrow checker bug? complains about a copy (or did the compiler introduce one?)
    schedulerManaged.add moveToHeap b
    schedulerManaged.add moveToHeap c
    schedulerManaged.add moveToHeap d
  else:
    schedulerManual.add moveToManual a # borrow checker bug? complains about a copy (or did the compiler introduce one?)
    schedulerManual.add moveToManual b
    schedulerManual.add moveToManual c
    schedulerManual.add moveToManual d
