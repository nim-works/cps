import std/hashes
import std/locks

type
  Semaphore* = object
    id: int
    lock: Lock
    cond: Cond
    count: int

proc id*(s: Semaphore): int = s.id

proc hash*(s: Semaphore): Hash =
  ## helper for use in containers
  result = s.id.Hash

proc `==`*(a, b: Semaphore): bool =
  ## helper for use in containers
  result = a.id == b.id

proc `<`*(a, b: Semaphore): bool =
  ## helper for use in containers
  result = a.id < b.id

proc init*(s: var Semaphore; id: int) =
  ## initialize a semaphore
  initLock s.lock
  initCond s.cond
  s.count = 0
  s.id = id

proc `=destroy`*(s: var Semaphore) =
  ## destroy a semaphore
  deinitCond s.cond
  deinitLock s.lock
  s.count = 0
  s.id = 0

proc signal*(s: var Semaphore) =
  ## blocking signal of `s`
  assert s.id != 0
  withLock s.lock:
    inc s.count
  signal s.cond

proc wait*(s: var Semaphore) =
  ## blocking wait on `s`
  assert s.id != 0
  withLock s.lock:
    while s.count <= 0:
      wait(s.cond, s.lock)
    dec s.count

proc isReady*(s: var Semaphore): bool =
  ## `true` if `s` is ready
  assert s.id != 0
  withLock s.lock:
    result = s.count > 0

template withReady*(s: var Semaphore; body: untyped): untyped =
  ## run the body with a ready `s`
  assert s.id != 0
  withLock s.lock:
    if s.count > 0:
      try:
        body
      finally:
        dec s.count
