# CPS-enabled non-blocking standard library

A large part of the standard library can be rewritten
with a standard composable and suspendable interface.

Coroutine-based:
- [ ] closure iterators
- [ ] sequtils
- [ ] algorithm
- [ ] strutils
- [ ] streams

Raw continuation-based:
- [ ] async/await
- [ ] asyncstream
- [ ] non-blocking IO
- [ ] threadpool
- [ ] channels

Sugar:
- [ ] lambda for anonymous coroutine creation. (CSP primitive)
- [ ] `collect` support for (chained) coroutines
- [ ] CSP (= named channel + anonymous coroutine)
- [ ] Actors (= named coroutine + anonymous channel)
