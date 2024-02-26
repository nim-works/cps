# examples

A small collection of examples provides good demonstration of multiple patterns
of CPS composition. Each example runs independently, with no other requirements,
yet demonstrates different exploits of `cps`.

| Example | Description |
|     --: | :--         |
|[Channels](/examples/channels.nim)|A channel connects sender and receiver continuations|
|[Goto](/examples/goto.nim)|Implementation of `label` and `goto` statements using CPS|
|[Iterator](/examples/iterator.nim)|A simple demonstration of a CPS-based iterator|
|[Coroutines](/examples/coroutine.nim)|A pair of continuations communicate as coroutines. [Walkthrough](/docs/coroutines.md).|
|[Lazy](/examples/lazy.nim)|Lazy streams are composed by continuations in a functional style|
|[Pipes](/examples/pipes.nim)|Coroutines compose streams which connect arbitrarily|
|[TryCatch](/examples/trycatch.nim)|Exception handling is reimplemented using only CPS|
|[CpsCps](/examples/cpscps.nim)|Continuations can efficiently call other continuations|
|[Work](/examples/work.nim)|Implementation of a simple continuation scheduler|
|[LuaCoroutines](/examples/lua_coroutines.nim)|Coroutines implemented in the style of Lua|
|[ThreadPool](/examples/threadpool.nim)|1,000,000 continuations run across all your CPU cores|
