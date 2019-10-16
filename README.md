# hs-concurrency-demos
[![Build Status](https://travis-ci.org/easoncxz/hs-concurrency-demos.svg?branch=master)](https://travis-ci.org/easoncxz/hs-concurrency-demos)
-----

From the tests you can see code examples. To save you from firing up Haskell
tools, you can look at test runs on Travis CI by clicking the badge. Currently
included deadlock and race condition situations. I'm not familiar enough with
Haskell asynchronous exceptions to figure out how to nicely catch
`BlockedIndefinitelyOnMVar` thrown on suspected deadlock, so tests will show as
"failing" on Travis.
