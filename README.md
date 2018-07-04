# motility

**This is a half-finished idea. It barely works.**

Experiments in evolving bok players.

## Usage

``` shell
lein run org.nfrac.motility.tcppn 5555 5556
```

### REPL

``` shell
clj -R:dev
```


``` clojure
(require '[org.nfrac.motility.tcppn :as tcppn])
;; so we can peek at the state later
(def bouts (atom {}))

;; start the player servers on given TCP ports
(def th (Thread. #(tcppn/main 5555 5556 bouts 5)))
(.start th)

(require '[org.nfrac.bok.visual-runner :as vrunner])

(def addrs ["tcp://localhost:5555" "tcp://localhost:5556"])
(def opts {:repeat true, :fast-vis true, :quiet? true})
(def vans (future (vrunner/main :sumo addrs opts)))


```

## TODO


* maintain a population but do some novelty search and innovation protection too
  - library not framework. scrap gpai
* save population / evolution state
  - continue from saved state
* CPPN crossover


## License

Copyright © 2015-2018 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
