# iterated hawk dove game

in the iterated hawk dove game, agents condition their strategy on the previous round. for now, there are 5 types: hawk, dove, tit for tat, cautious tit for tat, win stay lose shift (pavlov).

# how to run

open racket

```
(load "hd.rkt")
(create-population A 200 200 200 200 200)
(evolve-population 200 50 0)
```
