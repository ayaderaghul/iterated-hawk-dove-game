# iterated hawk dove game

*sorry i dont have a just-work version of the code, but you may try to read, it's readable.*

*or at least see the png file, they are results i run at small scales*

in the iterated hawk dove game, agents condition their strategy on the previous round. for now, there are 5 given types: hawk, dove, tit for tat, cautious tit for tat, win stay lose shift (pavlov).

however, new machines can be defined accordingly, then other command lines are mostly reusable (the visualising on TV part may need to be adjusted with new machines)

# how to run

open racket

```
(load "hd.rkt")
(create-population A 200 200 200 200 200) ; hawk dove tft ctft pavlov
(evolve-population 200 50 0) ; run 200 cycles, speed 50, pause 0s
```
