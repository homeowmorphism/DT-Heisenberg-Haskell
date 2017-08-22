# Run

You will need a version of [Haskell](https://www.haskell.org/). 

From the command line, type

```
$ ghci
Prelude> :load HSet.hs
```

You should see the following response.

```
[1 of 5] Compiling Group            ( Group.hs, interpreted )
[2 of 5] Compiling Heisenberg       ( Heisenberg.hs, interpreted )
[3 of 5] Compiling Set              ( Set.hs, interpreted )
[4 of 5] Compiling IntSet           ( IntSet.hs, interpreted )
[5 of 5] Compiling HSet             ( HSet.hs, interpreted )
Ok, modules loaded: Group, HSet, Heisenberg, IntSet, Set.
```

With the `HSet` module loaded, you have different options for returning balls of different generating sets (with the option to add more). The default options are `balls2, balls3, ballsKnight`.

```
*HSet> balls2
```
returns the elements in the ball with generating set `gen2` (see .. for ) 



# Understanding the code
