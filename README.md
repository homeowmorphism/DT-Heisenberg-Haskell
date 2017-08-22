# Run

## Print the balls themselves
You will need a version of [Haskell](https://www.haskell.org/). 

From the command line, type

```
$ ghci
Prelude> :load HSet.hs
*HSet>
```

With the `HSet` module loaded, you have different options for returning balls of different generating sets (with the option to add more). The default options are `balls2, balls3, ballsKnight`.

```
*HSet> balls2
```
returns the elements in the ball with generating set `gen2`. `Heisenberg.hs` tells us that `gens2` is the symmetric generating set generated by `(1,0,0)` and `(0,1,0)`, where the coordinates represent the `(a, b, c)` values of the matrix`[[1,a,b],[0,1,c],[0,0,1]]`. Thus, `gens2` represent the standard generating set of the Heisenberg group group and `(1,0,0)` and `(0,1,0)` are written in their matrix form as `[[1,1,0],[0,1,0],[0,0,1]]` and `[[1,0,0],[0,1,1],[0,0,1]]` respectively. 

## Print the size of the ball

```
$ ghci
Prelude> :load Test.hs
*Test> sizes gen2
```
prints the sizes for the ball with `gens2` as generating set. `Heisenberg.hs` tells us that`gens2` represents the standard generating set. Other preset options in `Heisenberg.hs` are: `gen3` and `genKnight`.

See print the balls themselves section for more details.

# Understanding the code

*Since it is unreasonable to expect someone to be simultaneously fluent in Haskell and in the Heisenberg group, we chose to expect some fluency with the Heisenberg group.*

#### Group.hs
The code starts with the file `Group.hs` which is simply defined as a monoid with an inverse operation. How to take the inverse is unspecified and left to the individual group. 

#### Heisenberg.hs
`Heisenberg.hs` defines the Heisenberg group, using individual element `Heisen` which inherits the type `Monoid` and takes as input the three parameters `a, b, c` of type `!Int`, which correspond to the coordinates of the group of matrices `[[1,a,b],[0,1,c],[0,0,1]]`. `Heisen` is then defined as a group element with inverse operation which maps `a b c` to `-a -b -c+a*b` (which is the correct formulation given the matrix notation). A type `Gens` (which stands for a generating set) is defined a list of `Heisen` elements. The function `makeSym` makes a given generating set a symmetric one by adding in the `one` element and the inverses of each element. Some generating sets are pre-defined, such as `gen2` (the standard generating set) `gen3` and `genKnight` (the chess generating set). Finally, a function `minkowski` is declared. `minkowski` takes as input two sets of `Heisen` elements and multiply each element of the first set with every element of the second set (I'm not sure I got this completely right). The balls corresponding to the individual generating sets are then recursively defined to be the union of the previous ball multiplied with the generating set. 

### Complexity

Let `n` be the radius of the ball for which we are calculating the size of. Let's assume monoid multiplication between `Heisen` element takes `O(1)` time. By a [formula of Bass and Guivarc'h](https://en.wikipedia.org/wiki/Gromov%27s_theorem_on_groups_of_polynomial_growth), each ball contains `O(n^3)` Heisen elements. Since we do multiplication on each element of the ball at each steps, it takes `O(n^3) = O(1)*O(n^3)` time to execute each step. Since we would like to know the size of the ball of radius `n`, we need to execute `n` such steps. Therefore, the time-complexity is `O(n^4)`. 

However, everything is lazily evaluated and I suspect that Haskell could be doing some optimization leading to an `O(n^3)` complexity. -- I need to ask someone about this.

## Better than the naive solution: how to reduce by a linear factor.

#### HSet.hs
The previous code was rather straightforward. `HSet.hs` briefly increments on this by treating the `c` coordinate in a special way. In the Heisenberg group, the `c` (formally `H 0 0 c`) coordinate is generated by taking the commutator of `a,b`, (formally `H a 0 0, H 0 b 0`) and `c` doesn't generate anything except itself when combined with other generators. This suggest that we can compress the `c` coordinate without affecting our calculations. We are going to group the `c` coordinate as intervals, defined by the data type `IntSet` defined in `IntSet.hs`.


#### IntSet.hs
*For ease of understanding the unIS wrapper has been omitted.*

The biggest challenge in understanding `IntSet.hs` is realizing that this data type is supposed to represent a set of intervals with integer values. Once this is clear, `merge'` and `merge''` are easy to understand as the required function to take the `union` of a list of intervals. Other auxiliary functions for interval lists `IntSet` include defining a `singleton` based on an integer, `toList` which lists every element in the list of intervals (ex: `[(1,4)]` would be listed as `[1,2,4]`) and `size` which gives us the number of integers contained in the list of intervals. (ex `[(1,4), (6,7)]` would have `4 = 3 + 1` elements). Finally, `translateIS` allows us to translate the entire list of intervals by a number (ex: translating `[(1,4), (6,7)]` by `3` would give us `[(4,7), (9,10)]`.

Back to `HSet.hs`, the `IntSet.hs` pays off its dividends. Instead of defining single elements of `Heisenberg` as `H a b c`, we define a single element of an `HSet` as `H a b cs` where `cs` represents an interval of integers of type `IntSet`. Thus, many elements of the Heisenberg group with the same `a, b` coordinates can be compressed in that way. `merge', Set HSet` and `translateHS` all inherit their functionality from their `IntSet` counterpart. This allows us to treat `HSet` as a `Monoid` with multiplication given by `translateHS` effectively reducing the computation by a dimension. 

### Complexity

Like in the naive solution, there is a certain number of elements in the ball of radius `n`, and `n` steps need to be executed to get our solution. Let `B(n)` be the number of HSet elements in the ball, and assume for sake of argument (I will need to confirm this later) that multiplication with an HSet element is `O(1)`. 

How many HSets are there in a ball of radius `n`? This needs to be proven but intuititvely, the power of `n` represents the volume element of the ball. Since we have one less coordinate to worry about, there are `O(n^2)` HSets in the ball. Thus the overall complexity is `O(n^3)`.

--- 

# Credit

This code was written by [Dylan Thurston](http://pages.iu.edu/~dpthurst/). Documentation by [Hang Lu Su](http://homeowmorphism.com/).

# Questions

* What is the actual complexity of HSet multiplication? *Hint:* Linear or less.
* Work out the geometry of the generating sets with respect to those `c` intervals.
* Can you prove that there are `O(n^2)` HSets in each ball?

* How does Haskell do lazy evaluation?
* Is it possible to input a number `n` and print the associated `sigma_n`?
