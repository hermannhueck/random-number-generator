# Detecting Functional Patterns while Developing a Random Number Generator

In this small series of examples I develop a functional Random Number Generator.

Starting with mutable generation of pseudo-random values using scala.util.Random
I go to immutable, random value generation using functions: *RNG => (RNG, a)*.
Then I wrap the functions into: *case class Random[A](run: RNG => (RNG, a))*.
After that I improve the design by detecting and implementing several
functional patterns: *map*, *flatMap*, *Monad*, *State Monad*, *map2*, *mapN*,
*pure*, *sequence* and *traverse*.

I implement these functional abstractions in a cats-like way
just to delete the new abstractions and replace them by the respective
import from the Cats library.
