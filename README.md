# Deducing the State Monad

In this small series of examples of simple Stack implementation
I deduce the State Monad.

Starting with a mutable Stack
I go to a immutable, functional Stack: s => (s, a)
Then I wrap the function into a case class Stack
and make it a Monad. Then I abstract the Stack to State
and implement the convenience functions (pure, get, set, modify
and inspect) that Cats also provides. In the last example
I delete my own implemention of State and import cats.data.State
instead. The Stack client code works as before.

A detailed description of each implementation step can be found
in the comments of the respective source file.
