# Foldr examples

### Multiplying two number using (+)

Multiplication between two number $n$ and $m$ can be very naturally implemented with ```foldr``` (only for the sake of the example, of course). We can i) build a function which constructs an array with m copies of a number n and ii) apply foldr and the (+) operator to this array.

```haskell
Prelude> let mcopy m n | m < 0 = [] | otherwise = n:mcopy (m-1) n
Prelude> mult m n = foldr (+) 0 (mcopy m n)
Prelude> mcopy 5 2
[2,2,2,2,2,2]
Prelude> mult 5 2
12
```

Note how the list is constructed _recursively_. This is is implemented through the function  ```mcopy```, whose first argument is the index of the list (to keep track of how many copies should be made), so we can call it recursively until the index reaches 0. Meanwhile the second argument is just the number being copied. ```foldr``` is then used to apply the (+) operators to this list, effectively performing the sum

$$
n \times m = n + .... + n
$$

m times.
