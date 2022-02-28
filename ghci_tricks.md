### Loading a haskell file

Use ```:l file_name.hs```. ```:r``` can be used to reload the file

### Multi-line support

Multi-line can be enabled in GHCi by using ``` :set +m ```:

```
Prelude> :set +m
Prelude> { addList _ [] = []
Prelude| ; addList y (x:x_rest) = x + y : addList y x_rest
Prelude| }
Prelude> addList 1 [0, 1, -1]
[1,2,0]
```

### Checking types

Use ```:type``` or ```:t```:

```
Prelude> :type head
head :: [a] -> a
Prelude> :type tail
tail :: [a] -> [a]
```

### Reusing the last result

Type ```it```:

```
*Main> 5+1
6
*Main> it**2
36.0
```
