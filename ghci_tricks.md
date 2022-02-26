### Loading a haskell file

Use ```:l file_name.hs```. ```:r``` can be used to reload the file

### Multi-line support

Multi-line can be enabled in GHCi by using ``` :set +m ```:

```haskell
:set +m
{ addList _ [] = []
; addList y (x:x_rest) = x+y:addList y x_rest
}


```
