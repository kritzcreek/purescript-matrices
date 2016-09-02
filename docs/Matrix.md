## Module Matrix

#### `Matrix`

``` purescript
newtype Matrix a
```

A two-dimensional Matrix. Its Show instance is meant as a tool for
debugging/pretty-printing.

##### Instances
``` purescript
(Show a) => Show (Matrix a)
(Eq a) => Eq (Matrix a)
Functor Matrix
Foldable Matrix
Traversable Matrix
```

#### `height`

``` purescript
height :: forall a. Matrix a -> Int
```

Returns the height of a matrix

#### `width`

``` purescript
width :: forall a. Matrix a -> Int
```

Returns the width of a matrix

#### `repeat`

``` purescript
repeat :: forall a. Int -> Int -> a -> Matrix a
```

Repeats the same value and creates a width × height `Matrix`

```purescript
> repeat 2 3 "X"
"X", "X"
"X", "X"
"X", "X"
```

#### `empty`

``` purescript
empty :: forall a. Matrix a
```

The empty Matrix

#### `isEmpty`

``` purescript
isEmpty :: forall a. Matrix a -> Boolean
```

Checks whether a Matrix is empty

#### `fromArray`

``` purescript
fromArray :: forall a. Array (Array a) -> Maybe (Matrix a)
```

Constructs a Matrix from an Array of Arrays. Returns `Nothing` if the
dimensions don't line up.

```purescript
> fromMaybe empty (fromArray [[1,2,3], [4,5,6]])
1, 2, 3
4, 5, 6

> fromArray [[1,2,3], [4,5]]
Nothing
```

#### `get`

``` purescript
get :: forall a. Int -> Int -> Matrix a -> Maybe a
```

Returns the value at column, row or `Nothing` if the index was out of
bounds

#### `set`

``` purescript
set :: forall a. Int -> Int -> a -> Matrix a -> Maybe (Matrix a)
```

Sets the value at column, row or returns `Nothing` if the index was out of
bounds

#### `modify`

``` purescript
modify :: forall a. Int -> Int -> (a -> a) -> Matrix a -> Maybe (Matrix a)
```

Applies the given function to the element at column, row or returns Nothing
if the index was out of bounds

#### `getRow`

``` purescript
getRow :: forall a. Int -> Matrix a -> Maybe (Array a)
```

#### `getColumn`

``` purescript
getColumn :: forall a. Int -> Matrix a -> Maybe (Array a)
```

#### `toIndexedArray`

``` purescript
toIndexedArray :: forall a. Matrix a -> Array { x :: Int, y :: Int, value :: a }
```

#### `indexedMap`

``` purescript
indexedMap :: forall a b. (Int -> Int -> a -> b) -> Matrix a -> Matrix b
```

#### `prettyPrintMatrix`

``` purescript
prettyPrintMatrix :: forall a. (a -> String) -> Matrix a -> String
```

#### `ex`

``` purescript
ex :: Matrix Int
```

#### `main`

``` purescript
main :: forall t13766. Eff (console :: CONSOLE | t13766) Unit
```


