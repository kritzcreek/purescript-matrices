module Matrix
       ( Matrix()
       , height
       , width
       , repeat
       , fromArray
       , get
       , getRow
       , getColumn
       , prettyPrintMatrix
       , empty
       , isEmpty
       , set
       , modify
       , toIndexedArray
       , indexedMap
       , zipWith
       ) where


import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.CodeUnits as StringCU
import Data.Array (mapMaybe, mapWithIndex)
import Data.Foldable (class Foldable, foldr, intercalate, maximum, foldMap, foldl)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Traversable (class Traversable, traverse, all, sequenceDefault)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)

-- | A two-dimensional Matrix. Its Show instance is meant as a tool for
-- | debugging/pretty-printing.
newtype Matrix a =
  Matrix { size ∷ {x ∷ Int, y ∷ Int}
         , values ∷ Array a
         }

instance showMatrix ∷ Show a ⇒ Show (Matrix a) where
  show = prettyPrintMatrix show

instance eqMatrix ∷ Eq a ⇒ Eq (Matrix a) where
  eq m1 m2 = values m1 == values m2

instance functorMatrix ∷ Functor Matrix where
  map = mapMatrix

mapMatrix ∷ ∀ a b. (a → b) → Matrix a → Matrix b
mapMatrix f (Matrix m) =
  Matrix m {values = map f m.values}

instance foldableMatrix ∷ Foldable Matrix where
  foldr f b m = foldr f b (values m)
  foldl f b m = foldl f b (values m)
  foldMap f m = foldMap f (values m)

instance traversableMatrix ∷ Traversable Matrix where
  traverse f = overValues (traverse f)
  sequence = sequenceDefault

-- | Returns the height of a matrix.
height ∷ ∀ a. Matrix a → Int
height (Matrix m) = m.size.y

-- | Returns the width of a matrix.
width ∷ ∀ a. Matrix a → Int
width (Matrix m) = m.size.x

-- | Repeats the same value and creates a width × height `Matrix`.
-- |
-- | ```purescript
-- | > repeat 2 3 "X"
-- | "X", "X"
-- | "X", "X"
-- | "X", "X"
-- | ```
repeat ∷ ∀ a. Int → Int → a → Matrix a
repeat x y v =
  Matrix { size: {x, y}
         , values: replicate (x * y) v
         }

-- | The empty Matrix.
empty ∷ ∀ a. Matrix a
empty = Matrix {size: {x: 0, y: 0}, values: []}

-- | Checks whether a Matrix is empty
isEmpty ∷ ∀ a. Matrix a → Boolean
isEmpty (Matrix {values: vs}) = Array.null vs

-- | Constructs a Matrix from an Array of Arrays. Returns `Nothing` if the
-- | dimensions don't line up.
-- |
-- | ```purescript
-- | > fromMaybe empty (fromArray [[1,2,3], [4,5,6]])
-- | 1, 2, 3
-- | 4, 5, 6
-- |
-- | > fromArray [[1,2,3], [4,5]]
-- | Nothing
-- | ```
fromArray ∷ ∀ a. Array (Array a) → Maybe (Matrix a)
fromArray vals =
  let
    height' = Array.length vals
    width' = fromMaybe 0 (Array.head vals <#> Array.length)
    allSame = all ((width' == _) <<< Array.length) vals
  in
    if not allSame then
      Nothing
    else
      Just (Matrix {size: {x: width', y: height'}, values: Array.concat vals})

-- | Returns the value at column, row or `Nothing` if the index was out of
-- | bounds.
get ∷ ∀ a. Int → Int → Matrix a → Maybe a
get x y m
  | x >= 0 && y >= 0 && x < width m && y < height m =
    (values m) Array.!! (y * (width m) + x)
  | otherwise = Nothing

-- | Sets the value at column, row or returns `Nothing` if the index was out of
-- | bounds.
set ∷ ∀ a. Int → Int → a → Matrix a → Maybe (Matrix a)
set x y new = modify x y (const new)

-- | Applies the given function to the element at column, row or returns Nothing
-- | if the index was out of bounds.
modify ∷ ∀ a. Int → Int → (a → a) → Matrix a → Maybe (Matrix a)
modify x y new m
  | x >= 0 && y >= 0 && x < width m && y < height m =
    overValues (Array.modifyAt (y * (width m) + x) new) m
  | otherwise = Nothing


-- | Get the row at the given index.
getRow ∷ ∀ a. Int → Matrix a → Maybe (Array a)
getRow y m
  | y < 0 || y >= height m = Nothing
  | otherwise =
    let
      w = width m
      start = y * w
      end = start + w
    in
     Just (Array.slice start end (values m))

-- | Get the column at the given index.
getColumn ∷ ∀ a. Int → Matrix a → Maybe (Array a)
getColumn x m
  | x < 0 || x >= width m = Nothing
  | otherwise =
    let
      w = width m
      maxIndex = Array.length (values m) - 1
      indices = unfoldr (\ix →
                          if ix > maxIndex then
                            Nothing
                          else
                            Just (Tuple ix (ix + w))) x
    in
      traverse ((values m) Array.!! _) indices

-- | Convert a `Matrix` to an indexed Array
toIndexedArray ∷ ∀ a. Matrix a → Array {x ∷ Int, y ∷ Int, value ∷ a}
toIndexedArray m =
  let
    w = width m
    f ix a = { x: ix `mod` w
             , y: ix / w
             , value: a
             }
  in
    mapWithIndex f (values m)

-- | Apply a function to every element in the given Matrix taking its indices
-- | into account
indexedMap ∷ ∀ a b. (Int → Int → a → b) → Matrix a → Matrix b
indexedMap f m =
    Matrix { size: size m
           , values: map (\ {x, y, value} → f x y value) (toIndexedArray m)
           }

-- | Combines two Matrices with the same dimensions by combining elements at the
-- | same index with the given function. Returns Nothing on a dimension
-- | mismatch.
zipWith ∷ ∀ a b c. (a → b → c) → Matrix a → Matrix b → Maybe (Matrix c)
zipWith f a b
  | width a /= width b || height a /= height b = Nothing
  | otherwise =
    Just $ Matrix { size: size a
                  , values: Array.zipWith f (values a) (values b)
                  }

-- | Pretty prints a matrix using the given formatting function on every element
prettyPrintMatrix ∷ ∀ a. (a → String) → Matrix a → String
prettyPrintMatrix showElem m'
  | isEmpty m' = "()"
  | otherwise =
    let
      m = mapMatrix showElem m'
      w = width m
      h = height m
      columnsm = traverse (flip getColumn m) (Array.range 0 (w - 1))
      acc = replicate h ""
    in
    case columnsm of
      Nothing → "Dimensions error"
      Just columns →
        intercalate "\n"
        (mapMaybe (String.stripSuffix (String.Pattern ", "))
         (foldr appendColumn acc columns))
      where
        appendColumn column acc =
          let
            maxLength = fromMaybe 0 $ maximum (map String.length column)
            app previous next = leftPad maxLength next <> ", " <> previous
          in
            Array.zipWith app acc column

-- private

leftPad ∷ Int → String → String
leftPad x s =
  StringCU.fromCharArray (replicate (x - (String.length s)) ' ') <> s

values ∷ ∀ a. Matrix a → Array a
values (Matrix m) = m.values

size ∷ ∀ a. Matrix a → {x ∷ Int, y ∷ Int}
size (Matrix m) = m.size

foreign import replicate ∷ ∀ a. Int → a → Array a

overValues
  ∷ ∀ a b f. (Functor f)
  ⇒ (Array a → f (Array b))
  → Matrix a
  → f (Matrix b)
overValues f (Matrix m) =
  Matrix <$> {size: m.size, values: _} <$> f m.values
