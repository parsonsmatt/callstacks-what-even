module Lib where

import GHC.Stack
import Data.Maybe (listToMaybe)

headNoCallStack :: [a] -> a
headNoCallStack (x:xs) = x
headNoCallStack [] = error "nope"

headWithCallStack :: HasCallStack => [a] -> a
headWithCallStack (x:xs) = x
headWithCallStack [] = error "nope"

maximumCS :: (HasCallStack, Ord a) => [a] -> a
maximumCS = foldr1CS max

foldr1CS :: HasCallStack => (a -> a -> a) -> [a] -> a
foldr1CS _ [x] = x
foldr1CS k (x:xs) = k x (foldr1CS k xs)
foldr1CS _ [] = error "foldr1 empty list"

someProgram :: HasCallStack => [[Int]] -> Int
someProgram = headWithCallStack . maximumCS


foo :: HasCallStack => Maybe a -> a
foo (Just a) = a
foo Nothing = error "foo is unpleased"

bar :: Maybe a -> a
bar = foo

baz :: HasCallStack => Maybe a -> a
baz = bar
