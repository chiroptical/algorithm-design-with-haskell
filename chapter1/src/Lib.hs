{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Function
import Prelude hiding (filter, map)

-- This isn't exactly correct, but should be fine for this book
type Nat = Int

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) = if f x then x : filter f xs else filter f xs

-- 1.2 Processing lists

-- An _online_ algorithm is one that processes a list without having the entire list available from the start

-- For foldr, can replace constructors with the following
-- f :: (a -> b -> b)
-- x :: b
-- Cons 1 (Cons 2 (Cons 3 Nil))
-- s/Cons/f/
-- s/Nil/x/

-- inductive definition of perms1
-- For first iteration
-- map (y:) (inserts x ys)
-- x = 0
-- y = 1
-- ys = [2, 3]
-- inserts x ys = [0, 2, 3]
inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y : ys) = (x : y : ys) : map (y :) (inserts x ys)

perms1 :: [a] -> [[a]]
perms1 [] = [[]]
perms1 (x : xs) = [zs | ys <- perms1 xs, zs <- inserts x ys]

perms1' :: [a] -> [[a]]
perms1' = foldr step [[]]
  where
    step x xxs = concatMap (inserts x) xxs

perms1'' :: [a] -> [[a]]
perms1'' = foldr (concatMap . inserts) [[]]

-- idiom ~ foldr (concatMap . steps) e

picks :: [a] -> [(a, [a])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

perms2 [] = [[]]
perms2 xs = [x : zs | (x, ys) <- picks xs, zs <- perms2 ys]

perms2' [] = [[]]
perms2' xs = concatMap subperms (picks xs)
  where
    subperms (x, ys) = map (x :) (perms2' ys)

-- map f . map g = map (f . g)
-- concatMap f . map g = concatMap (f . g)
-- foldr f e . map g = foldr (f . g) e

-- foldr f e . concat = foldr (f . concat) e
-- f :: a -> b -> b
-- e :: b
-- foldr f e :: [a] -> b
-- concat :: [[a]] -> [a]

first :: (a -> b -> b) -> b -> [[a]] -> b
first f e = foldr f e . concat

-- foldr f e (xs ++ ys) = ?
second :: (a -> b -> b) -> b -> [a] -> [a] -> b
second f e xs ys = foldr f e (xs ++ ys)

-- h (foldr f e xs) = foldr g (h e) xs
-- for all finite lists, provided the following proviso:
-- h (f x y) = g x (h y)

-- The base case,
-- h (foldr f e []) {definition of foldr}~ h e
-- foldr g (h e) [] {definition of foldr}~ h e

-- The induction step
-- h (foldr f e (x : xs))
--    {definition of foldr}~  h (f x (foldr f e xs))
--    {fusion candidate}~     g x (h (foldr f e xs))
--    {induction}~            g x (foldr g (h e) xs)
--    {definition of foldr}~  foldr g (h e) (x : xs)

-- foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs

-- concat = foldr (++) []

-- h (foldr (++) [] xss) = foldr g (h []) xss
-- provided, h (xs ++ ys) = g xs (h ys)

-- Exercise 1.2

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

-- Exercise 1.3

wrap :: a -> [a]
wrap = (: [])

unwrap :: [a] -> Maybe a
unwrap [x] = Just x
unwrap _ = Nothing

single :: [a] -> Bool
single [_] = True
single _ = False

-- Exercise 1.4

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- Exercise 1.5

mapr f = foldr ((:) . f) []

filterr f = foldr (\x acc -> if f x then x : acc else acc) []

-- Exercise 1.6 - foldr f e . filter p

foldrFilter p f e = foldr (\x acc -> if p x then f x : acc else acc) []

-- Exercise 1.7

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- Exercise 1.8

-- dropWhileEnd even [1, 4, 3, 6, 2, 4] = [1, 4, 3]
-- p = even
-- e = []
-- Evaluation happens from the right side first!!!
-- Cons 1 (Cons 4 (Cons 3 (Cons 6 (Cons 2 (Cons 4 Nil)))))

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr folder []
  where
    folder x xs = if p x && null xs then [] else x : xs

-- Exercise 1.9
-- foldr f e xs = if null xs then e else f (head xs) (foldr f e (tail xs))
-- foldl f e xs = if null xs then e else f (foldl f e (init xs)) (last xs)

-- head & tail are O(1), whereas init O(N) and last is O(N)

-- Exercise 1.10
-- foldr f e [x, y, z] = x `f` (y `f` (z `f` e))
-- foldl f e [x, y, z] = ((e `f` z) `f` y) `f` x
-- What is true about `f` and `e` when,
-- foldr f e xs = foldl f e xs
-- Monoid :smile:

-- Exercise 1.11

-- integer [4, 1, 3] = 413

tenToFromZero :: [Integer]
tenToFromZero = (10 ^) <$> [0 ..]

tenToFromOne :: [Integer]
tenToFromOne = (10 ^) <$> [1 ..]

integer :: [Integer] -> Integer
integer xs = sum $ zipWith (*) (reverse xs) tenToFromZero

--integer = snd . foldr (\val (exp, acc) -> (exp + 1, val * 10 ^ exp + acc)) (0, 0)

-- fraction [4, 1, 3] = 0.413

fraction :: [Integer] -> Rational
fraction xs = sum $ zipWith ((/) `on` fromIntegral) xs tenToFromOne

-- fraction xs = sum $ zipWith (/) (fromIntegral <$> xs) (fromIntegral <$> tenToFromOne)
-- fraction = snd . foldl (\(exp, acc) val -> (exp + 1, fromIntegral val / (10 ^ exp) + acc)) (1, 0.0)
