module Lecture01 where

-- Nearly all the problems that you see here are from 99 problems in haskell
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- NOTE: Do not copy the solutions from this page IF YOU WANT TO LEARN.
-- Otherwise well go ahead. Even better modify the test suite :P.
--
-- NOTE:
-- 1. Everything in Prelude is fair game to use.
-- 2. Try to solve the problems yourself.
-- 3. Only ask for help from people who attenend the lecture with you.
--    (Will create a channel where we can have descussions on these questions)
-- 4. Don't search for solutions on Google.

-- Problem 1
-- (*) Find the last element of a list.
-- λ> problem1 ['x','y','z']
-- 'z'
problem1 :: [a] -> a
problem1 (x:xs)
  | xs == []  = x
  | otherwise = problem1 xs

-- problem2 l = l !! ((length l) - 1)


-- Problem 2
-- (*) Find the last but one element of a list.
-- λ> problem2 [1,2,3,4]
-- 3
-- λ> problem2 ['a'..'z']
-- 'y'
problem2 :: [a] -> a
problem2 a@[_, _] = head a
problem2 (x:x':xs)
  | xs == []  = x
  | otherwise = problem1 xs
-- problem2 l = l !! ((length l) - 2)


-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- Example:
-- λ> problem3 [1,2,3] 2
-- 2
-- λ> problem3 "haskell" 5
-- 'e'
problem3 :: [a] -> Int -> a
problem3 l v = l !! (v + 1)


-- Problem 4
-- (*) Find the number of elements of a list.
-- Example
-- λ> problem4 [123, 456, 789]
-- 3
-- λ> problem4 "Hello, world!"
-- 13
problem4 :: [a] -> Int
problem4 [] = 0
problem4 [_] = 1
problem4 (x:xs) = 1 + (problem4 xs)


-- Problem 5
-- (*) Reverse a list.
-- Example
-- λ> problem5 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> problem5 [1,2,3,4]
-- [4,3,2,1]
problem5 :: [a] -> [a]
problem5 v@[_] = v
problem5 (x:xs) = problem5 xs ++ [x]


-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or
--     backward; e.g. (x a m a x).
-- Example
-- λ> problem6 [1,2,3]
-- False
-- λ> problem6 "madamimadam"
-- True
-- λ> problem6 [1,2,4,8,16,8,4,2,1]
-- True
problem6 :: Eq a => [a] -> Bool
problem6 [] = True
problem6 [_] = True
problem6 (a:l) = (a == last l) && problem6 (init l)


-- Problem 7
-- (**) Flatten a nested list structure.
--      Transform a list, possibly holding lists as elements into a `flat' list by
--      replacing each list with its elements (recursively).
-- Example:
-- Example in Haskell:
-- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a
  = Elem a
  | List [NestedList a]
-- λ> problem7 (Elem 5)
-- [5]
-- λ> problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> problem7 (List [])
-- []

problem7 :: NestedList a -> [a]
problem7 x = 
  case x of
  Elem a -> [a]
  List [] -> []
  List (xs:xss) -> problem7 xs ++ problem7 (List xss)


-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
--      If a list contains repeated elements they should be replaced with a single copy
--      of the element. The order of the elements should not be changed.
-- Example:
-- λ> problem8 "aaaabccaadeeee"
-- "abcade"
problem8 :: Eq a => [a] -> [a]
problem8 xs = [e | (e, e') <- zip xs (tail xs), e /= e'] ++ [last xs]


-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains
--      repeated elements they should be placed in separate sublists.
-- Example:
-- λ> problem9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
problem9 :: Eq a => [a] -> [[a]]
problem9 (x:xs) = foldl aux [[x]] xs
  where 
    aux acc e
      | e == (last (last acc)) = (init acc) ++ [e : (last acc)]
      | otherwise              = (acc ++ [[e]])


-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the
--     so-called run-length encoding data compression method. Consecutive duplicates
--     of elements are encoded as lists (N E) where N is the number of duplicates of
--     the element E.
-- Example:
-- λ> problem10 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
problem10 :: Eq a => [a] -> [(Int, a)]
problem10 [] = []
problem10 (x:xs) = undefined


-- Problem 11
-- (*) Modified run-length encoding.
--     Modify the result of problem 10 in such a way that if an element has no
--     duplicates it is simply copied into the result list. Only elements with
--     duplicates are transferred as (N E) lists.
--
-- Example in lisp (Where it kinda makes sense):
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell (where it does not, not so much without IO):
--
-- λ> problem11 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
data ListItem a
  = Single a
  | Multiple Int a
  deriving (Show, Eq)

-- Modified run-length encoding.
problem11 :: Eq a => [a] -> [ListItem a]
problem11 = undefined


-- Problem 12
-- Decode a run-length encoded list.
-- (**) Decode a run-length encoded list.
--      Given a run-length code list generated asspecified in problem 11.
--      Construct its uncompressed version.
--
-- Example:
-- λ> problem12 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd']
-- "aaaabccaad"
problem12 :: Eq a => [ListItem a] -> [a]
problem12 = undefined


-- Problem 13
-- (**) Run-length encoding of a list (direct solution).
--      Implement the so-called run-length encoding data compression method directly. I.e.
--      don't explicitly create the sublists containing the duplicates, as in problem 9,
--      but only count them. As in problem P11, simplify the result list by replacing the
--      singleton lists (1 X) by X.
--
-- Example:
-- λ> problem13 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
problem13 :: Eq a => [a] -> [ListItem a]
problem13 = undefined


-- Problem 14
-- (*) Duplicate the elements of a list.
--
-- Example:
-- λ> problem14 [1, 2, 3]
-- [1,1,2,2,3,3]
problem14 :: [a] -> [a]
problem14 xs = foldr (++) () map (\x -> [x, x]) xs


-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
-- λ> problem15 "abc" 3
-- "aaabbbccc"
problem15 :: [a] -> Int -> [a]
problem15 xs k = foldr (++) [] (map (\x -> replicate k x) xs)


-- Problem 16
-- (**) Drop every N'th element from a list.
--
-- Example:
-- λ> problem16 "abcdefghik" 3
-- "abdeghk"
problem16 :: [a] -> Int -> [a]
problem16 l v = [e |(idx, e) <- zip [1..(length l+1)] l, (idx `mod` v) /= 0 ]


-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
--     Do not use any predefined predicates.
--
-- Example:
-- λ> problem17 "abcdefghik" 3
-- ("abc", "defghik")
problem17 :: [a] -> Int -> ([a], [a])
problem17 xs k = (take k xs, drop k xs)


-- Problem 18
-- (**) Extract a slice from a list.
--      Given two indices, i and k, the slice is the list containing the elements
--      between the i'th and k'th element of the original list (both limits included).
--      Start counting the elements with 1.
--
-- Example:
-- λ> problem18 ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
problem18 :: [a] -> Int -> Int -> [a]
problem18 l a b = drop a (take b l)


-- Problem 19
-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
-- λ> problem19 ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> problem19 ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
problem19 :: [a] -> Int -> [a]
problem19 = l idx
  | idx < 0     = (drop reverse_idx l) ++ (take reverse_idx l)
  | otherwise = (drop idx l) ++ (take idx l)
  where reverse_idx = length l - idx


-- Problem 20
-- (*) Remove the K'th element from a list.
--
-- Example:
-- λ> problem20 2 "abcd"
-- ('b',"acd")
problem20 :: Int -> [a] -> (a, [a])
problem220 k l = ((l !! k), (init (take (k+1) l)) ++ drop (k+1) l)


-- Problem A
-- (*) Implement merge:
--     Given two sorted lists give out a sorted list
-- Example:
-- λ> problemA [1,3,4] [2,5,6]
-- [1,2,3,4,5,6]
problemA :: Ord a => [a] -> [a] -> [a]
problemA [] [] = []
problemA (x:xs) (x':xs') = x : x' : (problemA xs xs')


-- Problem B
-- (*) Implement merge-sort
--     Given a list returna  sorted list using merge sort algorithm you can use the merge
--     from previous problem
-- Example:
-- λ> problemB [4,3,2,1]
-- [1,2,3,4]
problemB :: Ord a => [a] -> [a]
problemB xs 
  | len <= 1  = xs
  | otherwise = sortTwo (problemB (take middle xs)) (problemB (drop middle xs))
  where
    len = length xs
    middle = fst (len `divMod` 2)
    sortTwo (xx:xxs) (xx':xxs') = xx : xx' : (sortTwo xxs xxs')