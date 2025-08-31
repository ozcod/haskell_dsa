--List and Highorder function

----------------------
-- Exercise 5.1.1
-- Write a function that returns a list of all even numbers
-- between two given integers (inclusive).
----------------------

listEvens :: Int -> Int -> [Int]
listEvens x y = [k | k <- [y, y-1 .. x], even k]

listEvens' :: Int -> Int -> [Int]
listEvens' x y =
  [k | k <- [y, y-1 .. x], k `mod` 2 == 0]

listEvens'' :: Integral a => a -> a -> [a]
listEvens'' x y = reverse [k | k <- [x..y], even k]


----------------------
-- Exercise 5.1.2
-- Generate all Pythagorean triples (a, b, c) such that
-- a² + b² = c² with 1 ≤ a ≤ b ≤ c ≤ n.
----------------------

type Triple = (Int, Int, Int)

pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples n =
  [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a^2 + b^2 == c^2]

pythagoreanTriples' :: Int -> [Triple]
pythagoreanTriples' n =
  [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2, a <= b]


----------------------
-- Exercise 5.1.3
-- Write a function that adds two lists pairwise.
-- Example: [1,2,3] + [4,5,6] = [5,7,9]
----------------------

addPairwise :: [Int] -> [Int] -> [Int]
addPairwise = zipWith (+)

addPairwise' :: [Int] -> [Int] -> [Int]
addPairwise' = zipWith (\x y -> x + y)

-- Recursive definition
addPairwise'' :: [Int] -> [Int] -> [Int]
addPairwise'' (x:xs) (y:ys) = (x + y) : addPairwise'' xs ys
addPairwise'' _ _ = []


----------------------
-- Exercise 5.1.4
-- Write a function that extracts a sublist given indices (i, j).
-- Example: subList [1..5] (0,3) = [1,2,3,4]
----------------------

subList :: [a] -> (Int, Int) -> [a]
subList xs (i,j) = [xs !! n | n <- [i..j]]

subList' :: [a] -> (Int, Int) -> [a]
subList' xs (i,j) = take (j - i + 1) (drop i xs)

subList'' :: [a] -> (Int, Int) -> [a]
subList'' [] _ = []
subList'' (x:xs) (i,j)
  | i > j     = []
  | i == 0    = x : subList'' xs (0, j-1)
  | otherwise = subList'' xs (i-1, j-1)


----------------------
-- Exercise 5.1.5
-- Write a function that pairs up adjacent elements in a list.
-- Example: together [1..5] = [(1,2),(2,3),(3,4),(4,5)]
----------------------

together :: [a] -> [(a,a)]
together xs = zip xs (tail xs)

together' :: [a] -> [(a,a)]
together' (x:y:xs) = (x,y) : together' (y:xs)
together' _        = []


----------------------
-- Exercise 5.2.1
-- Write a function that checks if a list contains an element.
----------------------

contains :: Eq a => [a] -> a -> Bool
[] `contains` _      = False
(x:xs) `contains` a  = a == x || xs `contains` a

contains' :: Eq a => [a] -> a -> Bool
contains' xs a = any (== a) xs

elem' :: Eq a => a -> [a] -> Bool
elem' a = any (== a)


----------------------
-- Exercise 5.2.2
-- Write a safe indexing function that returns Maybe.
----------------------

nth :: Int -> [a] -> Maybe a
nth _ []     = Nothing
nth 0 (x:_)  = Just x
nth n (_:xs)
  | n < 0     = Nothing
  | otherwise = nth (n-1) xs


----------------------
-- Exercise 5.2.3
-- Write a function that removes all occurrences of an element.
----------------------

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove a (x:xs)
  | a == x    = remove a xs
  | otherwise = x : remove a xs

remove' :: Eq a => a -> [a] -> [a]
remove' a = filter (/= a)


----------------------
-- Exercise 5.2.4
-- Write a function that substitutes one element with another.
----------------------

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (z:zs)
  | x == z    = y : substitute x y zs
  | otherwise = z : substitute x y zs

substitute' :: Eq a => a -> a -> [a] -> [a]
substitute' x y = map (\z -> if z == x then y else z)


----------------------
-- Exercise 5.3.1
-- Define addition of times (minutes, seconds),
-- making sure seconds are always < 60.
----------------------

type Time = (Int, Int)

addTimes :: Time -> Time -> Time
addTimes (m1,s1) (m2,s2) =
  (m1 + m2 + carry, secs)
  where
    total = s1 + s2
    carry = total `div` 60
    secs  = total `mod` 60

sumTimes :: [Time] -> Time
sumTimes = foldr addTimes (0,0)


----------------------
-- Exercise 5.3.2
-- Solve the Tower of Hanoi problem for n discs.
----------------------

type Moves = (Char, Char)
type Solution = [Moves]

hanoi :: Int -> Solution
hanoi n = hanoi' n 'a' 'c' 'b'

hanoi' :: Int -> Char -> Char -> Char -> Solution
hanoi' 0 _ _ _ = []
hanoi' n a c b =
  hanoi' (n-1) a b c ++ [(a,c)] ++ hanoi' (n-1) b c a

