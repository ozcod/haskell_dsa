

-----------------------------------
-- Basic Haskell
-----------------------------------

-- Exercise 4.1.1
-- Define a function `double` that triples a given number.
double :: Num a => a -> a
double x = 3 * x
-- >>> double (200+7) 
-- 621


-- Exercise 4.1.2
-- Define a function `ratio` that computes (x+y) / (x-y).
ratio :: Fractional a => a -> a -> a
ratio x y = (x + y) / (x - y)


-- Exercise 4.1.3
-- Define a function `hypotenuse` that computes sqrt(a² + b²).
hypotenuse :: Floating a => a -> a -> a
hypotenuse a b = sqrt (a^2 + b^2)


-- Exercise 4.1.4
-- Find the x-intercept of the line y = mx + c.
xIntercept :: Fractional a => a -> a -> a
xIntercept m c = -c / m


-- Exercise 4.1.5
-- Check if three numbers are all different.
threeDiff :: Eq a => a -> a -> a -> Bool
threeDiff m n p = m /= n && m /= p && n /= p


-- Exercise 4.1.6
-- Compute the average of three integers.
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3


-- Exercise 4.1.7
-- Compute the sum of an arithmetic progression.
arithmeticSum :: Integral a => a -> a -> a -> a
arithmeticSum a n d = (n * (2 * a + (n - 1) * d)) `div` 2


-- Recursive definition of arithmetic sum.
arithSum :: Int -> Int -> Int -> Int
arithSum _ 0 _ = 0
arithSum a n d = a + d*(n-1) + arithSum a (n-1) d


-- Exercise 4.1.8
-- Check if x is between lo and hi (inclusive).
inrange :: Int -> Int -> Int -> Bool
inrange x lo hi = lo <= x && x <= hi

-- Allow reversed bounds as well.
inrange1 :: Int -> Int -> Int -> Bool
inrange1 x lo hi = inrange x lo hi || inrange x hi lo


-- Exercise 4.1.9
-- Exclusive OR (XOR).
orExclusive :: Bool -> Bool -> Bool
orExclusive = (/=)

orExclusive' :: Bool -> Bool -> Bool
orExclusive' a b = (a && not b) || (not a && b)


-- Exercise 4.1.10
-- Find the hundreds digit of a number.
hundreds :: Integral a => a -> a
hundreds n = (n `div` 100) `mod` 10


-- Exercise 4.1.11
-- Get the middle character of a string.
middle :: String -> Char
middle s = s !! (length s `div` 2)

-- Check if a string contains "as".
containsAs :: String -> Bool
containsAs = isInfixOf "as"


-- Multiply any number by 3 (point-free style).
multiplyBy3 :: Num a => a -> a
multiplyBy3 = (* 3)


-----------------------------------
-- Section 4.3 Recursion
-----------------------------------

-- Exercise 4.3.1
-- Define a function `natural` that maps n ↦ n-1 for n>1, else 0.
natural :: Int -> Int
natural n | n <= 1    = 0
          | otherwise = 1 + natural (n-1)


-- Exercise 4.3.2
-- Compute the n-th odd number recursively.
odds :: Int -> Int
odds n | n <= 1    = 1
       | otherwise = 2 + odds (n-1)


-- Exercise 4.3.3
-- Sum of first n natural numbers.
sumRec :: Int -> Int
sumRec n | n <= 0    = 0
         | otherwise = n + sumRec (n-1)


-- Exercise 4.3.4
-- Factorial.
fact :: Int -> Int
fact n | n <= 0    = 1
       | otherwise = n * fact (n-1)


-- Exercise 4.3.5
-- Sum of factorials up to n.
sumFact :: Int -> Int
sumFact n | n < 0     = 0
          | otherwise = fact n + sumFact (n-1)


-- Higher-order recursion combinator.
natInd :: a -> (Int -> a -> a) -> Int -> a
natInd base f n | n <= 0    = base
                | otherwise = f n (natInd base f (n-1))

-- Examples using natInd.
sumRec'  = natInd 0 (\n acc -> n + acc)
fact'    = natInd 1 (\n acc -> n * acc)
sumFact' = natInd 0 (\n acc -> fact n + acc)


-- Exercise 4.3.6
-- n-th term of arithmetic series.
arithmeticSeries :: Int -> Int -> Int -> Int
arithmeticSeries a n d | n <= 0    = a
                       | otherwise = d + arithmeticSeries a (n-1) d


-- Exercise 4.3.7
-- n-th odd number using arithmeticSeries.
odds' :: Int -> Int
odds' n = arithmeticSeries 1 n 2


-- Exercise 4.3.8
-- Sum of arithmetic progression recursively.
arithmeticSumRec :: Int -> Int -> Int -> Int
arithmeticSumRec a n d | n <= 0    = 0
                       | otherwise = arithmeticSeries a n d + arithmeticSumRec a (n-1) d


-- Exercise 4.3.9
-- Multiplication using recursion.
multRec :: Int -> Int -> Int
multRec m n | n < 0     = -multRec m (-n)
            | n == 0    = 0
            | otherwise = m + multRec m (n-1)


-- Exercise 4.3.10
-- Product of numbers in range [m..n].
rangeProduct :: Int -> Int -> Int
rangeProduct m n | n < m     = 1
                 | otherwise = m * rangeProduct (m+1) n


-- Exercise 4.3.11
-- Integer square root using search.
intSqrt :: Int -> Int
intSqrt n = search 0
  where
    search m | (m+1)^2 > n = m
             | otherwise   = search (m+1)


-- Exercise 4.3.12
-- Maximum of f(k) for k ∈ [0..n].
maxfRec :: (Int -> Int) -> Int -> Int
maxfRec f n | n < 0     = 0
            | n == 0    = f 0
            | otherwise = max (f n) (maxfRec f (n-1))


-- Check if f(k) = 0 for some k ≤ n.
oneZero :: (Int -> Int) -> Int -> Bool
oneZero f n | n < 0     = False
            | otherwise = f n == 0 || oneZero f (n-1)


-----------------------------------
-- Section 4.4 Fibonacci
-----------------------------------

-- Exercise 4.4.1
-- Fibonacci sequence (naive recursion).
fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)


-- Exercise 4.4.2
-- Efficient Fibonacci: returns (F(n), F(n+1)).
fiboTwo :: Int -> (Int, Int)
fiboTwo 0 = (1, 1)
fiboTwo n = (y, x + y)
  where (x, y) = fiboTwo (n-1)
