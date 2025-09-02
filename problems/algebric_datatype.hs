-- | Exercise 6 - Algebraic Data Types, Recursion, and Evaluation

import Data.List
import Data.Maybe
import Control.Exception


-------------------------------------------------------
-- Exercise 6.1.1
-- Define a data type for the four seasons of the year.
-- Provide an instance of Enum to cycle through seasons.
-------------------------------------------------------

data Season = Spring | Summer | Autumn | Winter
    deriving (Eq, Show, Enum)

theSeasons :: [Season]
theSeasons = [Spring, Summer, Autumn, Winter]

theSeasons' :: [Season]
theSeasons' = enumFrom Spring


-------------------------------------------------------
-- Exercise 6.1.2
-- Write a function that lists all seasons starting
-- from a given one.
-------------------------------------------------------

seasonsFrom :: Season -> [Season]
seasonsFrom = enumFrom

seasonsFrom' :: Season -> [Season]
seasonsFrom' season = [season ..]

toEnum' :: Int -> Season
toEnum' = toEnum


-------------------------------------------------------
-- Exercise 6.1.3
-- Map the function from 6.1.2 over a list of seasons.
-------------------------------------------------------

mapSeasonsFrom :: [Season] -> [[Season]]
mapSeasonsFrom = map seasonsFrom


-------------------------------------------------------
-- Exercise 6.1.4
-- Define a Month type with all 12 months.
-- Provide functions to convert between month and number.
-------------------------------------------------------

data Month = Jan | Feb | Mar | Apr | May | Jun
           | Jul | Aug | Sep | Oct | Nov | Dec 
  deriving (Eq, Ord, Show, Enum)

monthNumber :: Month -> Int
monthNumber month = fromEnum month + 1

numberMonth :: Int -> Month 
numberMonth n = toEnum (n - 1)


-------------------------------------------------------
-- Exercise 6.1.5
-- Write a function that lists months between two given months.
-------------------------------------------------------

monthFromTo :: Month -> Month -> [Month]
monthFromTo m1 m2 = [m1 .. m2]


-------------------------------------------------------
-- Exercise 6.1.6
-- Write a function that maps a month to its season.
-------------------------------------------------------

monthToSeason :: Month -> Season
monthToSeason month = toEnum $ ((fromEnum month + 10) `mod` 12) `div` 3

monthToSeason' :: Month -> Season
monthToSeason' m
  | m `elem` [Mar .. May] = Spring
  | m `elem` [Jun .. Aug] = Summer
  | m `elem` [Sep .. Nov] = Autumn
  | otherwise             = Winter


-------------------------------------------------------
-- Exercise 6.1.7
-- Define your own Boolean type and conversions.
-------------------------------------------------------

data MyBoolean = MyFalse | MyTrue
  deriving (Eq, Show)

boolToMyBoolean :: Bool -> MyBoolean
boolToMyBoolean False = MyFalse
boolToMyBoolean True  = MyTrue

myBooleanToBool :: MyBoolean -> Bool
myBooleanToBool MyFalse = False
myBooleanToBool MyTrue  = True


-------------------------------------------------------
-- Exercise 6.1.8
-- Define custom boolean operators for MyBoolean.
-------------------------------------------------------

infixr 3 &:&
infixl 6 |:|

(&:&) :: MyBoolean -> MyBoolean -> MyBoolean
MyTrue &:& MyTrue = MyTrue
_      &:& _      = MyFalse

(|:|) :: MyBoolean -> MyBoolean -> MyBoolean
MyFalse |:| MyFalse = MyFalse
_       |:| _       = MyTrue


-------------------------------------------------------
-- Exercise 6.1.9
-- Define conjunction and disjunction functions over lists.
-------------------------------------------------------

myAnd :: [MyBoolean] -> MyBoolean
myAnd []            = MyTrue
myAnd (MyTrue:rest) = myAnd rest
myAnd (MyFalse:_)   = MyFalse

myAnd' :: [MyBoolean] -> MyBoolean
myAnd' = boolToMyBoolean . and . map myBooleanToBool

myAnd'' :: [MyBoolean] -> MyBoolean
myAnd'' = foldr (&:&) MyTrue

myOr'' :: [MyBoolean] -> MyBoolean
myOr'' = foldr (|:|) MyFalse


-------------------------------------------------------
-- Exercise 6.1.10
-- Convert binary numbers (lists of bits) to integers.
-------------------------------------------------------

data Bit = O | I
    deriving (Show, Enum)

bitsToInt :: [Bit] -> Int
bitsToInt = foldl (\n b -> 2*n + fromEnum b) 0


-------------------------------------------------------
-- Exercise 6.2.1
-- Define a Number type that may be exact or approximate.
-------------------------------------------------------

data Number = Exact Int | Approx Float

rounded :: Number -> Int
rounded (Exact i)  = i
rounded (Approx f) = round f


-------------------------------------------------------
-- Exercise 6.2.2
-- Define a Person type and functions to access its data.
-------------------------------------------------------

data Age = Years Int
    deriving Show

data Name = Name String String
    deriving Show

data Person = Person Name Age
    deriving Show

firstName :: Person -> String
firstName (Person (Name first _) _) = first

howOld :: Person -> Age
howOld (Person _ a) = a

addAges :: Person -> Person -> Age
addAges (Person _ (Years y1)) (Person _ (Years y2)) = Years $ y1 + y2


-------------------------------------------------------
-- Exercise 6.4
-- Define and evaluate arithmetic expressions.
-------------------------------------------------------

data Expr = 
     Lit Int 
   | Add Expr Expr 
   | Sub Expr Expr 
   | Mul Expr Expr 
   | Div Expr Expr
  deriving (Eq, Show)

size :: Expr -> Int
size (Lit _)     = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2

eval :: Expr -> Int
eval (Lit x)     = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2)
  | eval e2 == 0 = error "Division by zero"
  | otherwise    = eval e1 `div` eval e2

-- Extended solution with Option type (safe evaluation) omitted here for brevity
