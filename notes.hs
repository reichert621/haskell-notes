--Notes from 'Learn You A Haskell'
--(http://learnyouahaskell.com)

--FizzBuzz
fizz x = if (mod x 3 == 0) then "fizz" else ""
buzz x = if (mod x 5 == 0) then "buzz" else ""
fizzbuzz x = if (null (fizz x ++ buzz x))
             then show x
             else (fizz x ++ buzz x)

--[fizzbuzz x | x <- [1..100]]


--All true:
--10 + 40 == 50
--(+) 10 40 == 50
--(+ 10) 40 == 50


getEven :: Integral t => [t] -> [t]
getEven xs = [x | x <- xs, even x]

getEvenMatrix :: Integral t => [[t]] -> [[t]]
getEvenMatrix xxs = [getEven xs | xs <- xxs]

--getEvenMatrix [[1,2,3,4], [5,6,7,8], [9,10,11,12]]

rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

--removeVowels :: [Char] -> [Char]
removeVowels :: String -> String
removeVowels str = [s | s <- str, not (elem s "aeiouAEIOU")]

--Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

--factorial x = product [1..x]
--factorial' x = if x <= 1 then 1 else x * factorial (x - 1)

--Recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

len :: (Num b) => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs

add :: (Num a) => [a] -> a
add [] = 0
add (x:xs) = x + add xs

--Guards
tellGrade :: (Integral a) => a -> String
tellGrade a
  | a < 60 = "F"
  | a < 70 = "D"
  | a < 80 = "C"
  | a < 90 = "B"
  | otherwise = "You're a genius!"


--initials :: String -> String -> String
--initials fname lname = [head fname] ++ ". " ++ [head lname] ++ "."

--Where
initials :: String -> String -> String
initials fname lname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = fname
        (l:_) = lname

calcHypotenuse :: Floating t => [(t, t)] -> [t]
calcHypotenuse xs = [hyp x y | (x, y) <- xs]
  where hyp a b = sqrt (a^2 + b^2)

--calcBmis :: (RealFloat a) => [(a, a)] -> [a]
--calcBmis xs = [bmi w h | (w, h) <- xs]
--  where bmi w h = w / h^2

--Let
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

--caseExample xs = case xs of [] -> "empty."
--                         [x] -> "a singleton list."
--                         xs -> "a longer list."

--Case statement
caseExample :: [t] -> [Char]
caseExample xs = case xs of [] -> "Empty!"
                            [_] -> "Single!"
                            [_, _] -> "Pair!"
                            _ -> "List!"

--maximum' :: (Ord a) => [a] -> a
--maximum' [] = error "maximum of empty list"
--maximum' [x] = x
--maximum' (x:xs)
--    | x > maxTail = x
--    | otherwise = maxTail
--    where maxTail = maximum' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--rep :: (Enum t1, Num t1) => t -> t1 -> [t]
--rep n x = [n | _ <- [1..x]]

rep :: (Num a, Ord a) => i -> a -> [i]
rep n x
    | x <= 0    = []
    | otherwise = n:rep n (x-1)

--Infinite list of n
rep' :: a -> [a]
rep' n = n:rep' n

--Another way to do `rep` from above
getReps :: a -> Int -> [a]
getReps n x = take x (rep' n)


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [n | n <- xs, n <= x]
      largerSorted  = quicksort [n | n <- xs, n > x]
  in smallerSorted ++ [x] ++ largerSorted

--Using filter instead of list comprehension
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort' (filter (<= x) xs)
      largerSorted  = quicksort' (filter (> x) xs)
  in smallerSorted ++ [x] ++ largerSorted

--All true:
--0 < 10
--(<) 0 10
--(> 0) 10

--Currying simple examples
isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (> 0)
-- isPositive n = n > 0

takeHalf :: (Floating a) => a -> a
takeHalf = (/ 2)
--takeHalf n = n / 2

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
--isUpperAlphanum c = elem c ['A'..'Z']

applyTwice :: (a -> a) -> a -> a
applyTwice f n = f (f n)
--applyTwice (+5) 10 == 20
--applyTwice takeHalf 4 == 1

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

--Map vs list comprehension:
--map (+3) [1,5,3,1,6] == [x+3 | x <- [1,5,3,1,6]]

filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

--Filter vs list comprehension:
--filter even [1,2,3,4,5,6] == [x | x <- [1,2,3,4,5,6], even x]

--find the sum of all odd squares that are smaller than 10,000
--sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

--Generate Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | odd n = n : chain (n*3 + 1)

--With a lambda
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
--numLongChains = length (filter isLong (map chain [1..100]))
--  where isLong xs = length xs > 15

--Lambda not necessary in latter case
--map (*10) [1,2,3,4,5] == map (\x -> x * 10) [1,2,3,4,5]


--Generate fibonacci sequence
fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

getFibs :: Int -> [Integer]
getFibs x = take x fibs
