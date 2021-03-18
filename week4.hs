null' :: [a] -> Bool
null' [] = True
null' _ = False

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' list = helper 0 list
    where
        helper :: Int -> [a] -> Int
        helper len [] = len
        helper len (x:xs) = helper (len + 1) xs

length'' :: [a] -> Int 
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = if x == y then True else (elem' x ys)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:(take' (n - 1) xs)

-- Задача 1. Да се дефинира функция primesInRange, която връща списък от
-- простите числа в интервал [a, b].
primesInRange :: Int -> Int -> [Int]
primesInRange a b = [x | x <- [a..b], isPrime x] 
    where
        -- factors :: Int -> [Int]
        factors a = [x | x <- [2..a], a `mod` x == 0]
        -- isPrime :: Int -> Bool 
        isPrime x = factors x == [x]

-- Задача 3. Да се дефинира функция squares, която връща списък от наредени
-- двойки, така че първият елемент на двойката е число, а вторият е квадратът
-- му. За целта, функцията да приема границите на реален интервал и стъпка.
squares :: Int -> Int -> Int -> [(Int, Int)]
squares a b step = [(x, x*x) | x <- [a, (a+step)..b]]

merge :: [Int] -> Int 
merge [] = 0
merge (x:xs) = x * 10 ^ (length xs) + merge xs

-- [1, 2, 3] = 1:2:3:[]
main :: IO()
main = do
    print (null' ["a", "b", "c"])
    print (head' ("head":["a", "b", "c"]))
    print (tail' ["a", "b", "c"])
    print (sum' [1, 2, 3])
    print (length'' [1, 2, 3])
    print (elem' 3 [1, 2, 3])
    print (elem' ("a", "a") [("a", "a"), ("a", "b")]) 
    print (take' 2 [1,2,3])
    print (take' 5 [1,2,3])
    print ([2, 4, 2] ++ [1, 4, 6])
    print ([1..8]) -- texas range
    print (take 10 [1..])
    print ([x*x + y*y | x <- [1..8], y <- [10..20], even x])
    print ([(x, y) | x <- [1..8], y <- [10..20], even x, odd y])
    print ([1..8] !! 5) -- arr[5]
    print (zip [1, 2, 3] [4, 5, 6])
    print (primesInRange 1 20)
    print (squares 1 20 2)
    print (merge [1,4,5])