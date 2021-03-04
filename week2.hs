import Data.Char

isEven :: Int -> Bool
isEven x = if (x `mod` 2 == 0) then True else False 

-- /= <-> not equal to
isOdd :: Int -> Bool 
isOdd x = if (x `mod` 2 /= 0) then True else False

-- Задача 1.​ Да се дефинира функция ​inside a b x​, 
-- която проверява дали числото x принадлежи на интервала [a, b]
inside :: Int -> Int -> Int -> Bool
inside a b x = if (x >= a && x <= b) then True else False

-- Задача 2.​ Да се дефинира функция ​sumSquares a b​, 
-- която намира сумата на квадратите на числата a и b
sumSquares :: Integer -> Integer -> Integer
sumSquares a b = a ^ 2 + b ^ 2

-- Задача 3.​ Да се дефинира функция ​average a b​, 
-- която намира средноаритметичното на a и b
average :: Double -> Double -> Double
average a b = (a + b) / 2

average' :: Int -> Int -> Double
average' a b = (fromIntegral a + fromIntegral b) / 2

-- Задача 4.​  Да се дефинира функция ​squaresAverage a b​, 
-- която намира средно аритметичното на квадратите на a и b 
-- HW

-- Задача 5.​ Да се дефинира функция ​myMin x y​, 
-- която връща минималния от двата си аргумента
-- Clojure
myMin :: Int -> Int -> Int 
myMin x y = if x > y then y else x

-- Задача 6.​ Да се дефинира функция ​myFact n​, 
-- която пресмята факториела начислото n чрез ​рекурсивен ​процес
myFact :: Integer -> Integer
myFact n = if n > 1 then n * (myFact (n - 1)) else 1 

myFact' :: Int -> Int
myFact' n = product [1..n] -- [1..n] = [1, 2, 3, ..., n]

-- Задача 7.​ Да се дефинира функция ​myFactIter n​, 
-- която пресмятафакториела на числото n чрез ​итеративен ​процес
-- Задача 8.​ Да се дефинира функция ​myFib n​, 
-- която връща n-тото число от редицата на Фибоначи 
-- (редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0)
-- Задача 9.​ Да се дефинира функция ​myFibIter n​, 
-- която връща n-тото число отредицата на Фибоначи чрез ​итеративен ​процес
-- Задача 10.​ Да се дефинира функция ​myGcd a b​, която връща НОД(a, b)


-- Задача 1.​ Да се дефинира функция ​countDigits​, 
-- която намира броя на цифрите на дадено естествено число. 
-- Да се напише и итеративно решение
countDigits :: Int -> Int
countDigits a = if (a `div` 10 /= 0) then 1 + countDigits (a `div` 10) else 1

countDigits' :: Int -> Int
countDigits' a 
    | a < 0           = a
    | a `div` 10 /= 0 = 1 + countDigits' (a `div` 10)
    | otherwise       = 1 

-- Задача 2.​ Да се дефинира функция ​sumDigits​, 
-- която намира сумата на цифрите на дадено естествено число. 
-- Да се напише и итеративно решение
-- 2 / 10 = 0 234
sumDigits :: Int -> Int
sumDigits a = if (a `div` 10 == 0) then a else (a `mod` 10) + sumDigits (a `div` 10)

sumDigits' :: Int -> Int 
sumDigits' a 
    | a `div` 10 == 0 = a 
    | otherwise       = (a `mod` 10) + sumDigits' (a `div` 10)

-- Пишем образците от най-специфични към най-общи
testPattern :: Int -> Int
testPattern 0 = 0
testPattern 1 = 34
testPattern x = x + 1

-- Ползваме _ за да обозначим, че няма смисъл да даваме 
-- име на променливата, защото не я ползваме
testPattern' :: Int -> Int
testPattern' 0 = 1
testPattern' _ = 2

-- Можем да обединим образци и гардове, за да направим  
-- по-мощни проверки
testPattern'' :: Int -> Int
testPattern'' 0 = 1
testPattern'' a
    | a `mod` 2 == 0 = 30
    | a `mod` 5 == 0 = 300
    | otherwise      = a ^ 2 

main :: IO()
main = do
    {-
    print (2 + 5) -- a + b, +(a, b)
    print (div 6 2)
    print (6 `div` 2)
    print (6 `mod` 2)
    print (2 ^ 3)
    print (2 ^ (1 `div` 2))
    print (abs (-2))
    print (2 ** fromIntegral(1))
    print (ord 'a')
    print (chr 97)
    print (not True)
    print (isEven 32)
    print (isOdd 32) -}
    {- print (inside 2 5 4)
    print (inside 1 3 7)
    print (sumSquares 2 3)
    print (average' 2 3)
    print (myMin 2 3)
    print (myFact 4) -}
    print (countDigits' (-234))
    print (sumDigits' 234)
    print (testPattern'' 7)
    -- sqrt, 