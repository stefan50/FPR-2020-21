inside :: Int -> Int -> Int -> Bool
inside a b = isInside
    where
        isInside :: Int -> Bool
        isInside x = x >= a && x <= b

inside' :: Int -> Int -> (Int -> Bool)
inside' a b = (\ x -> x >= a && x <= b) 

{-test :: Int -> Int -> Int -> Int
test a b c d = a + b + c + d
-}
-- If it were a -> b:
-- f :: Int -> String
-- f(f(2)) = f("two") -> invalid!!
twice :: (a -> a) -> (a -> a) 
twice f = f . f

iter' :: Int -> (a -> a) -> (a -> a)
iter' 0 f = id
iter' n f = f . iter' (n - 1) f -- f^2 = f . f, f^1 = f, f^0 = id
-- id :: a -> a 
-- id x = x

-- f(x, y) <-> f x y
-- f (x, y) <-> (f x) y 
-- curry' :: ((a, b) -> c) -> a -> b -> c 

test' :: (Int -> Int -> Int) -> Int -> Int
test' f x = (f 2 3) * x

test'' :: (Int -> Int) -> Int -> Int 
test'' f x = (f 5) * x 

addNums :: Int -> Int -> Int 
addNums x y = x + y

-- Задача 4.​ Да се дефинира функция ​xSquaredPlusOne :: Int -> Int​, която пресмята ​x^2 + 1​, ползвайки композиция на функции.
xSquaredPlusOne :: Int -> Int 
xSquaredPlusOne = (\ x -> x + 1) . (\ x -> x^2) -- f . g <-> f(g(x))
-- Задача 5.​ Да се дефинира функция ​xPlusOneSquared :: Int -> Int​, която пресмята ​(x+1)^2​, ползвайки композиция на функции.
xPlusOneSquared :: Int -> Int
xPlusOneSquared = (\ x -> x^2) . (\ x -> x + 1) 

main :: IO()
main = do
    print (inside 3 6 8) 
    print (inside' 3 6 8)
    print (twice (\ x -> x^2) 5)
    print (iter' 3 (\ x -> x^2) 5)
    print (test' (+) 6)
    print (test'' (+2) 6)
    print ((addNums 2) 5)
    print (xSquaredPlusOne 5)
    print (xPlusOneSquared 5)
    -- f(x, y)
    -- f(x, y) = f(2, y) = z(y)
    