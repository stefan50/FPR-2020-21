length' :: [a] -> Int
length' list = helper list 0
    where
        helper :: [a] -> Int -> Int
        helper [] counter = counter
        helper (x:xs) counter = helper xs (counter + 1)

-- A ^ B = C
-- A ^ !B = C
-- => A = C
sum' :: [Int] -> Int
sum' list -- (x:y:ys) -- (x:xs)
    | null list = 0
    | otherwise = head list + sum' (tail list)

list1 :: [Int]
list1 = [x | x <- [1..50], even x, x `mod` 3 == 0]

sumOfLists :: [Int] -> [Int] -> [Int]
sumOfLists [] list = list
sumOfLists list [] = list
sumOfLists (x:xs) (y:ys) = (x + y):(sumOfLists xs ys)

eval :: Int -> Bool
eval 0 = True
eval _ = False

evalVec :: (Int, Int) -> Bool 
evalVec (x, y) = x > y

-- foldr f z [x1, x2, x3] <-> f(x1, f(x2, f(x3, z)))
-- foldr1 f [x1, x2, x3] <-> f(x1, f(x2, x3))
-- foldl f z [x1, x2, x3] <-> f(f(f(z, x1), x2), x3)
-- foldl1 f [x1, x2, x3] <-> f(f(x1, x2), x3)

sum'' :: [Int] -> Int
sum'' list = foldr (+) 0 list -- (+) <-> (\ x y -> x + y)
-- f (1, f(2, f(3, 0))) <-> f(1, f(2, 3)) <-> f(1, 5) <-> 6

-- +(x, y) <-> x + y
-- +(2, y) <-> +2(y) <-> y + 2

alternative :: [Int] -> Bool 
alternative list = foldr (\ x y -> y || x > 2) False list
-- f(1, f(2, f(3, False))) <-> f(1, f(2, True)) <-> f(1, True) <-> True
-- f(1, f(1, False)) <-> f(1, False) <-> False

-- + :: Int -> Int -> Int
-- + x y = x + y
(++)                    :: [a] -> [a] -> [a]
[]     ++ ys            =  ys
(x:xs) ++ ys            =  x : (xs++ys)



main :: IO()
main = do
    print list1
    print (sumOfLists [1..2] [2..6]) 
    print (eval 0)
    print (sum'' [1..5])
    print ((+2) 3)
    print (alternative [1..1])
    print (evalVec (2,4))