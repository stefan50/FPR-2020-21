-- foldr f z [x1, x2, x3] <-> f(x1, f(x2, f(x3, z)))
-- foldr1 f [x1, x2, x3] <-> f(x1, f(x2, x3))
-- foldl f z [x1, x2, x3] <->  f(f(f(z,x1), x2), x3)
-- foldl1 f [x1, x2, x3] <-> f(f(x1, x2), x3)

reverseList :: [a] -> [a]
reverseList = foldr (\ x y -> y ++ [x]) [] -- f(1, f(2, f(3, [])))
-- f :: a -> [a] -> [a]
-- - :: (Num a) => a -> a -> a

-- (x, i) <- zip list [1..(length list)] XOR x <- list, i <- [1..(length list)]
getOddCompositionValue :: [(Int -> Int)] -> (Int -> Int)
getOddCompositionValue list = foldr (.) id [x | (x, i) <- zip list [1..(length list)], odd i] 

type Account = (Int, Int, Double)
type Person = (Int, String, String)
type BankDB = ([Account], [Person])

removeNb :: Int -> [(Int, Int)]
removeNb n = [(a, b) | a <- [1..n], b <- [1..n], a*b == sum [x | x <- [1..n], x /= a, x /= b]]

-- f \in {A^n -> A} <-> f takes one arg and returns an (n - 1)-argument function
f :: Int -> Int -> Int
f x y = x + y
-- f(x, y) (maths) <->? (f x) y <-> f x y (Haskell, lambda calculus)
-- f(x, y), when (x, y) = P => f(P)

f' :: (Int, Int) -> Int
f' (x, y) = x + y

main :: IO()
main = do
    print (foldr (-) 54 [10, 11, 26, 7]) -- (-) <-> (\ x y -> x - y)
    print (foldl (-) 54 [10, 11, 26, 7])
    print (foldr1 (-) [10, 11, 26, 7])
    print (foldl1 (-) [10, 11, 26, 7])
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 
    print (f 5 6)
    print (f' (5, 6))
    print (curry f' 5 6)
    print (map (curry f' 20) [1..5])
    print (removeNb 26)