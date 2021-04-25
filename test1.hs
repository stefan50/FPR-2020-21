import Data.List

-- 1
sumOfEvenly :: Int -> Int -> Int
sumOfEvenly a b = sum (filter isEvenly [a..b])
    where
        isEvenly :: Int -> Bool
        isEvenly num = length [x | x <- [1..num], num `mod` x == 0] `mod` 2 == 0 

-- 2
kthMaxMin :: [Int] -> (Int -> Int)
kthMaxMin list = (\ k -> if k > length (reverse (sort (nub (filter (<0) list)))) then error "No such number" else (reverse (sort (nub (filter (<0) list)))) !! (k - 1))

main :: IO()
main = do
    print ((kthMaxMin [-1,-5,-6,-6,-6,-6]) 2)
    -- print ((kthMaxMin [-1,0,-1,0,-2,3,1,-1]) 3)
    print (reverse(sort (nub (filter (<0) [-1,-5,-6,-6,-6,-6]))))
    print (sumOfEvenly 1 10)