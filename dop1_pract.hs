import Data.List

isInteresting :: Int -> Bool
isInteresting num = num `mod` (sumDigits num) == 0 
    where
        sumDigits :: Int -> Int
        sumDigits 0   = 0
        sumDigits num = num `mod` 10 + sumDigits (num `div` 10)

numsInInterval :: Int -> Int -> Int 
numsInInterval a b = sum [x | x <- [a..b], (x - 1) `mod` 4 == 0, isThere6 x]
    where
        isThere6 :: Int -> Bool
        isThere6 0   = False 
        isThere6 num = (num `mod` 10) == 6 || isThere6 (num `div` 10)

isArithmeticProgression :: [Int] -> Bool 
isArithmeticProgression []       = True
isArithmeticProgression (x:y:z:xs) = y + (y - x) == z && isArithmeticProgression (y:z:xs)
isArithmeticProgression [_, _] = True
isArithmeticProgression [_]    = True

filterArithmeticProgression :: [[Int]] -> [[Int]]
filterArithmeticProgression list = [x | x <- list, isArithmeticProgression x]

filterArithmeticProgression' :: [[Int]] -> [[Int]]
filterArithmeticProgression' list = filter isArithmeticProgression list

sin' :: Int -> Double -> Double
sin' n x = sum [((-1) ** (fromIntegral i) * x ** (fromIntegral (2*i + 1))) / (fromIntegral (product  [1..(2*i + 1)])) | i <- [0..n]]

-- Ord >=, <=, >, <
-- Eq ==
-- f . g = f(g(x))
-- f, g => f . g => (f . g)(x) 
dominates :: (Ord b, Num b) => (a -> b) -> (a -> b) -> [a] -> Bool
dominates f g xs = all id (zipWith (>=) (map (abs . f) xs) (map (abs . g) xs))

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)

fst3 :: Record -> Student
fst3 (x, _, _) = x

snd3 :: Record -> Subject 
snd3 (_, x, _) = x

trd3 :: Record -> Note
trd3 (_, _, x) = x

-- hardestSubject :: [Record] -> Subject
-- hardestSubject list = min [(subject, grade) | subject <- [map snd3 list]]

-- reverseOrdSuff :: Int -> Int 
-- reverseOrdSuff num = if (num `mod` 10 < (num `mod` 100) `div` 10) then (num `mod` 10) * 10 + reverseOrdSuff (num `div` 10)  else (num `mod` 10)  

reverseOrdSuff' :: Int -> Int
reverseOrdSuff' n = helper n 0
    where
        helper :: Int -> Int -> Int
        helper 0 result = result
        helper n result
            | n < 0 = error "n was negative"
            | mod n 10 >= (div (mod n 100) 10) = (result * 10 + (mod n 10))
            | otherwise = helper (div n 10) (result * 10 + mod n 10)

sumUnique :: [[Int]] -> Int
sumUnique list = sum [sum (removeDuplicatesStrict x) | x <- list]
    where 
        removeDuplicatesStrict :: [Int] -> [Int]
        removeDuplicatesStrict [] = []
        removeDuplicatesStrict (x:xs)
            | x `elem` xs = [el | el <- xs, el /= x]
            | otherwise = x : (removeDuplicatesStrict xs)


main :: IO()
main = do
    print (isInteresting 411)
    print (numsInInterval 10 210)
    print (filterArithmeticProgression [[1..5], [1, 5..200], [1,3,5,1]])
    print (filterArithmeticProgression' [[1..5], [1, 5..200], [1,3,5,1]])
    print (sin' 10 1)
    print (dominates (*3) (*2) [1..5])
    print (reverseOrdSuff' 37563)
    print (reverseOrdSuff' 32763) -- → 367
    print (reverseOrdSuff' 32567) -- → 7
    print (reverseOrdSuff' 32666) -- → 6
    print (sumUnique [[1,2,3,2],[-4,-4],[5]])