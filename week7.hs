-- HW
-- Задача 4. Да се дефинира тип Rat, представящ рационално число като двойка
-- от цели числа - числител и знаменател. Да се дефинират функции за работа с
-- рационални числа:
-- a) sumRat, която сумира две рационални числа
-- b) multiplyRat, която умножава две рационални числа
-- c) divideRat, която разделя рационално число на друго рационално число
-- d) equalRat, която проверява дали две рационални числа са равни.
-- Забележка: две рационални числа с различни числител и знаменател може да
-- са равни, например 1/2 == 2/4 == 4/8 == …
-- e) normalizeRat x, която трансформира рационалното число x в равно на
-- него рационално число, такова че числителят и знаменателят имат НОД 1
-- f) да се пренапише sumRat, така че резултата винаги да е "нормализирано"
-- рационално число, тоест такова, което изпълнява условията от е)

type Rat = (Int, Int)

sumRat :: Rat -> Rat -> Rat
sumRat (x, y) (u, v) = normalizeRat ((x*v + u*y), y*v)  

multiplyRat :: Rat -> Rat -> Rat
multiplyRat (x, y) (u, v) = normalizeRat (x*u, y*v)

divideRat :: Rat -> Rat -> Rat
divideRat a (u, v) = normalizeRat (multiplyRat a (v, u))

normalizeRat :: Rat -> Rat
normalizeRat (x, y) = (x `div` (gcd x y), y `div` (gcd x y))

equalRat :: Rat -> Rat -> Bool
equalRat a b = normalizeRat a == normalizeRat b

-- foldl f z [x1, x2, x3] <-> f(f(f(z, x1), x2), x3)

-- Задача 0.​ Да се обърнат елементите на списък като се използва ​foldr​.
reverseList :: [a] -> [a]
reverseList = foldr (\ x y -> y ++ [x]) [] -- f(x1, f(x2, x3))

{-

Задача 1.​ Да се дефинира функция ​insert :: Int -> [Int] -> [Int]​,която добавя елемент в сортиран списък, като резултатният списък също е сортиран.
Задача 2.​ Да се реализира функция ​insertionSort :: [Int] -> [Int]​,която реализира сортиране чрез вмъкване върху списък.
Задача 3.Да се дефинира функция ​closestPoint ps​,която приема списък ps от точки в равнината (представени чрез двойки​ (x,y) ​)и връща едноаргументна функция,
чиято стойност в дадена точка ​p e най-близката до ​p точка от списъка ​ps​

-}

main :: IO()
main = do
    print (sumRat (1, 2) (1, 2))
    print (multiplyRat (1, 2) (1, 2))
    print (divideRat (1, 2) (1, 2))
    print (normalizeRat (2, 4))
    print (normalizeRat (9, 12))
    print (equalRat (2, 4) (4, 8))
    print (equalRat (9, 12) (3, 4))
    print (foldr (-) 54 [10, 11, 26, 7]) -- f(10, f(11, f(26, f(7, 54))))
    print (foldl (-) 54 [10, 11, 26, 7]) -- f(f(f(f(54, 10), 11), 26), 7)
    print (foldr1 (-) [10, 11, 26, 7]) -- f(10, f(11, f(26, 7)))
    print (foldl1 (-) [10, 11, 26, 7]) -- f(f(f(10, 11), 26), 7)
    -- print (foldr (\x y -> (x+y)/2) 54 [12, 4, 10, 6])
    print (foldr1 (++) ["first", "second", "third"])
    print (foldr (\x y -> concat ["(",x,"+",y,")"]) "0" ["first","second", "third"]) -- "(first+(second+(third+0)))"
    print (foldl (\x y -> concat ["(",x,"+",y,")"]) "0" ["first","second", "third"]) 
    print (reverseList [1..5])