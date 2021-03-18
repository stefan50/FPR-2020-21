-- Задача 6. Да се дефинира функция isPerfect, 
-- която проверява дали дадено
-- число е равно на сумата от делителите си.
-- 6 = 1 + 2 + 3
isPerfect :: Int -> Bool
isPerfect n = n == (sumDiv 1)
    where
        sumDiv start
            | n == start = 0
            | otherwise  = if (n `mod` start == 0) then start + sumDiv (start + 1) else sumDiv (start + 1)

-- Задача 7. Да се дефинира функция reverseNumber, 
-- която по дадено
-- естествено число n намира числото, 
-- записано със същите цифри, но в обратен ред.
-- 123 -> 321
reverseNumber :: Int -> Int
reverseNumber n 
    | n `div` 10 == 0 = n 
    | otherwise       = (n `mod` 10) * (10 ^ (countLength (n `div` 10))) + reverseNumber (n `div` 10)
        where 
            countLength n = if (n `div` 10 /= 0) then 1 + countLength (n `div` 10) else 1

-- Задача 8. Да се дефинира функция isPalindrome, 
-- която проверява дали дадено число е палиндром.
-- HW

-- Задача 3. Да се дефинира функция countOccurences n digit,
-- която връща броят на срещанията на цифрата digit в записа на числото n.
-- HW

-- "name", num, grade
-- tuple
type Student = (String, Int, Double)

-- fst :: (a, b) -> a
-- fst (x, _) = x
 
fst_3 :: (a, b, c) -> a
fst_3 (x, _, _) = x

snd_3 :: (a, b, c) -> b
snd_3 (_, x, _) = x

trd_3 :: (a, b, c) -> c
trd_3 (_, _, x) = x


type TupleInt = (Int, Int)
-- Задача 1. Да се дефинира функция addPair, 
-- която сумира двете координати на наредена двойка.
-- Concept = Typeclass 
addPair :: TupleInt -> Int
addPair (a, b) = a + b 

-- Задача 3. Да се дефинира тип Vector, определящ се от три координати - x, y
-- и z. Да се дефинират функции за работа с типа:
-- а) sumVectors, която намира сумата на два вектора
-- b) scaleVector, която умножава скалар и вектор
-- c) dotProduct, която намира скаларното произведение на два вектора
-- d) crossProduct, която намира векторното произведение на два вектора
-- e) magnitude, която намира дължината на даден вектор
type Vector = (Double, Double, Double)

sumVectors :: Vector -> Vector -> Vector
sumVectors (a, b, c) (x, y, z) = (a + x, b + y, c + z)

scaleVector :: Vector -> Double -> Vector
scaleVector (a, b, c) x = (a * x, b * x, c * x)

-- a * b = a^T * b = a1*b1 + a2*b2 + a3*b3
dotProduct :: Vector -> Vector -> Double
dotProduct (a, b, c) (x, y, z) = a * x + b * y + c * z 

-- (a1, a2, a3) * (b1, b2, b3) = (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)
crossProduct :: Vector -> Vector -> Vector
crossProduct (a1, a2, a3) (b1, b2, b3) = (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

-- magnitude - HW

-- testList :: [Int] -> Int


main :: IO()
main = do
    -- print (isPerfect 3)
    -- print (reverseNumber 1546343)
    print (trd_3 ("Gosho", 3, "s")) 
    print (addPair (23, 56))
    print (sumVectors (1, 2, 3) (4, 5, 6))
    print (scaleVector (1, 2, 3) 4)
    print (dotProduct (1, 2, 3) (4, 5, 6))
    print (crossProduct (1, 2, 3) (4, 5, 6))

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