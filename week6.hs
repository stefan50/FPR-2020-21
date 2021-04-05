-- all (>3) [] -> True
-- (forall x in null)[x in null -> x > 3]
-- x in null is False always -> implication is True

-- any (>3) [] -> False
-- (x in null && x > 3) 

-- ("ala" ++ "bala")
-- isStefan
isStefan :: String -> Bool
isStefan [] = False
isStefan x = x == "Stefan"

makeStefan :: String -> String
makeStefan x = "Stefan"

-- map
-- map f [a1, a2, ..., an] -> [f(a1), f(a2), ..., f(an)]

-- [1..n]
-- recursively - n
-- with use of !!

-- n times
recursiveId :: [a] -> [a]
recursiveId [] = []
recursiveId (x:xs) = x:recursiveId(xs)

-- 1 + 2 + 3 + ... + n = sum of arithmetic progression (in theory, infinite)
-- 1 + 2 + 3 + ... + n >> n
doubleExclMark :: [a] -> [a]
doubleExclMark list = helper list 0
    where
        helper :: [a] -> Int -> [a]
        helper list i = if i == length list then [] else (list !! i) : (helper list (i + 1))

-- а) ​incrementAllBy​, която получава списък и число и го добавя към всеки елемент на списъка
-- [1, 2, 3] 5 -> [1 + 5, 2 + 5, 3 + 5] -> [6, 7, 8]
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy list num = map (\ x -> x + num) list

incrementAllBy' :: [Int] -> Int -> [Int]
incrementAllBy' list num = map (+num) list

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

-- б) ​multiplyAllBy​, която получава списък и число и умножава всеки елементна списъка по числото;
multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy list num = map (*num) list

-- в) ​filterSmallerThan​, която получава списък и число и премахваелементите на списъка, които са по-големи или равни на числото (т.е. оставясамо тези елементи, които са по-малки от числото).
filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan list num = filter (<num) list

-- Задача 2.​ Да се дефинира функция ​splitByParity :: [Int] -> ([Int],[Int])​, която получава списък от цели числа и го разделя на два списъка - отнечетни и четни.
splitByParity :: [Int] -> ([Int], [Int])
splitByParity list = (filter even list, filter odd list) -- odd = not . even

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f list = (filter f list, filter (not . f) list)

splitByParity' :: [Int] -> ([Int], [Int])
splitByParity' list = partition' even list

-- Задача 5.​ Да се дефинира функция ​quickSort :: [Int] -> [Int]​, коятореализира бързо сортиране върху списък
-- HW


-- Задача 6.​ Нека ​as = [a1, a2 ... , ak]​ и ​bs = [b1, b2 ... , bk​] санепразни списъци с еднакъв брой числа. Да се дефинира предикатisImage :: [Int] -> [Int] -> Bool​, който да връща „истина“ точнокогато съществува такова число ​x​, че ​ai = x + bi​ за всяко ​i = 1,..., k​.
isImage :: [Int] -> [Int] -> Bool
isImage list1@(x:_) list2@(y:_) = map (+(x-y)) list2 == list1
-- a1 + b1 - a1 = b1
-- a2 + b1 - a1 = b2 <-> a2 = x + b2, let x = a1 - b1
-- ...
-- an + b1 - a1 = bn

triMatrix :: [[Int]]
triMatrix = [[1, 2, 3, 4],
             [0, 1, 2, 3],
             [0, 0, 2, 3],
             [0, 0, 0, 5]]

-- Задача 7.​ Да се дефинира предикат ​isTriangular :: [[Int]] -> Bool​,който получава квадратна числова матрица, представена като списък отсписъци, и проверява дали тя е горно триъгълна, т.е. дали всичките елементипод главния ѝ диагонал са нули.
-- isTriangular :: [[Int]] -> Bool
-- HW


-- foldr f z [x1 .. x4] <-> f(x1, f(x2, f(x3, f(x4, z))))
-- f(x1, f(x2, f(x3, x4))) <-> foldr' f [x1..x4] ??
-- foldl f z [x1 .. x4] <-> f()

main :: IO()
main = do
    print (any isStefan ["Anika", "Boris", "Stefan"])
    print (all isStefan ["Anika", "Boris", "Stefan"]) 
    print (any isStefan []) -- any returns False
    print (all isStefan []) -- all returns True
    print (map makeStefan ["Anika", "Boris", "Stefan"])
    print (any isStefan (map makeStefan ["Anika", "Boris", "Stefan"]))
    print (all isStefan (map makeStefan ["Anika", "Boris", "Stefan"]))
    print (filter (not . isStefan) ["Anika", "Boris", "Stefan"])
    print (filter (any isStefan) [["Anika", "Boris", "Stefan"], ["Dimitar"]])
    print (filter (all (not . isStefan)) [["Anika", "Boris", "Stefan"], ["Dimitar"]])
    print (incrementAllBy [2..5] 4)
    print (incrementAllBy' [2..5] 4)
    print (splitByParity [1..10])
    print (isImage [1..3] [5..7]) 
    print (isImage [1..3] [3, 20, 1])
    print (zipWith (++) ["Anika", "Boris", "Stefan"] ["Anika", "Boris", "Stefan"])
    print (foldr (+) 0 [1..5])
    print (foldl (+) 0 [1..5])
    print (foldr (++) "" ["Anika", "Boris", "Stefan"])