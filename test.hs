isGoingRight :: Char -> Char -> Bool -- проверява дали поредица от 2 символа отговаря на плъх, движещ се надясно
isGoingRight x y
    | x == ')' && y == '1' = True
    | otherwise = False

isGoingLeft :: Char -> Char -> Bool -- проверява дали поредица от 2 символа отговаря на плъх, движещ се наляво
isGoingLeft x y
    | x == '1' && y == '(' = True
    | otherwise = False

thereIsHunter :: String -> Bool -- проверява дали в оставащата част от низа има ловец
thereIsHunter [] = False
thereIsHunter (x:xs) 
    | x == 'P' = True 
    | otherwise = thereIsHunter xs


countRats :: String -> Int
countRats (x:y:ys) = helper (x:y:ys) 0 -- както пише в условието, приемаме, че задължително ИМА ловец в низа
    where
        helper :: String -> Int -> Int -- намира броя на плъховете, движещи се в грешната посока
        helper (x:y:ys) count
           | isGoingLeft x y && thereIsHunter ys = helper (y:ys) (count + 1) 
           | isGoingLeft x y && not(thereIsHunter ys) = helper (y:ys) count  -- ако вече сме подминали ловеца
           | isGoingRight x y && thereIsHunter ys = helper (y:ys) count 
           | isGoingRight x y && not(thereIsHunter ys) = helper (y:ys) (count + 1)  -- ако вече сме подминали ловеца
           | otherwise = helper (y:ys) count
        helper (x:xs) count = count

main :: IO()
main = do
   print(countRats ")1)1)1)1P)1)11()1")