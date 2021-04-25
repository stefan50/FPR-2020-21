type Student = (String, Double, Int)

test :: Student -> Bool
test _ = True

data MyBool = MyFalse | MyTrue

convertMyBoolToBool :: MyBool -> Bool 
convertMyBoolToBool MyFalse = False
convertMyBoolToBool MyTrue = True 

convertBoolToMyBool :: Bool -> MyBool
convertBoolToMyBool False = MyFalse
convertBoolToMyBool True = MyTrue 

type Radius = Double
type Height = Double 
type Width = Double 
type Length = Double
data Shape = Circle Radius | Rectangle Width Height | Triangle Length Length Length | Cylinder Radius Height -- deriving (Show)

identifyShape :: Shape -> String
identifyShape (Circle {}) = "Circle" -- Circle _ _
identifyShape (Rectangle {}) = "Rectangle" -- Rectangle _ _
identifyShape (Triangle {}) = "Triangle" -- Triangle _ _ _
identifyShape (Cylinder {}) = "Cylinder" -- Cylinder _ _

myShape :: Shape 
myShape = Circle 5.3

myShape2 :: Shape 
myShape2 = Rectangle 3.4 2.3


-- show :: (Show a) => a -> String
-- class Show a where 
instance Show Shape where
    show (Circle x) = "Circle with radius = " ++ show x
    show (Rectangle x y) = "Rectangle with width = " ++ show x ++ " and height = " ++ show y
    show (Triangle x y z) = "Triangle with one side = " ++ show x ++ " and side 2 = " ++ show y ++ " and side 3 = " ++ show z
    show (Cylinder x y) = "Cylinder with radius = " ++ show x ++ " and height = " ++ show y

{-
Typeclass definition

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-}

instance Eq Shape where 
    (Circle x) == (Circle y) = x == y
    (Rectangle x y) == (Rectangle a b) = x == a && y == b
    (Triangle x y z) == (Triangle a b c) = x == a && y == b && z == c 
    (Cylinder r h) == (Cylinder a b) = r == a && b == h
    _ == _ = False
    -- x /= y = not $ x == y   

-- (print x + y) => print $ x + y 
perimeter :: Shape -> Double
perimeter (Circle x) = 2 * 3.14 * x
perimeter (Rectangle x y) = 2 * (x + y)
perimeter (Triangle x y z) = x + y + z
perimeter (Cylinder x y) = 0

{- area :: Shape -> Double
area (Circle x) = 3.14 * (x ^ 2)
area (Rectangle x y) = x * y
area (Triangle x y z) = ... 
-}

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Cylinder _ _) = True
isRound _ = False

is2D :: Shape -> Bool
is2D (Cylinder _ _) = False
is2D _ = True

sumPerimeter :: [Shape] -> Double
sumPerimeter [] = 0
sumPerimeter (x:xs) = perimeter x + sumPerimeter xs

sumPerimeter' :: [Shape] -> Double
sumPerimeter' list = sum (map perimeter list)

sumPerimeter'' :: [Shape] -> Double
sumPerimeter'' list = foldr ((+) . perimeter) 0 list

sumPerimeter''' :: [Shape] -> Double
sumPerimeter''' list = sum (perimeter <$> list)

{- HW
biggestShape :: [Shape] -> Shape
biggestShape list = foldr (\ ) [(x, i) | x <- list, i <- (map perimeter list)] 
-}

{-
HW
Задача 4. Да се дефинира тип Point, който задава точка (в равнината или в
пространството) и е екземпляр на Show. Типа да се направи екземпляр на
класа Eq и за него да се дефинира равенство на точки от една и съща
размерност.
Задача 5. Да се дефинира функция distance за работа с типа Point, която
намира разстоянието между две точки от една и съща размерност. Ако точките
са с различна размерност (т.е. имат различен брой координати) функцията да
връща съобщение за грешка.
Задача 6. Да се дефинира функция getClosestPoint, която приема списък от
точки ps и още една точка p. Като резултат функцията да връща тази точка от
ps, която е най-близо до точката p.
-}

main :: IO()
main = do
    print "Hello world"
    print (sqrt 4 + 3 + 9)
    print (sqrt (4 + 3 + 9))
    print (sqrt $ 4 + 3 + 9)
    print $ 4 + 5
    print (test ("", 3.4, 0))
    print (convertMyBoolToBool (convertBoolToMyBool False))
    print (identifyShape myShape)
    print (identifyShape myShape2)
    print myShape
    print myShape2
    print (show myShape ++ " hhasa")
    print (myShape /= myShape2)
    print (filter isRound [myShape, myShape, myShape2])
    print (sumPerimeter'' [myShape, myShape, myShape2] == sumPerimeter [myShape, myShape, myShape2])
    print (sumPerimeter' [myShape, myShape, myShape2])