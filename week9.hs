data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

sampleTree :: BTree Int
sampleTree = Node 56 (Node 32 (Node 23 Empty Empty) (Node 9 Empty Empty)) (Node 24 (Node 13 Empty Empty) (Node 9 Empty Empty))

sampleTree2 :: BTree Int
sampleTree2 = Node 4 (Node 2 (Node 3 Empty Empty) Empty) (Node 20 Empty Empty)

size :: BTree a -> Int
size (Empty) = 0
size (Node _ x y) = 1 + size x + size y

height :: BTree a -> Int
height (Empty) = 0
height (Node _ x y) = 1 + (max (height x) (height y))

sumTree :: (Num a) => BTree a -> a
sumTree (Empty) = 0
sumTree (Node num x y) = num + sumTree x + sumTree y 

sumLeaves :: (Num a) => BTree a -> a
sumLeaves (Empty) = 0
sumLeaves (Node num Empty Empty) = num
sumLeaves (Node _ x y) = sumLeaves x + sumLeaves y 

instance Eq a => Eq (BTree a) where
    Empty == Empty                     = True
    Empty == _                         = False
    Node num1 x1 y1 == Node num2 x2 y2 = num1 == num2 && x1 == x2 && y1 == y2   

main :: IO()
main = do
    print (sampleTree)
    print (size sampleTree)
    print (height sampleTree)
    print (height sampleTree2)
    print (sumTree sampleTree2)
    print (sumLeaves sampleTree2)
    print (sampleTree == sampleTree)