type Tuple = (Int, Int)
tuple :: Int -> Int -> Tuple
tuple x y = (x, y)

size = 9

str8ts = [-10,-10,0,0,5,0,0,-3,-10,
          0,6,0,0,-10,-10,1,0,0,
          0,0,0,-10,-8,0,0,0,0,
          9,0,0,-4,0,0,0,-10,-5,
          -10,0,0,0,0,3,0,0,-10,
          -10,-10,0,0,0,-9,0,4,0,
          4,0,3,0,-10,-10,0,6,0,
          0,0,1,-10,-10,0,0,0,0,
          -10,-10,8,0,0,0,0,-10,-2] 

getX :: Tuple -> Int
getX (x, _) = x 

getY :: Tuple -> Int 
getY (_, y) = y 

coordinate :: Int -> Tuple 
coordinate i = ((i `div` size), (i -size*(i `div` size)))

index :: Tuple -> Int
index t = size*(getX t) + (getY t)

mdl :: Int -> Int
mdl n | n < 0 = -1*n
      |otherwise = n

indexAt :: [Int] -> Int -> Int -> Int
indexAt board value i | board!!i == value = 0
                      | otherwise = 1 + indexAt board value (i+1)   

boardSet :: [Int] -> [Int] -> Tuple -> Int -> Int -> [Int]
boardSet _ _ _ _ 81 = []
boardSet board (x:xs) t value i |i == (index t) = value : boardSet board xs t value (i+1)
                                |otherwise = x : boardSet board xs t value (i+1)

findEmpty :: [Int] -> [Int] -> Tuple
findEmpty _ [] = (-1,-1)
findEmpty board (x: xs) |(x == 0) = coordinate (indexAt board x 0)
                        |otherwise = findEmpty board xs

isValid :: [Int] -> Tuple -> Int -> Bool
isValid board t value = (rowValid board t value) && (colValid board t value) && (str8Valid board t value)

checkList :: [Int] -> Int -> Bool
checkList [] _ = False
checkList (x : xs) value |((mdl x) == value) = True
                        |otherwise = (checkList xs value)

getRow :: [Int] -> Tuple -> Int -> [Int]
getRow board t 9 = []
getRow board t i = (board!!(index ((getX t), i)) : getRow board t (i+1))

rowValid :: [Int] -> Tuple -> Int -> Bool
rowValid board t value = not (checkList (getRow board t 0) value)

getCol :: [Int] -> Tuple -> Int -> [Int]
getCol board t 9 = []
getCol board t i = (board!!(index(i,(getY t))) : getCol board t (i+1))

colValid :: [Int] -> Tuple -> Int -> Bool
colValid board t value = not (checkList (getCol board t 0) value)

getStrRow :: [Int] -> Tuple -> [Int]
getStrRow board t = ((getLower (getRow board t 0) t (getY t))++(getUpper (getRow board t 0) t (getY t) (getY t) ))

getStrCol :: [Int] -> Tuple -> [Int]
getStrCol board t = ((getLower (getCol board t 0) t (getX t))++(getUpper (getCol board t 0) t (getX t) (getX t) ))

getLower :: [Int] -> Tuple -> Int -> [Int]
getLower _ _ (-1) = []
getLower vector t i |(vector!!i < 0) = []
                    |otherwise = (vector!!i : (getLower vector t (i-1)))

getUpper :: [Int] -> Tuple -> Int -> Int -> [Int]
getUpper _ _ 9 _ = []
getUpper vector t i cond  |(vector!!i < 0) = []
                          |(i == cond) = (getUpper vector t (i+1) cond)
                          |otherwise = (vector!!i : (getUpper vector t (i+1) cond))

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = [y | y <- xs, y <= x] ++ (x : [y | y <- xs, y > x])

checkSequence :: [Int] -> Bool
checkSequence [] = True
checkSequence (a: (b: c))  |a + 1 /= b = False
                           | otherwise = checkSequence (b: c)  
   
str8Valid :: [Int] -> Tuple -> Int -> Bool
str8Valid board t value = (sValid (getStrRow board t) t value) && (sValid (getStrCol board t) t value)

sValid :: [Int] -> Tuple -> Int -> Bool
sValid board t value | (checkList (boardSet board board t value 0) 0) = True
                     | otherwise = checkSequence (quicksort(board))

backtrack :: [Int] -> Int -> Tuple -> [Int]
backtrack board 10 t = board
backtrack board value t | (isValid board t value) && (getX (findEmpty (solve (boardSet (board) (board) t value 0)) (solve (boardSet (board) (board) t value 0))) == -1 ) = (boardSet (board) (board) t value 0)
                        | otherwise = backtrack board (value+1) t

solve :: [Int] -> [Int]
solve board |(getX (findEmpty board board)== -1) = board
            |otherwise = backtrack board 1 (findEmpty board board)


main = do
   print (solve str8ts)
