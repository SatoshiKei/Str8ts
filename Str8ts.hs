--Tipo de dados Tupla que armazena dois inteiros (coordenadas x e y)
type Tuple = (Int, Int)
tuple :: Int -> Int -> Tuple
tuple x y = (x, y)

--Constante que representa o comprimento e a largura da matriz de entrada
size = 9

--Matriz de entrada utilizada no teste, as casas brancas são representadas pelos números positivos e as pretas por números negativos. Isto facilita a avaliação
--e tira a necessidade de utilizar duas matrizes. Podemos representar casas pretas não valoradas com algum digito negativo qualquer, neste caso -10
str8ts = [-10,-10,0,0,5,0,0,-3,-10,
          0,6,0,0,-10,-10,1,0,0,
          0,0,0,-10,-8,0,0,0,0,
          9,0,0,-4,0,0,0,-10,-5,
          -10,0,0,0,0,3,0,0,-10,
          -10,-10,0,0,0,-9,0,4,0,
          4,0,3,0,-10,-10,0,6,0,
          0,0,1,-10,-10,0,0,0,0,
          -10,-10,8,0,0,0,0,-10,-2] 

--Recebe um par ordenado e retorna o valor de x
getX :: Tuple -> Int
getX (x, _) = x 

--Recebe um par ordenado e retorna o valor de y
getY :: Tuple -> Int 
getY (_, y) = y 

--Recebe um inteiro que representa o índice da lista que armazena a matriz e faz as operações que identificam dado um size qualquer com qual coordenada a nossa matriz
--imaginária está associada
coordinate :: Int -> Tuple 
coordinate i = ((i `div` size), (i -size*(i `div` size)))

--Operação reversa, recebendo (x,y) e retornando o índice de busca
index :: Tuple -> Int
index t = size*(getX t) + (getY t)

--Função Módulo, será útil na validação, uma vez que para um número qualquer n (n == -n) não é verdade, precisamos comparar os módulos
--pois pouco importa se as casas são pretas ou brancas de qualquer modo os valores não podem se repetir
mdl :: Int -> Int
mdl n | n < 0 = -1*n
      |otherwise = n

--Dado um valor qualquer encontra o primeiro índice na matriz que armazena aquele valor, será útil para encontrar um espaço vazio
indexAt :: [Int] -> Int -> Int -> Int
indexAt board value i | board!!i == value = 0
                      | otherwise = 1 + indexAt board value (i+1)   

--Modifica um valor da matriz em um índice especificado
boardSet :: [Int] -> [Int] -> Tuple -> Int -> Int -> [Int]
boardSet _ _ _ _ 81 = []
boardSet board (x:xs) t value i |i == (index t) = value : boardSet board xs t value (i+1)
                                |otherwise = x : boardSet board xs t value (i+1)

--Retorna a primeira Tupla de coordenada cujo valor armazenado é 0, caso não haja retorna (-1,-1)
findEmpty :: [Int] -> [Int] -> Tuple
findEmpty _ [] = (-1,-1)
findEmpty board (x: xs) |(x == 0) = coordinate (indexAt board x 0)
                        |otherwise = findEmpty board xs

--Chamas funções auxiliares para determinar se a inserção de um valor em uma coordenada é válida
isValid :: [Int] -> Tuple -> Int -> Bool
isValid board t value = (rowValid board t value) && (colValid board t value) && (str8Valid board t value)

--Retorna se um valor pertence ou não a uma lista
checkList :: [Int] -> Int -> Bool
checkList [] _ = False
checkList (x : xs) value |((mdl x) == value) = True
                        |otherwise = (checkList xs value)

--Retorna a linha da matriz cuja coordenada (x, y) esteja situada
getRow :: [Int] -> Tuple -> Int -> [Int]
getRow board t 9 = []
getRow board t i = (board!!(index ((getX t), i)) : getRow board t (i+1))

--Função auxiliar que retorna True caso a inserção seja válida na linha
rowValid :: [Int] -> Tuple -> Int -> Bool
rowValid board t value = not (checkList (getRow board t 0) value)

--Retorna a coluna da matriz cuja coordenada (x, y) esteja situada
getCol :: [Int] -> Tuple -> Int -> [Int]
getCol board t 9 = []
getCol board t i = (board!!(index(i,(getY t))) : getCol board t (i+1))

--Função auxiliar que retorna True caso a inserção seja válida na coluna
colValid :: [Int] -> Tuple -> Int -> Bool
colValid board t value = not (checkList (getCol board t 0) value)

--Retorna o straight (sequência de números não negativos) em que a coordenada (x, y) esteja situada, sendo uma sublista da linha
getStrRow :: [Int] -> Tuple -> [Int]
getStrRow board t = ((getLower (getRow board t 0) t (getY t))++(getUpper (getRow board t 0) t (getY t) (getY t) ))

--Retorna o straight (sequência de números não negativos) en que a coordenada (x, y) esteja situada, sendo uma sublista da coluna
getStrCol :: [Int] -> Tuple -> [Int]
getStrCol board t = ((getLower (getCol board t 0) t (getX t))++(getUpper (getCol board t 0) t (getX t) (getX t) ))

--Função auxiliar que dado uma coordenada encontra a lista de elementos inferiores não negativos 
getLower :: [Int] -> Tuple -> Int -> [Int]
getLower _ _ (-1) = []
getLower vector t i |(vector!!i < 0) = []
                    |otherwise = (vector!!i : (getLower vector t (i-1)))

--Função auxiliar que dado uma coordenada encontra a lista de elementos superiores não negativos
getUpper :: [Int] -> Tuple -> Int -> Int -> [Int]
getUpper _ _ 9 _ = []
getUpper vector t i cond  |(vector!!i < 0) = []
                          |(i == cond) = (getUpper vector t (i+1) cond)
                          |otherwise = (vector!!i : (getUpper vector t (i+1) cond))

--Algoritmo simples de ordenamento de lista, serve para posteriormente verificar se as sublistas straight formam uma progressão aritimética de ordem 1
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = [y | y <- xs, y <= x] ++ (x : [y | y <- xs, y > x])

--Verifica se uma lista é uma progressão aritimética de ordem 1, ou seja a condição das sequências (straights)
checkSequence :: [Int] -> Bool
checkSequence [] = True
checkSequence (a: (b: c))  |a + 1 /= b = False
                           | otherwise = checkSequence (b: c)  

--Função principal que consegue a sub-linha e sub-coluna de straight para um ponto (x, y) e retorna se a insersção de um valor de entrada é legítima 
str8Valid :: [Int] -> Tuple -> Int -> Bool
str8Valid board t value = (sValid (getStrRow board t) t value) && (sValid (getStrCol board t) t value)

--Função auxiliar que apenas checa a validade do straight quando não há mais nenhum 0 naquele straight, ou seja todos os números já foram inseridos naquela sequência
sValid :: [Int] -> Tuple -> Int -> Bool
sValid board t value | (checkList (boardSet board board t value 0) 0) = True
                     | otherwise = checkSequence (quicksort(board))

--Função auxiliar da função resolver que contém o algorítimo recursivo
--Recebe um board, um valor e uma coordenada e tenta inserir o valor na coordenada começando pelo 1
--Verifica se a inserção é válida e em caso positivo, altera a matriz e chama a função solve
backtrack :: [Int] -> Int -> Tuple -> [Int]
backtrack board 10 t = board
backtrack board value t | (isValid board t value) && (getX (findEmpty (solve (boardSet (board) (board) t value 0)) (solve (boardSet (board) (board) t value 0))) == -1 ) = (solve (boardSet (board) (board) t value 0))
                        | otherwise = backtrack board (value+1) t
--Esta função dada uma matriz incompleta, resolve-a
--Caso não hajam mais espaços vazios (indicado por findEmpty) retorna o próprio argumento
--Em todos os outros casos faz uma inserção por backtrack começando em 1 que irá recursivamente testar todas as possibilidades
solve :: [Int] -> [Int]
solve board |(getX (findEmpty board board)== -1) = board
            |otherwise = backtrack board 1 (findEmpty board board)


main = do
   print (solve str8ts)
