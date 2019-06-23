import Data.Char
{--
Todos os exercícios dos slides "aula08 RecursoPrimitiva" e "aula10 RecursaoGeral".
Adicionalmente, os exercícios do livro do 7.8 a 7.26, exceto os exercícios 7.11, 7.15, 7.21, 7.22 e 7.23.
14 questoes do livro +  9 da aula8 + 16 da aula10 = 39
--}
-- Exercicio 8.1
multDois::Float->Float->Float
multDois a b
  | (a ==0) || (b == 0) = 0
  | otherwise = a + multDois a (b-1)
-- =====================================
-- Exercicio 8.2
-- Funcao que receba n e devola 2ˆn
potDois::Int->Int
potDois n
  | n == 0 = 1
  | otherwise = 2*potDois(n-1)
-- =====================================
-- Exercicio 8.3
-- Recebe m e n devolve mˆn
potMN::Int->Int->Int
potMN m n
  | n == 0 && m == 0 = 0
  | n == 0 = 1
  | m == 0 = 0
  | otherwise = m*potMN m (n-1)
-- =====================================
-- Exercicio 8.4
-- Escreva uma função que dado n, calcule: 0! + 1! + 2! + ... + n!
somaFatorial::Int->Int
somaFatorial n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n + 1 + somaFatorial (n-1)
-- =====================================
-- Exercicio 8.5
-- Escreva uma função que calcule 2ˆ0 + 2ˆ1 + 2ˆ2 + ... + 2ˆn
somaPot::Int->Int
somaPot n
  | n == 0 = 1
  | otherwise = potDois n + somaPot (n-1)
-- =====================================
-- Exercicio 8.6
{--
f :: Int -> Int
f m
  | m == 0 = 8
  | m == 1 = 44
  | m == 2 = 17
  | otherwise = 0
 --}
 {--
 f :: Int -> Int
 f m
   | m == 0 = 8
   | m == 1 = 44
   | m == 2 = 17
   | m > maiorF m = m
   | otherwise = 0

maiorF :: Int -> Int
maiorF x
  | x == 0 = 0
  | x == 1 = 1
  | x == 2 = 2
  | otherwise =  --}
-- ====================================
{-- 8.7
Dada um função f de Int em Int, defina por recursão primitiva
uma função algumF0 que aceite um natural n e devolva o booleano
True se e somente se um ou mais valores de f 0, f 1, ..., f n é zero.
Teste com diferentes definições de f.
--}
algumF0 :: Int -> Bool
algumF0 n
  | n == 0 = True
  | otherwise = AlgumF0 n-1
-- ====================================
-- 8.8
{--
Dada um função f de Int em Bool, defina por recursão primitiva
uma função algumFentre que aceite um natural n e devolva o booleano
True se e somente se f i é True para algum i entre 0 e n.
Teste com diferentes definições de f.
--}

algumFentre ::Int -> Bool
algumFentre i
  | i >= 0 && i<= n = True
  | otherwise = False

-- =====================================
-- 8.9
{--
Defina por recursão primitiva uma função que calcule a raiz
quadrada inteira de n (o maior natural cujo quadrado é menor ou
igual a n)
--}
raizQI :: Int -> Int
raizQI 0 = 0
raizQI n
  | (r+1)^2 == n = r + 1
  | otherwise = r
  where
    r = raizQI ( n-1 )
-- =====================================
-- 8.10
{--
Usando recursão primitiva sobre listas (não pode
usar compreensões), defina funções para
1– O produto dos elementos de uma lista de inteiros
2– Filtrar (eliminar) os números pares, ou seja, ficar somente
com os ímpares
3– Verificar se um string é formado somente por caracteres
alfanuméricos (letras e numerais). Use a função
isAlphaNum :: Char -> Bool
da biblioteca Data.Char
4– Eliminar a primeira ocorrência de um dado elemento, se
ele ocorrer, senão retornar a lista original
5– Eliminar todas as ocorrências de um dado elemento
6– Inverter um string
--}
-- 1-
prodInt :: [Int] -> Int
(x:xs) == [] = 0
prodInt x:xs
  | otherwise = x*prodInt(xs)
-- =====
-- 2-
filtrarPares :: [Int]->[Int]
filtrarPares x:xs
  | (x:xs)==[] = []
  | otherwise = acrescImpares x:xs
          where acrescImpares x:xs = (x `mod` 2) /= 0 = x:filtrarPares xs
-- =====
-- 3-
verAlfaNum :: String -> Bool

verAlfaNum s:st
  | st == [] = False
  | isAlphaNum s == elem s s:st = True
  | otherwise = verAlfaNum st
  
 -- =====
 -- 4-
eliminar :: Int ->[Int]->[Int]
eliminar _ [] = []
eliminar n (m:ms)
    | n == m = eliminar n ms
	| otherwise = (m:ms)
-- =====
-- 5-
eliminarT :: Int -> [Int] -> [Int]
eliminarT _ [] = []
eliminar n (m:ms)
  | n == m = elimnarT m ms
  | otherwise = eliminar n ms
-- =====
-- 6-
inverte :: String -> String
inverte [] = []
inverte x:xs = inverte xs ++ [x]
-- ===============================================
{-- 8.11
A função or aplica o operador ou lógico || a todos os elementos
de uma lista. Por exemplo:
or [False, True, False]
↝  False || True || False ↝  … ↝  True
Dê uma definição recursiva para a função or.
--}
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs
-- ================================================
-- Do Livro: 7.8
elemNum :: Integer -> [Integer] -> Integer
elemNum _ []     = 0
elemNum a (x:xs)
    | a == x    = 1 + elemNum a xs
    | otherwise = elemNum a xs

testElemNum = TestList
    [ TestCase (assertEqual "" 0 (elemNum 5 []))
    , TestCase (assertEqual "" 0 (elemNum 5 [1,2,3]))
    ]
-- ================================================
-- do Livro: 7.9
unique :: [Integer] -> [Integer]
unique ls    = [x | x <- ls, elemNum x ls == 1]

unique' :: [Integer] -> [Integer]
unique' []          = []
unique' (x : xs)
    | elemNum x (x : xs) == 1    = x : unique' xs
    | otherwise                  = unique' (deleteAll x xs)
  where
    deleteAll :: Integer -> [Integer] -> [Integer]
    deleteAll target ls          = [_x | _x <- ls, _x /= target]
-- ================================================


  
  
  
  