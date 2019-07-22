{-
Todos os exercícios nos slides "aula15-16-17 FuncoesComoArgumentos.pdf" e no arquivo "aula18-19 AltaOrdem.hs"
-}
import Data.Char
import Data.List


-- Qual o tipo mais geral de fiter?
-- Resp.: filter :: (a -> Bool)->[a] -> [a]

{-2ºResolva os problemas 1 e 4 da
primeira prova sem usar nem
compreensões e nem recursão
-}

separaDigitos :: String -> (String, String)
separaDigitos [] = ([],[])
separaDigitos xs = (filter isDigit xs, filter notDigit xs)

notDigit :: Char -> Bool
notDigit c = not (isDigit c)

-- Defina length usando map e sum
length' :: Num a => [a] -> Int
length' [] = 0
length' xs = sum (map f xs)
        where
            f x = 1 

-- Qual é o efeito de: 
-- map addOne (map addOne ns) ?
--   |                |-> retorna uma lista com cada
--   |                    elemento adicionado uma unidade.
--   |-> retorna uma lista com os elementos da 1a lista
--       retornada acima, acrescida novamente de uma unidade.
--       conclui-se então que, pode-se usar o resultado de uma 
--       função como argumento de outra, in line.

-- Defina funções que tomem uma lista, ns, e: 
{-
 .retorne a lista consistindo dos quadrados dos inteiros em
  ns: 
-}
quadInt :: Integral a => [a] -> [a]
quadInt [] = []
quadInt ns = map f ns
       where
           f x = x^2

-- .retorne a soma dos quadrados dos itens em ns:
somaQuad :: Integral a => [a] -> a
somaQuad ns = foldr (+) 0 (map (\x -> x^2) ns)

-- .verifique se todos os itens da lista são maiores que zero:
maiorQZero :: [Int] -> Bool
maiorQZero ns = and( map g ns )
            where
                g x = if x > 0 then True else False

-- Defina funções para:
-- 1. calcular o menor valor de uma função f aplicada de 0 até n
menorDe :: Int -> Int
menorDe 0 = 0
menorDe n = minimum  $ map m [1..n]
       where
         m x = x  
-- 2. verificar se os valores de f aplicados de 0 até n são todos
--    iguais:
-- Com recursão: 
seIguais ::Eq a => [a] -> [a]
seIguais (x:y:xs)
    | x==y = x:y:seIguais (y:xs)
    | otherwise = seIguais (y:xs)
           
-- Alta ordem:
seIguais':: Int ->(Int -> Int)-> Bool 
seIguais' n f = and $ map (\x -> x==(f 0))(map f [0..n])

-- 3. verificar se todos os valores de f aplicados de 0 até n são
--    maiores que 0
seMaiorQzero:: Int -> (Int -> Int)-> Bool 
seMaiorQzero n f = and $ map (\x -> x>(f 0))(map f [0..n])

-- 4. verificar se os valores de f aplicados de 0 até n estão em
--    ordem crescente
seCrescente::Int -> Bool 
seCrescente n = and (map g ls)
              where
                 ls = map f [0..n]
                 f x = x
                 g x = if (succ x) >= x then True else False

{-
p ["Penso", "logo", "existo"] [',', ' ', '.'] =
"Penso,logo existo."
-}
p :: [String] -> [Char] -> String
p xss ys = foldr (++) []  $ map (\(x,y)->x++[y]) dupla 
       where 
          dupla = zip xss ys

{-
Estabeleça o tipo e defina uma função twice que aceita
uma função e um valor e aplica esta função duas vezes.
Por exemplo, a função twice aplicada as entradas
double e a 7 produzirá 28 como resultado.

● Defina o tipo e defina a função iter tal que
iter n f x = f (f (f … (f x)…))
onde f ocorre n vezes no lado direito da equação. Por
exemplo, deveríamos ter que
iter 3 f x = f (f (f x)))
● Usando iter e double defina uma função a qual para a
entrada n retorna 2^n
-}

iter :: Int -> (a -> a) -> (a -> a)
iter n f = foldr (.) id fs
    where
    	fs = map g [1..n]
    	g _ = f

twice :: (Int -> Int) -> (Int -> Int)
twice f = iter1 2 f

{-Calcule a soma dos quadrados dos números naturais 1 até n usando
map e foldr
● Defina uma função que dê a soma dos quadrados dos inteiros positivos
de uma lista de inteiros -}
somaQ :: Integral a => a -> a
somaQ n = foldr (+) 0 (map (\x -> x^2) [1..n])

-- ● Usando foldr defina as funções unzip, last e init
unzip'::[(a,b)]->([a], [b])
unzip'  = foldr f ([],[]) 
      where
          f (x,y)(xs,ys) = (x:xs, y:ys)

last' :: [a] -> a
last' [] = error "lista vazia"
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "lista vazia"
init' [x] = []
init' (x:xs) = x:init' xs
{-
O que calcula a seguinte função?:
  misterio xs = foldr (++) [] (map sing xs)
           where sing x = [x] 
Resp.: misterio cria uma string de Char, elementos da lista "xs"
-}
misterio::[Char] -> String
misterio xs = foldr (++) [] (map sing xs)
       where 
           sing x = [x] 

{-
Defina uma função:
filterFirst :: (a -> Bool) -> [a] -> [a]
tal que filterFirst p xs remova o primeiro elemento de xs que não
satisfaz a propriedade p.
-}
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p xs = dropWhile p xs
             where
                 p x = False

{-
Defina:
filterLast :: (a -> Bool) -> [a] -> [a]
que remove a última ocorrência de um elemento de uma lista que não
satisfaz a propriedade.
-}
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = takeWhile p xs
             where
                 p x = True


