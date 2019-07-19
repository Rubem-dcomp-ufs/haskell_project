import Data.List
import Data.Char

-- splitWords

takeWord :: String -> String
takeWord xs = takeWhile isLetter xs

dropWord :: String -> String
dropWord xs = dropWhile isLetter xs

dropSpace :: String -> String
dropSpace xs = dropWhile notLetter xs

notLetter:: Char -> Bool
notLetter x = not(isLetter x)

splitWords :: String -> [String]
splitWords st = split (dropSpace st)
    where
        split [ ] = [ ]
        split ss = takeWord ss : split (dropSpace (dropWord ss))

-- Funções iter1 e pot2
iter1 :: Int -> (a -> a) -> (a -> a)
iter1 n f = foldr (.) id fs
        where
            fs = map g [1..n]
            g _ = f


pot2 :: Int -> Int
pot2 n = (iter1 n dobro) 1
      where
          dobro x = 2 * x

--Funções twice e addoneall
twice1 :: (Int -> Int) -> (Int -> Int)
twice1 f = iter1 2 f

addOneAll :: [Int] -> [Int]
addOneAll xs = map (\n -> n + 1) xs

--Função doubleall

doubleAll :: [Int] -> [Int]
doubleAll xs = map (\n -> n * 2) xs

--Função length

length1 :: [a] -> Int
length1 xs = foldr (\_ n -> n+1) 0 xs

--Função mapFuns

mapFuns :: [a -> b] -> a -> [b]
mapFuns fs x = map (\f -> f x) fs
