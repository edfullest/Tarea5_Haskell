--Juan Ordaz A01191576
--Eduardo Serna A01196007
-- Tarea 5

import Data.List

--1
mediana :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
mediana a b c d e = (sort [a,b,c,d,e]) !! 2

--2
columna :: Integer -> Integer -> String -> String
columna j m line 
        | j == m = line
        | j < m = (columna (j + 1)  m  (line ++ " " ++ (show j)))

renglon :: Integer -> Integer -> Integer -> Integer -> String -> IO()
renglon renglones columnas inicio i lineas 
        | i == renglones = putStr(lineas)
        | i < renglones = 
                            (renglon renglones columnas inicio (i + 1)
                                (lineas ++ (columna (i * columnas + inicio) (i * columnas + inicio + columnas) []) 
                                        ++ "\n")
                            )
                        
tabla :: Integer -> Integer -> Integer -> IO()
tabla renglones columnas inicio = renglon renglones columnas inicio 0 []

--3
hazPar :: Integer -> Integer -> (Integer,Integer)
hazPar a b 
        | a < b = (b `div` a, (mod b a) )
        | a > b = (a `div` b , (mod a b)  )
        | a == b = (1,0)

divide :: [Integer] -> [Integer] -> [(Integer,Integer)]
divide [] [] = []
divide (x:xs) (y:ys) = [(hazPar x y)] ++ (divide xs ys)  

--6
data AB t = A (AB t) t (AB t) | V deriving Show
ab = A (A (A V 2 V) 5 (A V 7 V)) 8 (A V 9 (A (A V 11 V) 15 V))

nivel_aux :: (Eq e) => AB e -> e -> Integer -> Integer
nivel_aux V _ _ = -1
nivel_aux (A izq root der) valor niv  
                                    | root == valor = niv
                                    | nivelIzq >= 0 = nivelIzq
                                    | nivelDer >= 0 = nivelDer
                                    | otherwise = -1
                            where
                                nivelIzq = (nivel_aux izq valor (niv + 1) )
                                nivelDer = (nivel_aux der valor (niv + 1) )



nivel ::(Eq e) => AB e -> e -> Integer
nivel ab valor = nivel_aux ab valor 0

--7
rango :: AB Integer -> Integer -> Integer -> [Integer]
rango V _ _  = []
rango (A izq root der) lowerBound upperBound  
                                    | root >= lowerBound && root <= upperBound = listaIzq ++ [root] ++ listaDer
                                    | otherwise = listaIzq ++ listaDer
                            where
                                listaIzq = (rango izq lowerBound upperBound )
                                listaDer = (rango der lowerBound upperBound )


--10
takeLastN :: Integer -> [Integer] -> [Integer]
takeLastN n lista = (reverse (take (fromIntegral n) (reverse lista)))

-- Esta funcion da el numero de elementos a la derecha de un elemento. Una precondicion importante es que el numero que se recibe no se repite antes de él                                        
numElementsToRight :: Integer -> [Integer] -> Integer
numElementsToRight num lista = 
                    let index = elemIndex num lista
                    in case index of
                        Just x -> fromIntegral ((length lista) - (x + 1))
                        Nothing -> 0


reduce_aux :: [Integer] -> [[Integer]]
reduce_aux lista = foldl (\x y -> x ++ [[y] ++ (takeLastN (numElementsToRight y (drop (length x) lista)) (drop (length x) lista) )] ) [] lista -- x en este caso hace referencia al [] del foldl, y a las acumulaciones consecuentes

reducir :: [Integer] -> [[Integer]]
reducir lista = (reduce_aux lista) ++ [[]]






