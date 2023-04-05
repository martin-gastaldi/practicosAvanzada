
--PRACTICA 3

-- 1 ) Define una funci ́on que, dadas dos listas ys y xs de naturales ordenadas,
--retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
--elementos de ys y xs.

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) 
   | x < y = x: merge xs (y:ys)
   | otherwise = y: merge (x:xs) ys


-- 2 )  Define una funcion que, dada una lista de naturales, la ordene.
ordLista :: (Ord a) => [a] -> [a]
ordLista [x] = [x] 
ordLista (x:y) = minimum (x:y): ordLista (delete1 (minimum (x:y))(x:y))  
    
delete1 :: Eq a => a -> [a] -> [a] -- Elimina 1 elemento x de la lista 
delete1 _ [] = []
delete1 n (y:ys) 
  | n == y = ys 
  | otherwise = y : delete1 n ys

-- 3 ) Define una funcion que, recursivamente y solo utilizando adicion y multiplicacion,
-- calcule, dado un natural n, el numero 2^n.
  
elevado :: Int -> Int
elevado 0 = 1
elevado 1 = 2
elevado n = 2*elevado(n-1)
  -- elevado 3: 2*elevado(2)
                  -- 2 * 2 (elevado 1)
                      -- 2 * 2 * 2 = 8       

--4 ) Define una funci ́on que, dado un n ́umero natural n, retorne su representaci ́on
--binaria como secuencia de bits.
binary :: Int -> [Int]
binary 0 = [] -- Caso base: si el número es 0, devolver la lista []
binary n = binary (n `div` 2) ++ [n `mod` 2]       

--5 ) nDefine una funci ́on que, dado un n ́umero natural n en su representaci ́on
--binaria, decida si n es par o no.
esPar :: [Int] -> Bool
esPar (x:xs) =  last (x:xs) == 0      
   

-- 6 ) distancia de Hamming: dadas dos listas es el numero de posiciones
-- en que los correspondientes elementos son distintos.
distanciaH :: (Eq a) => [a] -> [a] -> Int 
distanciaH [] [] = 0
distanciaH x  [] = 0
distanciaH [] y = 0
distanciaH (x:xs) (y:ys) = if x==y then 0 + distanciaH xs ys else 1 + distanciaH xs ys  


-- 7) Define la funcion que, dado un n ́umero natural, decida si el mismo es un
--cuadrado perfecto o no.

cuadradoPerf :: Integer -> Bool
cuadradoPerf x  -- 23
   | n == n*n = True  -- 25 == 5*5 = True
   | n >  n*n = False -- 25 > 
 where n=round (sqrt(fromIntegral x))  


-- 8 )Define la funcion repetidos de forma tal que dado un elemento x y un entero
--n; x aparece n veces.

repetidos :: (Eq a) => a -> Int -> [a]
repetidos x 0 = []
repetidos x 1 = [x]
repetidos x n = x : repetidos x (n-1)   


--  [] a 2 
repetidos2 :: (Eq a) => [a] -> a -> Int -> Bool  -- cuantas veces se repite un elemento en la lista 
repetidos2 [] elem 0 =  True
repetidos2 [] elem i = False  
repetidos2 (x:xs) elem i -- [m,a,c] a 1
  | x == elem = repetidos2 xs elem (i-1) -- [a , c ] a 1   -- si el elemento es igual se decrementa la variable i 
  | x /= elem = repetidos2 xs elem i     
  
        



 

-- 9 ) Define la funcion nelem tal que nelem xs n es elemento enesimo de xs,
--empezando a numerar desde el 0. Por ejemplo:
--nelem [1, 7, 2, 4, 9, 7]  2  −>   7
   --   nelem [7, 2, 4, 9, 7]  1
      --    nelem [2, 4, 9, 7] 0 

nelem :: (Eq a) => [a] -> Int -> a 
nelem (x:xs) 0 = x
nelem (x:xs) n = nelem xs (n-1) 
    
-- 10 )Define la funcion posicionesC tal que posicionesC xs c es la lista de la
--posiciones del caracter c en la cadena xs. Por ejemplo:
--posicionesC ”Catamarca” 'a'  -> [1, 3, 5, 8]






-- 11 )Define la funcion compact, dada una lista retorna la lista sin los elementos
--repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3]

compacto :: (Eq a) => [a] -> [a] 
compacto [] = []
compacto [x] = [x]
compacto (x:xs:y) = if x==xs then compacto (xs:y) else x: compacto(xs:y)



