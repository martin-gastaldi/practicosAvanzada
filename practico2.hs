primerElemento :: [a] -> a  --(funcion head)
primerElemento (x:_) = x 

tl :: [a] -> [a] -- retorna todos menos el primero
tl (x:xs)= xs   

lastt :: [a] -> a
lastt x = head (reverse x)     

lastt2 :: [a] -> a --(muestra el ultimo)
lastt2 [x] = x
lastt2 (_:xs) = lastt2 xs

initt :: [a] -> [a] -- muestra todo menos el ultimo
initt [] = error "lista vacia"
initt [xs]= [] 
initt (x:xs) = x: initt xs
 
--2)
maxTres :: Int -> Int -> Int -> [Char]
maxTres x y z 
  | x > y && x > z = "maximo de los tres es: " ++ show x
  | y > x && y > z = "maximo de los tres es: " ++ show y
  | z > x && z > y = "maximo de los tres es: " ++ show z

  --concatenar

concatenarr :: (Num a) => [a] -> [a] -> [a]
concatenarr [] [] = [] -- en este caso no tiene sentido escribirlo
concatenarr xs [] = xs -- xs puede ser vacia
concatenarr [] ys = ys 
concatenarr (x:xs) ys = x : (concatenarr xs ys) -- el x:xs obliga que tenga al menos un elemento

take2 :: Int -> [a] -> [a] -- toma n numeros de una lista y los muestra
take2 0 (x:xs) = [] 
take2 1 (x:xs) = [x]
take2 n (x:xs) = x:take2(n-1)xs 

--
tirarr :: Int -> [a] -> [a]
tirarr 1 (x:xs) = xs
tirarr n (x:xs) = tirarr(n-1)xs    
--      
concatt :: [a] -> [a] -> [a]
concatt [x] [] = [x]


--dosPuntos :: a -> [a] -> [a]
--dosPuntos b x =  
 
abss :: Int -> Int
abss x 
  |  x > 0 = x
  |  x < 0 = x*(-1)  --Se debe escribir el parametro como (-2)
--


edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int  -- años que pasaron de diferencia 
edad (dia1,mes1,anio1)(dia2,mes2,anio2) 
   | dia1 == dia2 && mes1 == mes2  = anio2-anio1 --(01,01,2000)(01,01,2023)= 23
   | dia1 /= dia2 && mes1 == mes2  = (anio2-anio1)-1 --(03,01,2000)(01,01,2023)     = 22
   | dia1 /= dia2 && mes1 <= mes2  = anio2-anio1 --(03,01,2000)(01,02,2023) 
   | anio1 == anio2 = 0
   
  --si el mes1 es mayor que mes2 && año1 menor que año2 = (año2-año1)-1        --(03,02,2000)(01,01,2023) = 22
  -- si el mes1 es mayor que mes2 && año1 mayor que año2 = (año2-año1)*(-1)     -- (01,01,2022)(03,02,2023)= 

--La disyuncion excluyente xor de dos f ́ormulas se verifica si una es verdadera
--y la otra es falsa. Defina la funci ́on xor que calcule la disyunci ́on excluyente a
--partir de la tabla de verdad.

xor :: Bool -> Bool -> Bool
xor x y 
 | x == True && y==False = True
 | x == False && y==True = True
 | x == True && y==True = False
 | x == False && y==False = False


--Ahora defina la funcion xor2 que calcule la disyuncion excluyente pero sin
--que considere todos los posibles valores de las entradas. Cu ́al sera la diferencias
--entre ambas definiciones?
 
xor2 :: Bool -> Bool -> Bool
xor2 x y 
 | x == y = False
 | otherwise = True 

-- si no abarca todas las formas que da False
-- podria dar True en cualquiera caso que tendria que dar False, 
-- por ejemplo: en caso que las dos sean verdaderas deberia dar falso
-- y da verdadero
------------------------------------------------------------------------------

factoresDe :: Int -> [Int]
factoresDe n = [x | x <- [1..n], mod n x  == 0 ]  -- Numeros que al dividirlos por n dan resto cero
-- factoresDe 8 = [1,2,4,8]
-- factoresDe 5 = [1,5]

esPrimo :: Int -> Bool
esPrimo x = factoresDe x == [1,x] -- esto es igual a:  if factoresDe x == [1,x] then True else False
-- si es verdadera la comparacion es True

--Defina una funcion que dado un numero natural n, retorne la lista de todos
--los numeros naturales primos menores que n.

primosMenores :: Int -> [Int]
primosMenores x = [x | x <- [2..x], esPrimo x]

--10. Defina una funcion que dada una lista, retorne la reversa de la misma.

reversaLista :: [a] -> [a] 
reversaLista (x:xs) = reversaLista xs ++ [x] 

-- Defina una funcion que dadas dos listas, decida si las listas son iguales.

sonIguales ::(Eq a) => [a] -> [a] -> Bool
sonIguales x y 
    | x == y = True 
    | otherwise = False 

esPalindromo :: [Char] -> Bool
esPalindromo lista 
  | lista == reverse lista = True
  | otherwise= False 
  
esPalindromo2 :: Eq a => [a] -> Bool
esPalindromo2 [] = True
esPalindromo2 [_] = True
esPalindromo2 (x:xs) = (x == last xs) && esPalindromo2 (init xs) -- last (ultimo), init (todos menos el ultimo)

--esPalindromo "neuquen" -> ( n == (n) ) y esPalindromo2 (euque)
--esPalindromo "euque" -> (e == (e) y esPalindromo2 (uqu)
--esPalindromo "uqu" -> (u == last "qu" (u) y esPalindromo2 (q)
--esPalindromo "q" -> (q == last "q" (u) y esPalindromo2 (q)


--Defina una funcion que dados tres numeros a, b, c devuelva la cantidad de 
--raıces reales de la ecuacion ax2 + bx + c = 0

cantRaiz :: Double -> Double -> Double -> Int
cantRaiz a b c 
  | discriminante > 0 = 2
  | discriminante < 0 = 0
  | discriminante == 0 = 1 
  where discriminante = b^2 - 4*a*c

largo :: [Int] -> Int
largo [x]= 1 
largo (x:xs) = 1+largo xs   

sumaLis :: [Int] -> Int  -- suma los 3 primeros numeros de una lista
sumaLis (x:y:z:_) =  x+y+z 
 
-- otra manera: -- para sumar los primeros 4 numeros
sumaLos4 :: [Int] -> Int
sumaLos4 x = sum (take 4 x)

--
maximoo :: [Int] -> Int
maximoo (x:xs) = maximum (x:xs)  
--
--minimo :: (Ord a) => [a] -> a
--minimo (x:xs) = minimum(x:xs)
------------------------------------------------------------------------------------------------
--PRACTICA 3

-- 1 ) Define una funci ́on que, dadas dos listas ys y xs de naturales ordenadas,
--retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
--elementos de ys y xs.

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


-- 7) Define la funci ́on que, dado un n ́umero natural, decida si el mismo es un
--cuadrado perfecto o no.

--cuadradoPerf :: Float -> Bool
--cuadradoPerf x  -- 25
   -- | n == n*n True  -- 25 == 5*5 = True
   -- | n > n*n = False -- 25 > 
   --where n = sqrt x 


-- 8 )Define la funcion repetidos de forma tal que dado un elemento x y un entero
--n; x aparece n veces.

repetidos :: (Eq a) => a -> Int -> [a]
repetidos x 0 = []
repetidos x 1 = [x]
repetidos x n = x : repetidos x (n-1)    
 

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
--posicionesC ”Ca” 'a'  -> [1, 3, 5, 8]






-- 11 )Define la funcion compact, dada una lista retorna la lista sin los elementos
--repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3]

compacto :: (Eq a) => [a] -> [a] 
compacto [] = []
compacto [x] = [x]
compacto (x:xs:y) = if x==xs then compacto (xs:y) else x: compacto(xs:y)













--[x] [y] 
  -- | x == y = [1]
  -- | x /= y = [] 
 -- | (x:xs) (y:ys)
--anda con listas del mismo tamaño
---sonIgual :: (Eq a) => [a] -> [a] -> Int
--sonIgual (x:xs) (y:ys)  
   -- |  (x:xs) == (y:ys)= 0
    -- |  xs == [] && ys == [] = 1
    -- |  x /= y  = 1 + sonIgual (delete1 x (x:xs)) (delete1 y (y:ys)) 
    -- |  x == y  = 0 + sonIgual (delete1 x(x:xs)) (delete1 y (y:ys))

    -- |  xs == [] && ys == [] = 1
    -- |  xs /= [] && ys == [] = 1
    -- |  xs == [] && ys /= [] = 1
    --




