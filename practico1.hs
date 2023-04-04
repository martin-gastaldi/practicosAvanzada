
--Utilizando las funciones head y tail, y dada la lista “hola mundo”,
--obtenga el segundo elemento de la misma (la letra “o”).

palabra:: [Char] -> Int -> Char -- devuelve la letra segun su posicion
palabra cadena 0 = head cadena
palabra cadena n = palabra (tail cadena)(n-1) 

--palabra "hola" 2 = palabra ("ola")(1)
--palabra "ola" 1 = palabra ("la")(0)
--palabra "la" 0 = head("la") = l
------------------------------------------------------

--Utilizando las funciones head y reverse, y dada la lista “hola mundo”,
--obtenga el  ́ultimo elemento de la misma (la letra “o”).

palabra2:: [Char] -> Char
palabra2 cadena = head (reverse cadena)

--"hola" = head "aloh" = a

-----------------------------------------------------------------
--Utilizando la funcion realizada en el ejercicio anterior y la funcion mod
--determine si un numero, representado como la lista de sus digitos (ej: 123
-- = [1,2,3]) es par.

--parImpar :: (Eq a) => [a] -> String
--parImpar cadena
  --  | cadena2 == [2]   = "par"
    -- | otherwise        = "impar"
    --where cadena2 = head (reverse cadena2)

-------------------------------------
factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 1 = 1
factorial2 n = n * factorial2(n-1)

--

lucky :: [Int] -> String
lucky [7]= "E"
lucky [x] = "Lo siento,NO ES"

--

sumL :: (Num a) => [a] -> a
sumL [] = 0
sumL (x:xs) = x + sumL xs
--

-- 
digitoss :: Int -> [Int]
digitoss 0 = []
digitoss n = reverse (n `mod` 10 : reverse (digitoss (n `div` 10))) 
-- (n div 10) me va a dar como resultado todo el numero menos el ultimo, 891 -> 89
-- en cambio (mod n 10) me va a dar el ultimo. -> 1

-- cuando se llame a la funcion con un numero por ejemplo 891, 
-- reverse ( (1 se va a unir a la lista de: reverse (digitos 89) )   ->    1: [9,8] -> reverse [1,9,8] -> [8,9,1] (FINAL)
--                                                   (reverse (( 9 ) : reverse (digitos 8)      -> reverse ( 9 : [8]) -> reverse [9,8] ->   [8,9]              
--                                                             reverse ( 8 : reverse (digitos 0) )    ->  8:[] -> [8]
--                                                                                        []
--                                                             
   
--Utilizando las funciones reverse y == determine si una frase, representada como un string, es un palındromo.

frase1 :: String -> Bool
frase1 cadenaa 
   | cadenaa == n = True
   | otherwise = False
   where n = reverse cadenaa

--

mul :: (Num a) => [a] -> [a] 
mul xs = [x*x | x <- xs]     --(MULTIPLICA CADA TERMINO POR SI MISMO

mult :: [Int] -> Int  --(multiplica todos los elementos de la lista)
mult x = product x 


todosMenos1:: [a] -> [a] -- (puedo poner todosMenos1 "hola" y va a arrojar "ola" ya que lo considera como [Char] que es igual a String)
todosMenos1 (_:y) =  y

--Escriba una funcion que dada una lista de 3 numeros
--  determine si su suma es igual a su producto. 

esIgual :: [Int] -> String
esIgual x  
    | n == product x = "la suma es igual a su producto, la suma da: " ++ show n ++ " y el producto: "++ show (product x)
    | otherwise = "la suma no es igual al producto, las suma es igual a: " ++ show n ++ " y el producto: " ++ show (product x)
    where n = sum x
          
    --    3 = [1,1,1]   
--        n [1,2,3] = 1
       -- 3 == (3*3)

--Utilizando las funciones de los [ items 5, 6 ] determine si un n ́umero es multiplo de 6.

concatenarConComas :: [String] -> String
concatenarConComas lista = foldr (\x acc -> x ++ (if null acc then "" else ", " ++ acc)) "" lista

