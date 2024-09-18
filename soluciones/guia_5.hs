
-- Ejercicio 1

-- 1.1 
-- Devuelve la cantidad de elementos de una lista
longitud :: [t]->Integer 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 1.2
-- Devuelve el último elemento de la lista dada
ultimo :: [t]->t 
ultimo [x] = x
ultimo (x:xs) = ultimo xs


-- 1.3
-- Devuelve la lista sin el último elemento. No está definida para listas vacías
principio :: [t] -> [t] 
principio [x] = []
principio(x:xs) = x:(principio xs) 


-- 1.4 
-- Da como resultado la lista al revéz
reverso :: [t] -> [t] 
reverso [] = []
reverso [x] = [x]
reverso xs = (ultimo xs):(reverso (principio xs))



-- Ejercicio 2

-- 2.1
-- Devuelve True si y solo si el elemento pertenece a la lista
pertenece :: Eq t => t -> [t] -> Bool   
pertenece x []      = False
pertenece x (y:ys)
    | x == y        = True
    | otherwise     = pertenece x ys

-- 2.2 
todosIgualesA :: Eq t => [t] -> t -> Bool   
todosIgualesA [] y  = True
todosIgualesA (x:xs) y
    | x == y        = todosIgualesA xs y
    | otherwise     = False

-- Devuelve True si y solo si todos los elementos de la lista son iguales
todosIguales :: Eq t => [t] -> Bool 
todosIguales []     = True
todosIguales (x:xs) = todosIgualesA xs x

-- 2.3
-- Devuelve Talse si y solo si existen dos posiciones distintas de s con igual valor
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos []   = True 
todosDistintos [x]  = True
todosDistintos (x:xs)
    | pertenece x xs    = False
    | otherwise         = todosDistintos xs


-- 2.4
-- Devuelve True si y solo si existen dos posiciones distintas de s con igual valor
hayRepetidos :: (Eq t) => [t] -> Bool 
hayRepetidos xs = not (todosDistintos xs)


-- 2.5
-- Dados un entero x y una lista xs, elimina la primera aparición de x en la lista xs 
quitar :: (Eq t) => t -> [t] -> [t]
quitar x []         = []
quitar x (y:ys)
    | (x == y)      = ys
    | otherwise     = y:(quitar x ys) 

-- 2.6
-- Dados un entero x y una lista xs, elimina todas las apariciones de x en la lista xs
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x []    = []
quitarTodos x (y:ys)
    | (x==y)        = quitarTodos x ys
    | otherwise     = y : (quitarTodos x ys)

-- 2.7
-- Deja en la lista una única aparición de cada elemento, eliminando las repeticiones adicionales.
-- En esta implementación, siempre queda la primer aparición de cada elemento
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : (eliminarRepetidos (quitarTodos x xs)) -- Eliminamos todas las repeticiones posteriores de x y aplicamos recursión sobre el tail.

-- 2.8
{- dadas dos listas devuelve verdadero si y solamente si
ambas listas contienen los mismos elementos, sin tener en cuenta repeticiones. -}

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
{-
Si hay un elemento de xs que no se haya en ys, las listas no tienen los mismos elementos, retornamos False.
De otro modo, tras eliminar de ys todos los elementos de xs, la lista resultante será vacía
si y solo si xs e ys contienen los mismos elementos 
 -}
mismosElementos [] ys           = (ys == [])
mismosElementos (x:xs) ys
    | not (pertenece x ys)      = False 
    | otherwise                 = mismosElementos (quitarTodos x xs) (quitarTodos x ys)  

  
-- 2.9 
capicua :: (Eq t) => [t] -> Bool -- Devuelve True si y solo si la lista es capicua
capicua l = (l == reverso l)




-- Ejercicio 3

-- 3.1
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x: xs) = x + sumatoria xs

-- 3.2 
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- 3.3
maximo :: [Integer] -> Integer
maximo [x]      = x
maximo (x:xs)   = max x (maximo xs)

-- 3.4
-- sumarN n xs retorna la lista obtenida tras sumarle n a cada posición de xs
sumarN :: (Num t) => t -> [t] -> [t]
sumarN n [] = []
sumarN n (x:xs) = (x + n) : (sumarN n xs)

-- 3.5
-- Le suma a cada posición de la lista su primer elemento.
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- 3.6
-- Le suma a cada posición de la lista su último elemento.
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo xs = sumarN (ultimo xs) xs 

-- 3.7
-- Solo deja los elementos pares de la lista dada
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
    | (mod x 2 == 0)    = x:(pares xs) 
    | otherwise         = pares xs


-- 3.8
minimo :: [Integer] -> Integer
minimo [x]      = x
minimo (x:xs)   = min x (minimo xs)

-- Devuelve la lista ordenada en orden ascendente
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = min_xs : (ordenar xs_2) -- Movemos el minimo al inicio y ordenamos lo restante
    where
        min_xs = minimo xs
        xs_2 = quitarTodos min_xs xs -- Le quita todas las repeticiones de min_xs a xs


-- Ejercicio 5
-- 5.1
{- sumaAcumulada xs retorna una lista ys de misma longitud tal que,
en casa posición i de ys, su valor es igual a la suma de los valores de xs en todas las posiciones anteriores a i-}
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada (x:xs) = x : (sumarN x (sumaAcumulada xs))

-- 5.2

-- Para todo n > 1, devuelve el primer divisor distinto de 1
primerDivisor :: Integer -> Integer
primerDivisor n = primerDivisorDesde n 2
    where
        primerDivisorDesde n k
            | mod n k == 0      = k
            | otherwise         = primerDivisorDesde n (k+1)

-- Retorna una lista de primos cuyo producto es el numero de entrada
descomposicionPrima :: Integer -> [Integer]
descomposicionPrima 1 = []
descomposicionPrima n = d : descomposicionPrima (div n d)
    where d = primerDivisor n --Notemos como el primerDivisor n necesariamente es primo


-- Devuelve una lista de mismo largo que la de entrada, correspondiendo a la descomposicion prima de cada elemento
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = (descomposicionPrima x) : (descomponerEnPrimos xs)