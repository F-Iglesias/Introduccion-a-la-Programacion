
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

