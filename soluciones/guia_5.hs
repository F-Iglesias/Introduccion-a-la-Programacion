
-- Ejercicio 1

-- 1.1 
longitud :: [t]->Integer -- Devuelve la cantidad de elementos de una lista
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 1.2
ultimo :: [t]->t -- Devuelve el último elemento de la lista dada
ultimo [x] = x
ultimo (x:xs) = ultimo xs


-- 1.3
principio :: [t] -> [t] -- Devuelve la lista sin el último elemento. No está definida para listas vacías
principio [x] = []
principio(x:xs) = x:(principio xs) 


-- 1.4 
reverso :: [t] -> [t] -- Da como resultado la lista al revéz
reverso [] = []
reverso [x] = [x]
reverso xs = (ultimo xs):(reverso (principio xs))

