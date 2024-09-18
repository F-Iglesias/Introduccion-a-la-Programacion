
-- Ejercicio 1

longitud :: [t]->Integer -- Devuelve la cantidad de elementos de una lista
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

ultimo :: [t]->t -- Devuelve el último elemento de la lista dada
ultimo [x] = x
ultimo (x:xs) = ultimo xs

principio :: [t] -> [t] -- Devuelve la lista sin el último elemento. No está definida para listas vacías
principio [x] = []
principio(x:xs) = x:(principio xs) 

reverso :: [t] -> [t] -- Da como resultado la lista al revéz
reverso [] = []
reverso [x] = [x]
reverso xs = (ultimo xs):(reverso (principio xs))

