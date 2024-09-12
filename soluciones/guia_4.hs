-- Ejercicio 1: Fibonacci
fibonacci::Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

{- 
La que sigue es una forma más eficiente que implementé.
La idea es aplicar la operación (a, b) -> (b, a+b) de forma iterada al par (0, 1)

 -}
fibonacci2::Integer -> Integer
fibonacci2 0 = 1
fibonacci2 n = b
    where 
        (a, b) = parFibonacci n
        parFibonacci::Integer -> (Integer, Integer)
        parFibonacci 1 = (0, 1)
        parFibonacci n = sucesorParFibonacci (parFibonacci (n-1))

        sucesorParFibonacci :: (Integer, Integer) -> (Integer, Integer)
        sucesorParFibonacci (i, j) = (j, i+j)




--Ejercicio 2: Parte Entera
parteEntera :: Float -> Integer
parteEntera x   | x < 1 = 0 
                | otherwise = 1 + parteEntera (x-1)




-- Ejercicio 3: Divisibilidad

esDivisible :: Integer -> Integer -> Bool
esDivisible n k = existeDivisionDesde 0 n k 
    where 
        existeDivisionDesde :: Integer -> Integer -> Integer -> Bool
        existeDivisionDesde i n k   | (n == k * i) = True
                                    | (n < k*i) = False
                                    | otherwise = existeDivisionDesde (i+1) n k




-- Ejercicio 4: Suma de números impares

sumaImpares :: Integer -> Integer
sumaImpares  1 = 1
sumaImpares n = sumaImpares(n-1) + n_esimoImpar
    where n_esimoImpar = 2*n - 1




-- Ejercicio 5: Medio factorial

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * (medioFact (n-2))




-- Ejercicio 6: Todos digitos iguales

primerDigito:: Integer -> Integer
primerDigito n = mod n 10

todosDigitosIgualesA :: Integer -> Integer -> Bool
todosDigitosIgualesA k n    | (not (primerDigito n == k)) = False
                            | (n < 10) = True
                            | otherwise = todosDigitosIgualesA k (div n 10)





-- Ejercicio 7: iésimo dígito y cantidad de dígitos

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n 1 = primerDigito n
iesimoDigito n i = iesimoDigito (div n 10) (i - 1)

cantDigitos::Integer -> Integer
cantDigitos 0 = 0
cantDigitos n = cantDigitos (div n 10) + 1





-- Ejercicio 8: Suma de dígitos

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = primerDigito n + sumaDigitos (div n 10)





-- Ejercicio 9: Números capicua

esCapicuaHasta :: Integer -> Integer -> Bool
esCapicuaHasta n 0 = True
esCapicuaHasta n 1 = True
esCapicuaHasta n i  | not (iesimoDigito n i == iesimoDigito n 1) = False
                    | otherwise = esCapicuaHasta (div n 10) (i - 2)

esCapicua :: Integer -> Bool
esCapicua n = esCapicuaHasta n (cantDigitos n)




-- Ejercicio 10: Sumas

f1 :: Integer -> Integer
f1 0 = 1
f1 n = 2^n + f1 (n-1)

f2 :: Integer -> Float -> Float
f2 0 q = 0
f2 n q = q ^ n + f2 (n-1) q

f3 :: Integer -> Float -> Float
f3 n q = f2 (2*n) q

f4 :: Integer -> Float -> Float
f4 n q = f3 n q - f2 (n-1) q




-- Ejercicio 11: Aproximación de e

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = 1/(fromIntegral (factorial n)) + eAprox(n-1)

e::Float
e = eAprox 10




-- Ejercicio 12: Fracción recursiva

fraccionRecursiva :: Integer -> Float
fraccionRecursiva 1 = 2
fraccionRecursiva n = 2 + 1/(fraccionRecursiva (n-1))

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = fraccionRecursiva n - 1




--Ejercicio 13: Suma doble

sumaDoble :: Integer -> Integer -> Integer
sumaDoble 0 m = 0
sumaDoble n m = sumaInteriorPitagoras n m + sumaDoble (n-1) m
    where
        sumaInteriorPitagoras :: Integer -> Integer -> Integer
        sumaInteriorPitagoras i 0 = 0
        sumaInteriorPitagoras i m = i^m + sumaInteriorPitagoras i (m-1)




-- Ejercicio 14: Suma de potencias

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 0 m = 0
sumaPotencias q n m = sumaInteriorPitagoras q n m + sumaPotencias q (n-1) m
    where
        sumaInteriorPitagoras :: Integer -> Integer -> Integer -> Integer
        sumaInteriorPitagoras q a 0 = 0
        sumaInteriorPitagoras q a m = q ^ (a + m) + sumaInteriorPitagoras q a (m-1)





-- Ejercicio 15: Suma de racionales

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 0 m = 0
sumaRacionales n m = sumaInteriorPitagorasRacionales n m + sumaRacionales (n-1) m
    where 
        sumaInteriorPitagorasRacionales :: Integer -> Integer -> Float
        sumaInteriorPitagorasRacionales p 0 = 0
        sumaInteriorPitagorasRacionales p m = (fromIntegral p)/(fromIntegral m) + sumaInteriorPitagorasRacionales p (m-1)




-- Ejercicio 16: Primos

menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorDesde 2 n
    where
        menorDivisorDesde i n   | (mod n i == 0) = i
                                | otherwise = menorDivisorDesde (i+1) n

esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && (menorDivisor n == n)

mcd :: Integer -> Integer -> Integer -- Máximo común divisor
mcd 0 b = b
mcd a 0 = a
mcd a b = mcd (mod b a) a

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1

primerPrimoDesde :: Integer -> Integer
primerPrimoDesde 1 = 2
primerPrimoDesde k  | (esPrimo k)   = k
                    | otherwise     = primerPrimoDesde (k+1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 1 = 2
nEsimoPrimo n = primerPrimoDesde (nEsimoPrimo (n-1) + 1)





-- Ejercicio 17: Es Fibonacci

--n es fibonacci desde el término k
esFibonacciDesde :: Integer -> Integer -> Bool --Recorremos la sucesión hasta que aparezca n o pasemos delante suyo
esFibonacciDesde n k    | (n == f_k) = True
                        | (f_k > n) = False --Si esto ocurre nos pasamos de n, por lo que no será fibonacci
                        | otherwise = esFibonacciDesde n (k+1)
                    where f_k = fibonacci k



esFibonacci :: Integer -> Bool
esFibonacci n = esFibonacciDesde n 0 





-- Ejercicio 18: 

esPar::Integer->Bool
esPar n = mod n 2 == 0

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n    | n < 10 = p
                    | otherwise = max p (mayorDigitoPar (div n 10))
                    where 
                        p   | esPar (primerDigito n) = primerDigito n
                            | otherwise = -1





-- Ejercicio 19: Suma de primos

sumaDePrimos :: Integer -> Integer
sumaDePrimos 0 = 0
sumaDePrimos m = nEsimoPrimo m + sumaDePrimos (m-1)

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde 1
    where
        esSumaInicialDePrimosDesde::Integer -> Bool
        esSumaInicialDePrimosDesde k
            | sumaPrimos < n    = esSumaInicialDePrimosDesde (k+1)
            | sumaPrimos == n   = True
            | otherwise         = False
            where
                sumaPrimos = sumaDePrimos k





-- Ejercicio 20

sumaDivisores::Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n
    where
        sumaDivisoresHasta::Integer -> Integer
        sumaDivisoresHasta 0 = 0
        sumaDivisoresHasta k    | mod n k == 0 = k + sumaDivisoresHasta (k-1)
                                | otherwise = sumaDivisoresHasta (k-1)

tomaValorMax :: Integer -> Integer -> Integer   -- Da el valor máximo de sumaDivisores(n) para a<=n<=b 
tomaValorMax a b    | (a == b) = sumaDivisores a --Suma de un elemento, no hace falta comparar nada
                    | otherwise = max (sumaDivisores b) (tomaValorMax a (b-1))




-- Ejercicio 21: Pitágoras

funcionPitagoras::Integer -> Integer -> Integer -> Integer
funcionPitagoras p q r
    | p ^ 2 + q ^ 2 <= r ^ 2    = 1
    | otherwise                 = 0


sumaInteriorPitagoras :: Integer -> Integer -> Integer -> Integer
sumaInteriorPitagoras p 0 r = funcionPitagoras p 0 r
sumaInteriorPitagoras p m r = funcionPitagoras p m r + sumaInteriorPitagoras p (m-1) r


pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras 0 m r = sumaInteriorPitagoras 0 m r
pitagoras n m r = sumaInteriorPitagoras n m r + pitagoras (n-1) m r







