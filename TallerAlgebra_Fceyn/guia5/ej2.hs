-- unidades: dado un entero, devuelve el dı́gito de las unidades del número (el dı́gito menos
-- significativo).

unidades :: Integer->Integer
unidades n = mod n 10

-- sumaUnidades3: dados 3 enteros, devuelve la suma de los dı́gitos de las unidades de los 3
-- números.

sumaUnidades3::Integer->Integer->Integer->Integer
sumaUnidades3 x y z = unidades x + unidades y + unidades z


-- todosImpares: dados 3 números enteros determina si son todos impares.
todosImpares ::Integer->Integer->Integer->Bool
todosImpares x y z = (mod x 2 /= 0) && (mod y 2 /= 0) && (mod z 2 /= 0)

todosImpares2 ::Integer->Integer->Integer->Bool
todosImpares2 x y z = (mod (unidades x) 2 /= 0) && (mod (unidades y) 2 /= 0) && (mod (unidades z) 2 /= 0)

-- alMenosUnImpar: dados 3 números enteros determina si al menos uno de ellos es impar.
alMenosUnImpar::Integer->Integer->Integer->Bool
alMenosUnImpar x y z = (mod x 2 /= 0) || (mod y 2 /= 0) || (mod z 2 /= 0)


-- alMenosDosImpares: dados 3 números enteros determina si al menos dos de ellos son
-- impares.
impares ::Integer->Integer->Bool -- determina si dos numero son impares
impares x y =  (mod x 2 /= 0) && (mod y 2 /= 0)

alMenosDosImpares::Integer->Integer->Integer->Bool
alMenosDosImpares x y z = impares x y || impares x z || impares y  z


-- alMenosDosPares: dados 3 números enteros determina si al menos dos de ellos son pares.
pares::Integer->Integer->Bool
pares x y = mod x 2 ==0 && mod y 2 == 0


alMenosDosPares::Integer->Integer->Integer->Bool
alMenosDosPares x y z = pares x y || pares x z || pares y z