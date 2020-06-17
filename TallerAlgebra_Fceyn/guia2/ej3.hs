--Escribir una funcion que sume dos vectores de R2 con y sin tuplas
sumaVectores1:: (Float,Float)->(Float,Float)->(Float,Float)
sumaVectores1 v w = (fst v + fst w , snd v + snd w)

{-Notar que no se puede hacer una funcion que me devuelva dos parametros
sino es con tuplas 
sumaVectores2 :: Float -> Float -> Float -> Float -> Float -> Float
sumaVectores2 a b c d = 
-}

--Hacer funciones que retornen la norma de un vector de R2
--con tuplas y con numeros sueltos

normaNum:: Float-> Float->Float
normaNum x y = sqrt ( x*x + y*y)

normaVector1:: (Float,Float)-> Float
normaVector1 v = sqrt( (fst v) * (fst v) + (snd v)*(snd v) )

--Ahora hacer una funcion que retorne la nora de la suma de dos 
--vectores usando las dos funciones anteriores
normaSuma1::(Float, Float)->(Float, Float)-> Float
normaSuma1 v w = normaVector1 (sumaVectores1 v w)

