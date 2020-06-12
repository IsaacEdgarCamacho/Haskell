{-
Dispersión
    Trabajamos con tres números que imaginamos como el nivel del río Paraná a
    la altura de Corrientes medido en tres días consecutivos; cada medición es
    un entero que representa una cantidad de cm.
    P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm.
    A partir de estos tres números, podemos obtener algunas conclusiones.
Definir estas funciones:
    a. dispersion​ , que toma los tres valores y devuelve la diferencia entre el más
    alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las
    versiones de dos elementos. De esa forma se puede definir dispersión sin
    escribir ninguna guarda (las guardas están en max y min, que estamos
    usando).
b. diasParejos​ , ​ diasLocos y ​ diasNormales reciben los valores de los
    tres días. Se dice que son días parejos si la dispersión es chica, que son días
    locos si la dispersión es grande, y que son días normales si no son ni parejos
    ni locos. Una dispersión se considera chica si es de menos de 30 cm, y
    grande si es de más de un metro.
    Nota​ : Definir ​ diasNormales a partir de las otras dos, no volver a hacer las
    cuentas.-}
minimo :: Float ->Float -> Float
minimo x y  | x < y     = x
            | otherwise = y

min3Param ::Float -> Float -> Float -> Float
min3Param x y z = minimo x (minimo y z)

maximo :: Float -> Float -> Float
maximo x y  | x < y     = y
            | otherwise = x

max3Param :: Float -> Float -> Float -> Float
max3Param x y z = maximo x (maximo y z)

dispersion :: Float -> Float -> Float -> Float
dispersion x y z = (max3Param x y z) - (min3Param x y z)


diasParejos :: Float -> Float -> Float -> Bool
diasParejos x y z = (dispersion x y z) < 0.30

diasLocos ::Float -> Float -> Float -> Bool
diasLocos x y z = (dispersion x y z) > 1

diasNormales :: Float -> Float -> Float -> Bool
diasNormales x y z = (dispersion x y z) >0.30 && (dispersion x y z) < 1 
