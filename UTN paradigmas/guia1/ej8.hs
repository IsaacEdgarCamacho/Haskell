{-

Definir la función ​ haceFrioF/1​ , indica si una temperatura expresada en grados
Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados
Celsius.

-}

-- Resolvemos con guardas
haceFrioF_v1:: Float -> Bool
haceFrioF_v1 x | x < 8 = True  
            | otherwise = False


-- Rasolvemos con if
haceFrioF_v2 :: Float -> Bool
haceFrioF_v2 x = if x < 8   then True 
                            else False


