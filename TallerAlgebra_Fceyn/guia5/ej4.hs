-- Definir la función listar :: a -> a -> a -> [a] que toma 3 elementos y los convierte
-- en una lista.

listar :: a -> a -> a -> [a]
listar x y z = [x,y,z]

-- Definir la función pertence :: a -> [a] -> Bool que indica si el elemento que se le pasa
-- está o no en la lista.

pertenece ::  Integer -> [Integer] -> Bool
pertenece x lista
        | length lista == 0     = False
        | otherwise             = (x == (head lista)) || pertenece x (tail lista)