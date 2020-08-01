-- CONJUNTOS
--Definamos un renombre de tipos para conjuntos: type Set a = [a]

type Set a = [a]

--Definir vacio :: Set Integer que represente el conjunto vacı́o

vacio :: Set Integer -> Bool
vacio a = a == []

-- implementar entre todos la función
-- agregar :: Integer -> Set Integer -> Set Integer
-- que dado un entero y un conjunto agrega el primero al segundo (ayuda: La función
-- “pertenece” en Haskell existe y se llama “elem”)

agregar :: Integer -> Set Integer -> Set Integer
agregar x conj 
        | not (x `elem` conj)     = x : conj
        | otherwise             = conj

-- Implementar una función
-- incluido :: Set Integer -> Set Integer -> Bool que determina si el primer conjunto
-- está incluido en el segundo.

incluido :: Set Integer -> Set Integer -> Bool
incluido [] _   = True
incluido a  b   = (head a `elem` b) && (incluido (tail a) b)
       


-- Implementar una función
-- iguales :: Set Integer -> Set Integer -> Bool que determina si dos conjuntos son
-- iguales.
-- Ejemplo> iguales [1,2,3,4,5] [2,3,1,4,5]
-- True

iguales :: Set Integer -> Set Integer -> Bool
iguales a b = (incluido a b) && (incluido b a)



-- Implementar una función
-- agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer) que dado un
-- conjunto de enteros y un conjunto de conjunto de enteros agrega el primero al segundo.
-- Ejemplo> agregarC [1,2] [[4,5],[2,3,1],[2,1]]
-- [[4,5],[2,3,1],[2,1]

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
agregarC a c = a : c


-- Implementar la función
-- agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer) que dado un
-- número n y un conjunto de conjuntos cls agrega a n en cada conjunto de cls.

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos x c 
                | length c == 0         = agregarC (agregar x []) []
                | length c == 1         = agregarC (agregar x (head c)) tail c
                | otherwise             = agregarATodos x (tail c)

-- Implementar una función
-- partes :: Integer -> Set (Set Integer) que genere todos los subconjuntos del
-- conjunto {1, 2, 3, . . . , n}.
-- Ejemplo> partes 2
-- [[], [1], [2], [1, 2]]

-- partes :: Integer -> Set (Set Integer)
-- partes 





















