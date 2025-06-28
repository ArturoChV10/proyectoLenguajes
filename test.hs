concatenar :: [String] -> String
concatenar = foldr (++) ""

concatenar ["Hola", "mundo!"] -- "Hola mundo!"

minimo :: [Int] -> Int
minimo (x:xs) = foldl min x xs

minimo [2,4,6,8,10] -- 2

maximo :: [Int] -> Int
maximo (x:xs) = foldl max x xs

maximo [2,4,6,8,10] -- 10

-- listas infinitas
naturales = [1..]
primeros10 = take 10 naturales  -- solo se calculan los primeros 10

any even [1, 3, 4, 7]   -- True (porque 4 es par)

takeWhile (< 5) [1, 3, 4, 6, 2]   -- [1, 3, 4]

dropWhile (< 5) [1, 3, 4, 6, 2]   -- [6, 2]

data Arbol a = Vacio
             | Nodo a (Arbol a) (Arbol a)
             deriving (Show)

ejemplo :: Arbol Int
ejemplo = Nodo 5 (Nodo 3 Vacio Vacio) (Nodo 8 Vacio Vacio)

  5
 / \
3   8

m :: Map.Map String Int
m = Map.fromList [("a", 1), ("b", 2), ("c", 3)]

Map.lookup "b" m      -- Just 2
Map.insert "d" 4 m    -- Añade ("d", 4)
Map.delete "a" m      -- Elimina la clave "a"

map (\x -> x * 2) [1, 2, 3, 4] -- [2, 4, 6, 8]
map (\x -> x > 5) [3, 7, 1, 8] -- [False, True, False, True]

-- Funciones aplicados a listas
funcs :: [Int -> Int]
funcs = [\x -> x + 1, \x -> x * 2, \x -> x ^ 2]

aplicadas = map (\f -> f 3) funcs -- [4,6,9]

-- Funciones aplicados a tuplas
parFuncion :: (String, Int -> String)
parFuncion = ("doble", \x -> "Resultado: " ++ show (x * 2))

usarPar = snd parFuncion 5 -- 10

-- Funciones a estructuras personalizadas
data Procesador = Procesador {
    nombre :: String,
    operacion :: Int -> Int
}

cuadrado = Procesador "Cuadrado" (\x -> x * x)
aplicar = operacion cuadrado 4 -- 16

1 == 2        -- comparación de int
'a' == 'b'    -- comparación de char
