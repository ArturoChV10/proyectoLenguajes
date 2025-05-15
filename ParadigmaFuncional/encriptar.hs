import Data.Hashable

encriptar :: String -> String
--retorna el string hasheado
encriptar msj = show (hash msj) 

separar :: String -> (String, String)
--Separa el string cuando encuentra un ;
separar xs = let (a, b) = break (== ';') xs
                in (a, drop 1 b)

esValido :: String -> String -> FilePath -> IO Bool
-- Recibe un usuario y contraseña y verifica si son correctos -> True: Acceso habilitado
--                                                            -> False: Acceso inhabilitado
esValido "" _ _ = return False --Usuario vacio
esValido _ "" _ = return False --Contraseña vacia
esValido _ _ "" = return False -- Archivo vacio
esValido usuario contra archivo = do
    cuerpo <- readFile archivo
    let lineas = lines cuerpo
        lista = map separar lineas
        uHash = encriptar usuario
        cHash = encriptar contra
    return $ any (\(u, c) -> u == uHash && c == cHash) lista

--Ejemplos de como usar las funciones                           
--  print (encriptar "Hola")
--  valido <- esValido "Hola" "Hola" "usuarios.txt"
--  print valido