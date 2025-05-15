-- Proyecto 2: Paradigma Funcional

-- Registro de usuarios

import System.IO
import System.Directory (doesFileExist) -- >> ghc -package directory RegistroUsuarios.hs --> .\RegistroUsuarios.exe
import Data.List
import Data.Char
import Data.Maybe

-- Funcion que registra un usuario y lo guarda en "archivos.txt"
registrarUsuario :: String -> String -> IO ()
registrarUsuario nombre contraseña = do
    let archivo = "usuarios.txt"
    existe <- doesFileExist archivo
    -- doesFileExist verifica si el archivo existe
    if existe
        then do
            usuarios <- leerUsuarios archivo
            -- llama a la función leerUsuarios para obtener la lista de usuarios y guardarla en la variable usuarios
            if any (\u -> nombre == head u) usuarios
                -- any verifica si el nombre ya existe en la lista de usuarios
                -- se utiliza una funcion lambda para comparar el nombre ingresado con el primer elemento de cada sublista
                then putStrLn "El usuario ya existe."
                else do
                    appendFile archivo (nombre ++ " " ++ contraseña ++ "\n")
                    -- appendFile agrega el nombre al final del archivo
                    putStrLn "Usuario registrado con éxito."
    else do
        writeFile archivo (nombre ++ " " ++ contraseña ++ "\n")
        putStrLn "Usuario registrado con exito."

-- Funcion que lee el archivo "usuarios.txt" y devuelve una lista de usuarios
leerUsuarios :: FilePath -> IO [[String]]
leerUsuarios archivo = do
    contenido <- readFile archivo
    -- readFile lee el contenido del archivo y lo guarda en la variable contenido
    let lineas = lines contenido
    -- lines separa el contenido en lineas
    return $ map words lineas
    -- map words aplica la funcion words a cada linea, separando las palabras


-- main :: IO ()
-- main = do
--    putStrLn "Registro de usuario"
--    putStrLn "===================="
--    putStrLn "Ingrese el nombre de usuario:"
--    nombre <- getLine
--    putStrLn "Ingrese la contraseña:"
--    contraseña <- getLine
--    registrarUsuario nombre contraseña