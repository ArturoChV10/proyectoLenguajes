-- Proyecto 2: Paradigma Funcional

-- Registro de usuarios

import System.IO
import System.Directory (doesFileExist) -- >> ghc -package directory RegistroUsuarios.hs --> .\RegistroUsuarios.exe
import Text.Read (readMaybe)

-- Funcion que registra un usuario y lo guarda en "archivos.txt"
registrarUsuario :: IO ()
registrarUsuario = do
    putStrLn "Ingrese el nombre de usuario:"
    nombre <- getLine
    if nombre == "SALIR"
        then putStrLn "Se cancelo el registro del usuario."
        else do
            pin <- pedirPIN
            -- llama a la funcion pedirPIN para obtener el PIN
            case pin of
                 Nothing -> putStrLn "Se cancelo el registro del usuario."
                 Just pin_validado -> do
                    let archivo = "usuarios.txt"
                    existe <- doesFileExist archivo
                    -- doesFileExist verifica si el archivo existe
                    let pin_String = show pin_validado
                    -- convierte el PIN a String
                    if existe
                        then do
                            usuarios <- leerUsuarios archivo
                            -- llama a la función leerUsuarios para obtener la lista de usuarios y guardarla en la variable usuarios
                            if any (\u -> nombre == head u) usuarios
                                -- any verifica si el nombre ya existe en la lista de usuarios
                                -- se utiliza una funcion lambda para comparar el nombre ingresado con el primer elemento de cada sublista
                                then putStrLn "El usuario ya existe."
                                else do
                                    appendFile archivo (nombre ++ " " ++ pin_String ++ "\n")
                                    -- appendFile agrega el nombre al final del archivo
                                    putStrLn "Usuario registrado con éxito."
                    else do
                        writeFile archivo (nombre ++ " " ++ pin_String ++ "\n")

-- Funcion que pide el PIN al usuario y lo valida
pedirPIN :: IO (Maybe Int)
-- se pone maybe porque puede ser un int o cancelar el registro
pedirPIN = do
    putStrLn "Ingrese el PIN o escriba SALIR para cancelar el registro de usuario:"
    pin <- getLine
    if pin == "SALIR"
        then return Nothing
        -- nothing se devuelve si el usuario quiere cancelar el registro
        else case readMaybe pin :: Maybe Int of
            -- readMaybe intenta convertir el string a un int
            Just pin_validado -> return (Just pin_validado)
            -- si la conversion es exitosa, devuelve el pin con Just (caso valido donde el valor es del tipo que se puso en el maybe)
            Nothing -> do
                -- si la conversion falla, devuelve un mensaje de error
                putStrLn "----------------------"
                putStrLn "El PIN debe ser un número. Intente de nuevo."
                putStrLn "----------------------"
                pedirPIN

-- Funcion que lee el archivo "usuarios.txt" y devuelve una lista de usuarios
leerUsuarios :: FilePath -> IO [[String]]
leerUsuarios archivo = do
    contenido <- readFile archivo
    -- readFile lee el contenido del archivo y lo guarda en la variable contenido
    let lineas = lines contenido
    -- lines separa el contenido en lineas
    return $ map words lineas
    -- map words aplica la funcion words a cada linea, separando las palabras

login :: IO (Maybe String)
login = do
    putStrLn "Ingrese su nombre de usuario:"
    nombre <- getLine
    if nombre == "SALIR"
        then return Nothing
        else do
            usuarios <- leerUsuarios "usuarios.txt"
            -- llama a la funcion leerUsuarios para obtener la lista de usuarios y guardarla en la variable usuarios
            if any (\u -> nombre == head u) usuarios
                -- any verifica si el nombre ya existe en la lista de usuarios
                -- se utiliza una funcion lambda para comparar el nombre ingresado con el primer elemento de cada sublista
                then return (Just nombre)
                else do
                    putStrLn "El usuario no existe."
                    login

--main :: IO ()
--main = do
--    putStrLn "Registro de usuario"
--    putStrLn "===================="
--    registrarUsuario
