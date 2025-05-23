module PlantillaXRegistro (login, registrarUsuario, agregarArchivo, eliminarServicio, consultarServicio) where

-- Proyecto 2: Paradigma Funcional

-- Registro de usuarios

import System.IO
import System.Directory (doesFileExist, removeFile, renameFile) -- >> ghc -package directory RegistroUsuarios.hs --> .\RegistroUsuarios.exe
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Encriptar (encriptar, desencriptar, separar, separar3, esValido)
import Control.Exception (evaluate)


-- Funcion que registra un usuario y lo guarda en "archivos.txt"
registrarUsuario :: IO ()
registrarUsuario = do
    putStrLn "Ingrese el nombre de usuario:"
    hFlush stdout
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
                            usuarios <- leerUsuarios pin_validado archivo
                            -- llama a la función leerUsuarios para obtener la lista de usuarios y guardarla en la variable usuarios
                            if any (\u -> nombre == head u) usuarios
                                -- any verifica si el nombre ya existe en la lista de usuarios
                                -- se utiliza una funcion lambda para comparar el nombre ingresado con el primer elemento de cada sublista
                                then putStrLn "El usuario ya existe."
                                else do
                                    appendFile archivo (encriptar nombre pin_validado ++ ";" ++ encriptar pin_String pin_validado ++ "\n")
                                    -- appendFile agrega el nombre al final del archivo
                                    putStrLn "Usuario registrado con éxito."
                    else do
                        writeFile archivo (encriptar nombre pin_validado ++ ";" ++ encriptar pin_String pin_validado ++ "\n")
                        putStrLn "Usuario registrado con éxito. \n"

-- Funcion que pide el PIN al usuario y lo valida
pedirPIN :: IO (Maybe Int)
-- se pone maybe porque puede ser un int o cancelar el registro
pedirPIN = do
    putStrLn "Ingrese el PIN o escriba SALIR para cancelar la operacion:"
    hFlush stdout
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

-- Función encargada de tomar una cadena de texto y separar donde haya ";"
separarPorPuntoYComa :: String -> [String]
separarPorPuntoYComa [] = [""]
separarPorPuntoYComa (x:xs)
    | x == ';'  = "" : separarPorPuntoYComa xs
    | otherwise = (x : head resto) : tail resto
  where
    resto = separarPorPuntoYComa xs

-- Funcion que lee el archivo "usuarios.txt" y devuelve una lista de usuarios
leerUsuarios :: Int -> FilePath -> IO [[String]]
leerUsuarios pin archivo = do
    contenido <- readFile archivo
    evaluate (length contenido)
    -- readFile lee el contenido del archivo y lo guarda en la variable contenido
    let lineas = lines contenido
    -- lines separa el contenido en lineas
    return $ map (\linea ->
        let [u, c] = separarPorPuntoYComa linea
        in [desencriptar u pin, desencriptar c pin]
      ) lineas
    -- map words aplica la funcion words a cada linea, separando las palabras

login :: IO (Maybe (String, Int))
login = do
    putStrLn "Ingrese su nombre de usuario:"
    hFlush stdout
    nombre <- getLine
    existe <- doesFileExist "usuarios.txt"
    if not existe
        then do
            putStrLn "Usuarios no encontrados, cree un nuevo usuario"
            registrarUsuario
            login
            else do
        if nombre == "SALIR"
            then return Nothing
            else do
                pin <- pedirPIN
                case pin of
                    Nothing -> do
                        putStrLn "Se cancelo el inicio de sesion."
                        return Nothing
                    Just pin_validado -> do
                        valido <- esValido nombre (show pin_validado) pin_validado "usuarios.txt"
                        if valido
                            then do 
                                return (Just (nombre, pin_validado))
                            else do 
                                putStrLn "Usuario o contraseña incorrecta. Intente nuevamente"
                                login
                        -- usuarios <- leerUsuarios pin_validado "usuarios.txt"
                        -- -- llama a la funcion leerUsuarios para obtener la lista de usuarios y guardarla en la variable usuarios
                        -- let usuario_ingresado = filter (\u -> nombre == head u) usuarios
                        --     -- se utiliza una funcion lambda para comparar el nombre ingresado con el primer elemento de cada sublista
                        -- if not (null usuario_ingresado)
                        --     then do
                        --         let pin_guardado = (usuario_ingresado !! 0) !! 1
                        --             -- se obtiene el segundo elemento de la sublista que contiene el nombre y el PIN
                        --             -- !! 0 obtiene la primera sublista y !! 1 obtiene el segundo elemento de esa sublista
                        --         if show pin_validado == pin_guardado
                        --             -- si el PIN ingresado es igual al guardado, se devuelve el nombre de usuario
                        --             then do
                        --                 putStrLn "Usuario ingresado con éxito."
                        --                 return (Just nombre)
                        --             else do
                        --                 putStrLn "PIN incorrecto. Intente nuevamente."
                        --                 login
                        -- else do
                        --     putStrLn "El usuario no existe."
                        --     login

main :: IO ()
main = do
    putStrLn "\n=== Bienvenido ==="
    putStrLn "Seleccione la acción a realizar"
    putStrLn "1. Registrar usuario"
    putStrLn "2. Ingresar usuario existente"
    putStrLn "3. Salir"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    -- Opcion para crear un nuevo usuario
    case opcion of
        "1" -> do
            registrarUsuario
            main

    -- Opcion para ingresar a cuenta de usuario
        "2" -> do
            -- login -- Por ahora no voy a implementar el login debido a que tengo que hacer validaciones de contraseña y me da pereza
            putStrLn "Ingrese su nombre de usuario:"
            --nombre <- getLine
            --loopGestion nombre

    -- Opcion para salir del programa
        "3" -> do
            putStrLn "Saliendo del programa..."

    -- Caso donde no se escoja una de las opciones
        _ -> do
            putStrLn "Opción no válida, intente nuevamente"
            main


agregarArchivo :: String -> String -> String -> Handle -> Int -> IO ()
agregarArchivo username serviceName password archivo pin = do
  hPutStrLn archivo ((encriptar serviceName pin) ++ ";" ++ (encriptar username pin) ++ ";" ++ (encriptar password pin))

consultarServicio :: String -> String -> Handle -> Int -> IO ()
consultarServicio username serviceName archivo pin = do
    completo <- hIsEOF archivo
    if completo
        then do
            putStrLn "No se encontró el servicio"
            return ()
        else do
            lineaEnc <- hGetLine archivo
            let camposEnc = splitOn ";" lineaEnc           -- primero separar la línea en campos cifrados
                campos = map (`desencriptar` pin) camposEnc  -- luego desencriptar cada campo
                coincide = case campos of
                    [servicio, usuario, _] -> usuario == username && servicio == serviceName
                    _ -> False
            if coincide
                then do
                    case campos of
                        -- Se imprime con el formato de tabla
                        [sitio, usuario, contrasena] -> do
                            putStrLn $ "Información de " ++ sitio
                            putStrLn "| Sitio Web | Usuario | Contraseña |"
                            putStrLn $ "| " ++ sitio ++ " | " ++ usuario ++ " | " ++ contrasena ++ " |"
                            putStrLn "------------------------------------"
                else consultarServicio username serviceName archivo pin


eliminarServicio :: String -> String -> Handle -> Handle -> Int -> IO ()
eliminarServicio username serviceName oldFile newFile pin = do
  completo <- hIsEOF oldFile -- Comprueba si oldFile llegó a su final (h is End Of File)
  if completo
    then return ()
    else do
        linea <- hGetLine oldFile -- Pone un cursor sobre la primera línea, la obtiene y pasa el cursor a la siguiente línea
        let (servEnc, userEnc, passEnc) = separar3 linea -- separa una linea en una tupla (String, String, String) buscando ";"
            servicio = desencriptar servEnc pin
            usuario  = desencriptar userEnc pin
        if usuario == username && servicio == serviceName
            then eliminarServicio username serviceName oldFile newFile pin
            else do
                hPutStrLn newFile linea
                eliminarServicio username serviceName oldFile newFile pin


--loopGestion :: String -> IO ()
--loopGestion nombreUsuario = do
--    putStrLn "\n=== Gestión de Contraseñas ==="
--    putStrLn "Seleccione la acción a realizar"
--    putStrLn "1. Agregar cuenta"
--    putStrLn "2. Consultar cuenta"
--    putStrLn "3. Modificar cuenta"
--    putStrLn "4. Eliminar cuenta"
--    putStrLn "5. Regresar"
--    putStr "Seleccione una opción: "
--    hFlush stdout
--    opcion <- getLine

    -- Opcion para agregar un nuevo usuario
--    case opcion of
--        "1" -> do
--            putStrLn "Nombre del nuevo servicio: "
--            service <- getLine
--            case service of
--                "" -> do -- Podría usar _ -> do, pero esto generaba problemas con el flujo
--                    putStrLn "Ingrese un nombre de servicio válido"
--                    loopGestion nombreUsuario
--            putStrLn "Contraseña: "
--            password <- getLine
--            case password of
--                "" -> do
--                    putStrLn "Ingrese una contraseña de servicio válido"
--                    loopGestion nombreUsuario
--            archivo <- withFile "servicios.txt" AppendMode
--            agregarArchivo nombreUsuario service password archivo
--            hClose archivo
--            loopGestion nombreUsuario

    -- Opcion para consultar servicio de usuario
--        "2" -> do
--            putStr "Nombre del servicio a consultar: "
--            nombre <- getLine
--            case nombre of
--                "" -> do
--                    putStrLn "Ingrese un nombre de servicio válido"
--                    loopGestion nombreUsuario
--            archivo <- withFile "servicios.txt" AppendMode
--            consultarServicio nombre archivo
--            hClose archivo
--            loopGestion nombreUsuario

    -- Opción para modificar los datos de un servicio
--        "3" -> do
--            putStrLn "Nombre del servicio a modificar: "
--            service <- getLine
--            case service of
--                "" -> do
--                    putStrLn "Ingrese un nombre de servicio válido"
--                    loopGestion nombreUsuario
--            putStrLn "Nueva contraseña: "
--            password <- getLine
--            case password of
--                "" -> do
--                    putStrLn "Ingrese una contraseña de servicio válido"
--                    loopGestion nombreUsuario

            -- Eliminar el servicio antiguo
--            archivoViejo <- withFile "servicios.txt" ReadMode
--            archivoNuevo <- withFile "servicios.tmp" WriteMode
--            eliminarServicio nombreUsuario service archivoViejo archivoNuevo
--            hClose archivoViejo
--            hClose archivoNuevo
            
--            removeFile "servicios.txt"
--            renameFile "servicios.tmp" "servicios.txt"

            -- Agregar el archivo nuevo
--            archivo <- withFile "servicios.txt" AppendMode
--            agregarArchivo nombreUsuario service password archivo
--            hClose archivo

--            loopGestion nombreUsuario

    -- Opción para eliminar los datos de un servicio
    -- De manera resumida, toma el archivo servicios.txt y guarda en servicios.tmp todas las líneas
    -- que no correspondan a la línea a eliminar, luego eliminar el archivo servicios.txt y renombra
    -- servicios.tmp como servicios.txt, para dejarlo todo normal
--        "4" -> do
--            putStrLn "Nombre del servicio a eliminar: "
--            service <- getLine
--            case service of
--                "" -> do
--                    putStrLn "Ingrese un nombre de servicio válido"
--                    loopGestion nombreUsuario
            
--            archivoViejo <- withFile "servicios.txt" ReadMode
--            archivoNuevo <- withFile "servicios.tmp" WriteMode
--            eliminarServicio nombreUsuario service archivoViejo archivoNuevo
--            hClose archivoViejo
--            hClose archivoNuevo
            
--            removeFile "servicios.txt"
--            renameFile "servicios.tmp" "servicios.txt"

--            loopGestion nombreUsuario

    -- Opción para regresar al menú principal
--        "5" -> do
--            main

--        _ -> do
--            putStrLn "Opción no válida, intente nuevamente"
--            loopGestion nombreUsuario

