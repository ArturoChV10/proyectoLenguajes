-- Proyecto 2: Paradigma Funcional

-- Registro de usuarios

import System.IO
import System.Directory (doesFileExist, removeFile, renameFile) -- >> ghc -package directory RegistroUsuarios.hs --> .\RegistroUsuarios.exe
import Text.Read (readMaybe)
import Data.List (isInfixOf)

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
                                    appendFile archivo (nombre ++ ";" ++ pin_String ++ "\n")
                                    -- appendFile agrega el nombre al final del archivo
                                    putStrLn "Usuario registrado con éxito."
                    else do
                        writeFile archivo (nombre ++ ";" ++ pin_String ++ "\n")

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

-- Función encargada de tomar una cadena de texto y separar donde haya ";"
separarPorPuntoYComa :: String -> [String]
separarPorPuntoYComa [] = [""]
separarPorPuntoYComa (x:xs)
    | x == ';'  = "" : separarPorPuntoYComa xs
    | otherwise = (x : head resto) : tail resto
  where
    resto = separarPorPuntoYComa xs

-- Funcion que lee el archivo "usuarios.txt" y devuelve una lista de usuarios
leerUsuarios :: FilePath -> IO [[String]]
leerUsuarios archivo = do
    contenido <- readFile archivo
    -- readFile lee el contenido del archivo y lo guarda en la variable contenido
    let lineas = lines contenido
    -- lines separa el contenido en lineas
    return $ map separarPorPuntoYComa lineas
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

mainLoop :: IO ()
mainLoop = do
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
            mainLoop

    -- Opcion para ingresar a cuenta de usuario
        "2" -> do
            -- login -- Por ahora no voy a implementar el login debido a que tengo que hacer validaciones de contraseña y me da pereza
            putStrLn "Ingrese su nombre de usuario:"
            nombre <- getLine
            loopGestion nombre

    -- Opcion para salir del programa
        "3" -> do
            putStrLn "Saliendo del programa..."

    -- Caso donde no se escoja una de las opciones
        _ -> do
            putStrLn "Opción no válida, intente nuevamente"
            mainLoop


agregarArchivo :: String -> String -> String -> Handle -> IO ()
agregarArchivo username serviceName password archivo = do
  hPutStrLn archivo (username ++ ";" ++ serviceName ++ ";" ++ password)

consultarServicio :: String -> Handle -> IO ()
consultarServicio name archivo = do
    completo <- hIsEOF archivo
    if completo
        then do
            putStrLn "No se encontró el servicio"
            return ()
        else do
            linea <- hGetLine archivo
            if name `isInfixOf` linea -- name está en linea
                then do
                    putStrLn linea
                    return ()
                else consultarServicio name archivo

eliminarServicio :: String -> String -> Handle -> Handle -> IO ()
eliminarServicio username serviceName oldFile newFile = do
  completo <- hIsEOF oldFile
  if completo
    then return ()
    else do
        linea <- hGetLine oldFile
        let coincide = serviceName `isInfixOf` linea
        if coincide
            then eliminarServicio username serviceName oldFile newFile
            else do
                hPutStrLn newFile linea
                eliminarServicio username serviceName oldFile newFile


loopGestion :: String -> IO ()
loopGestion nombreUsuario = do
    putStrLn "\n=== Gestión de Contraseñas ==="
    putStrLn "Seleccione la acción a realizar"
    putStrLn "1. Agregar cuenta"
    putStrLn "2. Consultar cuenta"
    putStrLn "3. Modificar cuenta"
    putStrLn "4. Eliminar cuenta"
    putStrLn "5. Regresar"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    -- Opcion para agregar un nuevo usuario
    case opcion of
        "1" -> do
            putStrLn "Nombre del nuevo servicio: "
            service <- getLine
            putStrLn "Contraseña: "
            password <- getLine
            archivo <- openFile "servicios.txt" AppendMode
            agregarArchivo nombreUsuario service password archivo
            hClose archivo
            loopGestion nombreUsuario

    -- Opcion para consultar cuenta de usuario
        "2" -> do
            putStr "Nombre del servicio a consultar: "
            nombre <- getLine
            archivo <- openFile "servicios.txt" AppendMode
            consultarServicio nombre archivo
            hClose archivo
            loopGestion nombreUsuario

    -- Opción para modificar los datos de un servicio
        "3" -> do
            putStrLn "Nombre del servicio a modificar: "
            service <- getLine
            putStrLn "Nueva contraseña: "
            password <- getLine

            -- Eliminar el servicio antiguo
            archivoViejo <- openFile "servicios.txt" ReadMode
            archivoNuevo <- openFile "servicios.tmp" WriteMode
            eliminarServicio nombreUsuario service archivoViejo archivoNuevo
            hClose archivoViejo
            hClose archivoNuevo
            
            removeFile "servicios.txt"
            renameFile "servicios.tmp" "servicios.txt"

            -- Agregar el archivo nuevo
            archivo <- openFile "servicios.txt" AppendMode
            agregarArchivo nombreUsuario service password archivo
            hClose archivo

            loopGestion nombreUsuario

    -- Opción para eliminar los datos de un servicio
    -- De manera resumida, toma el archivo servicios.txt y guarda en servicios.tmp todas las líneas
    -- que no correspondan a la línea a eliminar, luego eliminar el archivo servicios.txt y renombra
    -- servicios.tmp como servicios.txt, para dejarlo todo normal
        "4" -> do
            putStrLn "Nombre del servicio a eliminar: "
            service <- getLine
            
            archivoViejo <- openFile "servicios.txt" ReadMode
            archivoNuevo <- openFile "servicios.tmp" WriteMode
            eliminarServicio nombreUsuario service archivoViejo archivoNuevo
            hClose archivoViejo
            hClose archivoNuevo
            
            removeFile "servicios.txt"
            renameFile "servicios.tmp" "servicios.txt"

            loopGestion nombreUsuario

    -- Opción para regresar al menú principal
        "5" -> do
            mainLoop

        _ -> do
            putStrLn "Opción no válida, intente nuevamente"
            loopGestion nombreUsuario

