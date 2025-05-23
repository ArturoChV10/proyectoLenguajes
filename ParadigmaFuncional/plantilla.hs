import System.IO
import Data.List (isInfixOf)
import System.Directory (removeFile, renameFile)

main :: IO ()
main = mainLoop

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
            putStr "Nombre de usuario: "
            hFlush stdout
            username <- getLine
            putStr "Contraseña: "
            hFlush stdout
            password <- getLine
            
            archivo <- openFile "usuarios.txt" AppendMode
            crearArchivo username password archivo
            hClose archivo
            mainLoop

    -- Opcion para ingresar a cuenta de usuario
        "2" -> do
            putStrLn "Nombre de usuario existente: "
            hFlush stdout
            username <- getLine
            putStrLn "Contraseña del usuario: "
            hFlush stdout
            password <- getLine
            loopGestion

        "3" -> do
            putStrLn "Saliendo del programa..."

        _ -> do
            putStrLn "Opción no válida, intente nuevamente"
            mainLoop

crearArchivo :: String -> String -> Handle -> IO ()
crearArchivo name password archivo = do
  hPutStrLn archivo (name ++ ";" ++ password)

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

eliminarServicio :: String -> Handle -> Handle -> IO ()
eliminarServicio name oldFile newFile = do
  completo <- hIsEOF oldFile
  if completo
    then return ()
    else do
        linea <- hGetLine oldFile
        let coincide = name `isInfixOf` linea
        if coincide
            then eliminarServicio name oldFile newFile
            else do
                hPutStrLn newFile linea
                eliminarServicio name oldFile newFile


loopGestion :: IO ()
loopGestion = do
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
            crearArchivo service password archivo
            hClose archivo
            loopGestion

    -- Opcion para consultar cuenta de usuario
        "2" -> do
            putStr "Nombre del servicio a consultar: "
            nombre <- getLine
            archivo <- openFile "servicios.txt" AppendMode
            consultarServicio nombre archivo
            hClose archivo
            loopGestion

    -- Opción para modificar los datos de un servicio
        "3" -> do
            putStrLn "Nombre del servicio a modificar: "
            service <- getLine
            putStrLn "Nueva contraseña: "
            password <- getLine

            -- Eliminar el servicio antiguo
            archivoViejo <- openFile "servicios.txt" ReadMode
            archivoNuevo <- openFile "servicios.tmp" WriteMode
            eliminarServicio service archivoViejo archivoNuevo
            hClose archivoViejo
            hClose archivoNuevo
            
            removeFile "servicios.txt"
            renameFile "servicios.tmp" "servicios.txt"

            -- Agregar el archivo nuevo
            archivo <- openFile "servicios.txt" AppendMode
            crearArchivo service password archivo
            hClose archivo

            loopGestion

    -- Opción para eliminar los datos de un servicio
        "4" -> do
            putStrLn "Nombre del servicio a eliminar: "
            service <- getLine
            
            archivoViejo <- openFile "servicios.txt" ReadMode
            archivoNuevo <- openFile "servicios.tmp" WriteMode
            eliminarServicio service archivoViejo archivoNuevo
            hClose archivoViejo
            hClose archivoNuevo
            
            removeFile "servicios.txt"
            renameFile "servicios.tmp" "servicios.txt"

            loopGestion

    -- Opción para regresar al menú principal
        "5" -> do
            mainLoop

        _ -> do
            putStrLn "Opción no válida, intente nuevamente"
            loopGestion

