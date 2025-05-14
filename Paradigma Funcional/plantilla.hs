import System.IO

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
            username <- getLine
            putStr "Contraseña: "
            password <- getLine
            mainLoop
    
    -- Opcion para ingresar a cuenta de usuario
        "2" -> do
            putStr "Nombre de usuario existente: "
            username <- getLine
            putStr "Contraseña del usuario: "
            password <- getLine
            loopGestion
            
        "3" -> do
            putStrLn "Saliendo del programa..."
            
        _ -> do
            putStrLn "Opción no válida, intente nuevamente"
            mainLoop
            
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
            putStr "Nombre del nuevo servicio: "
            service <- getLine
            putStr "Contraseña: "
            password <- getLine
            loopGestion
    
    -- Opcion para consultar cuenta de usuario
        "2" -> do
            putStr "Nombre del servicio a consultar: "
            nombre <- getLine
            loopGestion
            
    -- Opción para modificar los datos de un servicio
        "3" -> do
            putStr "Nombre del servicio a modificar: "
            service <- getLine
            putStr "Nueva contraseña: "
            password <- getLine
            loopGestion

    -- Opción para eliminar los datos de un servicio
        "4" -> do
            putStr "Nombre del servicio a eliminar: "
            service <- getLine
            loopGestion
            
        "5" -> do
            mainLoop
            
        _ -> do
            putStrLn "Opción no válida, intente nuevamente"

