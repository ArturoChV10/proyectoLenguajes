{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant return" #-}
import System.IO
import System.Directory (doesFileExist)
import Data.List.Split (splitOn)
import PlantillaXRegistro (login)


-- Función para mostrar la tabla de usuarios
tablaSistema :: String -> IO ()
tablaSistema usuario = do
    existe <- doesFileExist "contenido.txt"
    if not existe
        then putStrLn "No hay contenido en el archivo."
        else do
            contenido <- readFile "contenido.txt"
            let lineas = lines contenido
            -- let lineas divide el texto del archivo en una lista
                tabla = [ (campos !! 0, campos !! 1, campos !! 2)
                -- Convierte cada linea en una tupla (sitio, usuario, contraseña) 0 es el sitio, 1 es el usuario y 2 es la contraseña
                        | separar <- lineas
                        -- Convierte cada linea en una lista de palabras
                        , let campos = splitOn ";" separar
                        -- Separa cada linea en una lista de palabras usando el separador ";"
                        , campos !! 1 == usuario
                        -- Considera solamente la sublista con el usuario que inicio sesión
                        ]
            putStrLn "======== Tabla de sitios & contraseñas ========="
            putStrLn "| Sitio Web | Usuario | Contraseña |"
            mapM_ imprimirFila tabla


-- Función para imprimir las filas de la tabla
imprimirFila :: (String, String, String) -> IO ()
imprimirFila (sitio, usuario, contrasena) = do
    putStrLn $ "| " ++ sitio ++ " | " ++ usuario ++ " | " ++ contrasena ++ " |"
    putStrLn "------------------------------------"

-- Función para el sitio web que se va a desplegar en la tabla
sitioWebTabla :: IO String
sitioWebTabla = do
    putStrLn "Ingrese el nombre del sitio web:"
    sitio <- getLine
    return sitio
    -- devuelve el nombre del sitio web ingresado

-- Funcion para la contraseña que se va a desplegar en la tabla
contraseñaTabla:: IO String
contraseñaTabla = do
    putStrLn "Ingrese el nombre de la contraseña del sitio web:"
    contraseña <- getLine
    return contraseña

-- Funcion para agregar el sitio web, el usuario y la contraseña al .txt que usa la tabla
tablaContenido :: String -> IO ()
tablaContenido usuario = do
    sitioWeb <- sitioWebTabla
    contraseña <- contraseñaTabla

    let archivo = "contenido.txt"
    existe <- doesFileExist archivo
    let linea = sitioWeb ++ ";" ++ usuario ++ ";" ++ contraseña ++ "\n"

    if existe
        then do
            appendFile archivo linea
            putStrLn "Contenido agregado con éxito."
        else do
            writeFile archivo linea
            putStrLn "Contenido agregado con éxito."

menuUsuario :: String -> IO ()
menuUsuario usuario = do
    putStrLn "-----------------------------------------------"
    putStrLn ("Bienvenido, " ++ usuario ++ "!")
    putStrLn "\nSeleccione una opción:\n"
    putStrLn "\n 1. Ver sitios y contraseñas\n"
    putStrLn "\n2. Agregar un nuevo sitio con su contraseña\n"
    putStrLn "\n3. Cerrar sesión\n"
    putStrLn "\n4. Salir\n"
    putStrLn "-----------------------------------------------"
    putStr "Ingrese su opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            tablaSistema usuario
            menuUsuario usuario
        "2" -> do
            tablaContenido usuario
            menuUsuario usuario
        "3" -> do
            putStrLn "Cerrando sesión..."
            main
        "4" -> do
            putStrLn "Saliendo del programa..."
            return ()
        _ -> do
            putStrLn "Opción no válida. Intente de nuevo."
            menuUsuario usuario

main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de contraseñas"
    putStrLn "-----------------------------------------------"
    resultado <- login
    case resultado of
        Nothing -> putStrLn "Usuario o contraseña incorrectos. Intente de nuevo."
        Just usuario -> do
            menuUsuario usuario
            putStrLn "-----------------------------------------------"