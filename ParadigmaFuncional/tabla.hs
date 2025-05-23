{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant return" #-}
import System.IO
import System.Directory (doesFileExist, removeFile, renameFile)
import Data.List.Split (splitOn)
import PlantillaXRegistro (login, registrarUsuario, agregarArchivo, eliminarServicio, consultarServicio)
import Control.Exception (evaluate)
import Encriptar (encriptar, desencriptar)

-- Función para mostrar la tabla de usuarios
tablaSistema :: String -> Int -> IO ()
tablaSistema usuario pin = do
    existe <- doesFileExist "contenido.txt"
    if not existe
        then putStrLn "No hay contenido en el archivo."
        else do
            contenido <- readFile "contenido.txt"
            evaluate (length contenido)
            let lineasEnc = lines contenido
                lineas = map (\lineaEnc ->
                                    let camposEnc = splitOn ";" lineaEnc
                                        camposDec = map (`desencriptar` pin) camposEnc
                                    in camposDec
                                ) lineasEnc
            -- let lineas divide el texto del archivo en una lista
            let tabla = [ (sitio, usuarioCampo, pass)
                        | campos <- lineas
                        , length campos == 3
                        , let [sitio, usuarioCampo, pass] = campos
                        , usuarioCampo == usuario
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
tablaContenido :: String -> Int -> IO ()
tablaContenido usuario pin = do
    sitioWeb <- sitioWebTabla
    contraseña <- contraseñaTabla

    let archivo = "contenido.txt"
    existe <- doesFileExist archivo
    let linea = ((encriptar sitioWeb pin)++ ";" ++ ( encriptar usuario pin) ++ ";" ++ (encriptar contraseña pin) ++ "\n")

    if existe
        then do
            appendFile archivo linea
            putStrLn "Contenido agregado con éxito."
        else do
            writeFile archivo linea
            putStrLn "Contenido agregado con éxito."

modificarContenido :: String -> Int -> IO()
modificarContenido nombreUsuario pin = do
    putStrLn "Nombre del servicio a modificar: "
    service <- getLine
    if null service
        then do
            putStrLn "Ingrese un nombre de servicio válido"
            menuUsuario nombreUsuario pin
        else do 
            putStrLn "Nueva contraseña: "
            password <- getLine
            if null password
                then do
                    putStrLn "Ingrese una contraseña de servicio válido"
                    menuUsuario nombreUsuario pin
                else do
                    -- Eliminar el servicio antiguo
                    archivoViejo <- openFile "contenido.txt" ReadMode
                    archivoNuevo <- openFile "contenido.tmp" WriteMode
                    eliminarServicio nombreUsuario service archivoViejo archivoNuevo pin
                    hClose archivoViejo
                    hClose archivoNuevo
                    
                    removeFile "contenido.txt"
                    renameFile "contenido.tmp" "contenido.txt"

                    -- Agregar el archivo nuevo
                    archivo <- openFile "contenido.txt" AppendMode
                    agregarArchivo nombreUsuario service password archivo pin
                    hClose archivo

                    menuUsuario nombreUsuario pin

eliminarContenido :: String -> Int -> IO ()
eliminarContenido username pin = do
    putStrLn "Nombre del servicio a eliminar: "
    service <- getLine
    if null service
        then do
            putStrLn "Ingrese un nombre de servicio válido"
            menuUsuario username pin
        else do 
            archivoViejo <- openFile "contenido.txt" ReadMode
            archivoNuevo <- openFile "contenido.tmp" WriteMode
            eliminarServicio username service archivoViejo archivoNuevo pin
            hClose archivoViejo
            hClose archivoNuevo
            
            removeFile "contenido.txt"
            renameFile "contenido.tmp" "contenido.txt"
            menuUsuario username pin

consultarContenido :: String -> Int -> IO ()
consultarContenido username pin = do
    putStrLn "Nombre del servicio a consultar: "
    service <- getLine
    if null service
        then do
            putStrLn "Ingrese un nombre de servicio válido"
            menuUsuario username pin
        else do 
            archivo <- openFile "contenido.txt" ReadMode
            consultarServicio username service archivo pin
            hClose archivo
            menuUsuario username pin

menuUsuario :: String -> Int -> IO ()
menuUsuario usuario pin = do
    putStrLn "-----------------------------------------------"
    putStrLn ("Bienvenido, " ++ usuario ++ "!")
    putStrLn "\nSeleccione una opción:\n"
    putStrLn "1. Ver sitios y contraseñas"
    putStrLn "2. Agregar un nuevo sitio con su contraseña"
    putStrLn "3. Consultar un sitio especifico"
    putStrLn "4. Modificar contraseña de un sitio"
    putStrLn "5. Eliminar un sitio"
    putStrLn "6. Registrar un nuevo usuario"
    putStrLn "7. Cerrar sesión"
    putStrLn "8. Salir"
    putStrLn "-----------------------------------------------"
    putStr "Ingrese su opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            tablaSistema usuario pin
            menuUsuario usuario pin
        "2" -> do
            tablaContenido usuario pin
            menuUsuario usuario pin
        "3" -> do
            consultarContenido usuario pin
            menuUsuario usuario pin
        "4" -> do
            modificarContenido usuario pin
            menuUsuario usuario pin
        "5" -> do
            eliminarContenido usuario pin
            menuUsuario usuario pin
        "6" -> do
            putStrLn "-----------------------------------------------"
            putStrLn "Registro de nuevo usuario"
            putStrLn "-----------------------------------------------"
            registrarUsuario
            putStrLn "-----------------------------------------------"
            menuUsuario usuario pin
        "7" -> do
            putStrLn "Cerrando sesión..."
            main
        "8" -> do
            putStrLn "Saliendo del programa..."
        _ -> do
            putStrLn "Opción no válida. Intente de nuevo."
            menuUsuario usuario pin

main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de contraseñas"
    putStrLn "-----------------------------------------------"
    resultado <- login
    case resultado of
        Nothing -> putStrLn "Usuario o contraseña incorrectos. Intente de nuevo."
        Just (usuario, pin) -> do
            menuUsuario usuario pin
            putStrLn "-----------------------------------------------"