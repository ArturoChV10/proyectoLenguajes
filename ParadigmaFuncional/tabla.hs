{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant return" #-}
import System.IO
import System.Directory (doesFileExist)

-- Función para mostrar la tabla de usuarios
tablaSistema :: IO ()
tablaSistema = do
    nombre_usuario <- readFile "usuarios.txt"
    let lineas = lines nombre_usuario
        usuarios = map(head.words) lineas
        tabla = map(\u -> ("Sitio", u, "XXXX")) usuarios
    putStrLn "======== Tabla de Usuarios ========="
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
    let linea = sitioWeb ++ " " ++ usuario ++ " " ++ contraseña ++ "\n"

    if existe
        then do
            appendFile archivo linea
            putStrLn "Contenido agregado con éxito."
        else do
            writeFile archivo linea
            putStrLn "Contenido agregado con éxito."

main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de contraseñas"
    putStrLn "-----------------------------------------------"
    tablaContenido
    tablaSistema
    putStrLn "-----------------------------------------------"
    putStrLn "Fin del programa"