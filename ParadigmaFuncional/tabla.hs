{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
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
sitioWeb :: IO ()
sitioWeb = do
    putStrLn "Ingrese el nombre del sitio web:"
    sitio <- getLine
    putStrLn $ "Sitio ingresado: " ++ sitio

-- Funcion para la contraseña que se va a desplegar en la tabla
contraseñaTabla:: IO ()
contraseñaTabla = do
    putStrLn "Ingrese el nombre de la contraseña del sitio web:"
    contraseña <- getLine
    putStrLn $ "Sitio ingresado: " ++ contraseña

-- Funcion para agregar el sitio web, el usuario y la contraseña al .txt que usa la tabla
tablaContenido :: IO ()
tablaContenido = do
    nombre_usuario <- readFile "usuarios.txt"
    let lineas = lines nombre_usuario
        usuarios = map(head.words) lineas

    sitioWeb <- contraseñaTabla
    contraseña <- contraseñaTabla

    let archivo = "contenido.txt"
    existe <- doesFileExist archivo

    if existe
        then do
            appendFile archivo (sitioWeb ++ " " ++ usuarios ++ " " ++ contraseña ++ "\n")
            putStrLn "Contenido agregado con éxito."
        else do
            writeFile archivo (sitioWeb ++ " " ++ usuarios ++ " " ++ contraseña ++ "\n")
            putStrLn "Contenido agregado con éxito."

main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de contraseñas"
    putStrLn "-----------------------------------------------"
    tablaContenido
    tablaSistema
    putStrLn "-----------------------------------------------"
    putStrLn "Fin del programa"