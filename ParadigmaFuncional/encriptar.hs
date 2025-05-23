module Encriptar (
    encriptar,
    desencriptar,
    separar,
    separar3,
    esValido
) where

import Data.Bits (xor)
import Data.Char (chr, ord)
import Numeric (showHex, readHex)
import Text.Printf (printf)
import Control.Exception (evaluate)

-- Encripta con XOR y lo convierte a hexadecimal
encriptar :: String -> Int -> String
encriptar texto pin = concatMap (\c -> printf "%02x" (ord c `xor` pin)) texto

-- Desencripta de hexadecimal
desencriptar :: String -> Int -> String
desencriptar [] _ = []
desencriptar (a:b:resto) pin =
    let [(valor, _)] = readHex [a,b]
    in chr (valor `xor` pin) : desencriptar resto pin

separar :: String -> (String, String)
--Separa el string cuando encuentra un ;
separar xs = let (a, b) = break (== ';') xs
                in (a, drop 1 b)

separar3 :: String -> (String, String, String)
separar3 xs =
    let (a, resto1) = break (== ';') xs
        (b, resto2) = break (== ';') (drop 1 resto1)
        c = drop 1 resto2
    in (a, b, c)

esValido :: String -> String -> Int -> FilePath -> IO Bool
-- Recibe un usuario y contraseña y verifica si son correctos -> True: Acceso habilitado
--                                                            -> False: Acceso inhabilitado
esValido "" _ _ _ = return False --Usuario vacio
esValido _ "" _ _= return False --Contraseña vacia
esValido _ _ 0 _= return False -- Pin vacio
esValido _ _ _ "" = return False --Ruta vacia
esValido usuario contra pin archivo = do
    cuerpo <- readFile archivo
    evaluate (length cuerpo)
    let lineas = lines cuerpo
        lista = map separar lineas
        uHash = encriptar usuario pin
        cHash = encriptar contra pin
    return $ any (\(u, c) -> u == uHash && c == cHash) lista

--Ejemplos de como usar las funciones                           
-- main :: IO()
-- main = do
--     print (encriptar "Hola" 123)
--     valido <- esValido "Hola" "Hola" 123 "usuarios.txt"
--     print valido
--     print (desencriptar "3314171a" 123 )
