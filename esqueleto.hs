-- DATOS Y SHOW

data Modificacion
  = Insertar Integer Char
  | Borrar Integer
  | Substituir Integer Char deriving (Show, Eq)

type PaqueteModificaciones = [Modificacion]

data Archivo
  = ArchivoVacio
  | NuevaVersion PaqueteModificaciones Archivo
instance Show Archivo where
    show ArchivoVacio = "Archivo vacio"
    show file = "Archivo: " ++ obtenerUltimaVersion file

data SCV = NuevoSCV | AgregarArchivo Archivo SCV
instance Show SCV where
    show NuevoSCV = "SCV vacio"
    show scv = verArchivos scv

verArchivos :: SCV -> String
verArchivos NuevoSCV = ""
verArchivos (AgregarArchivo file scv)
  = "- " ++ (show file) ++ "\n" ++ (verArchivos scv)

-- EJERCICIOS

-- Ejercicio 1/8

borrar :: Integer -> String -> String
borrar _ [] = error
  "La string no posee un caracter en la posicion que se quiere borrar"
borrar 0 (x:xs) = xs
borrar n (x:xs) = x:borrar (n-1) xs

sust :: Integer -> Char -> String -> String
sust _ _ [] = error
  "La string no posee un caracter en la posicion que se quiere sustituir"
sust 0 c (x:xs) = c:xs
sust n c (x:xs) = x:sust (n-1) c xs

insert :: Integer -> Char -> String -> String
insert 0 c str = c:str
insert _ _ [] = error
                "La posicion donde se quiere insertar no existe en la string"
insert n c (x:xs) = x:insert (n-1) c xs


aplicarModificacion :: String -> Modificacion -> String
aplicarModificacion str (Insertar n c) = insert n c str
aplicarModificacion str (Borrar n) = borrar n str
aplicarModificacion str (Substituir n c) = sust n c str

-- Ejemplos:
-- Main> aplicarModificacion "d" (Insertar 1 'a')
-- "da"
-- Main> aplicarModificacion "d" (Insertar 0 'a')
-- "ad"
-- Main> aplicarModificacion "dato" (Borrar 1)
-- "dto"

-- Ejercicio 2/8
aplicarPaqueteModificaciones ::
  String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones str [] = str
aplicarPaqueteModificaciones str (m:ms) =
  aplicarPaqueteModificaciones (aplicarModificacion str m) ms

-- Ejemplos:
-- Main> aplicarPaqueteModificaciones "dato" [Substituir 0 'p', Insertar 4 's']
-- "patos"

-- Ejercicio 3/8
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion ArchivoVacio = ""
obtenerUltimaVersion (NuevaVersion paquete archivo) =
  aplicarPaqueteModificaciones (obtenerUltimaVersion archivo) paquete

-- Ejemplos: (ver def. archivo1 y archivo2 abajo)
-- Main> obtenerUltimaVersion archivo1
-- "dato"
-- Main> obtenerUltimaVersion archivo2
-- "ddato"

-- Ejercicio 4/8
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio = 0
cantVersiones (NuevaVersion paquete archivo) =
  1 + cantVersiones archivo

-- Ejemplos:
-- Main> cantVersiones archivo1
-- 1
-- Main> cantVersiones archivo2
-- 2

-- Ejercicio 5/8

obtenerVersionAnterior :: Archivo -> Archivo
obtenerVersionAnterior ArchivoVacio = ArchivoVacio
obtenerVersionAnterior (NuevaVersion paquete archivo) = archivo

obtenerVersion :: Integer -> Archivo -> String
obtenerVersion n archivo
  | cantVersiones archivo == n = obtenerUltimaVersion archivo
  | otherwise = obtenerVersion n (obtenerVersionAnterior archivo)

-- Ejemplos:
-- Main> obtenerVersion 1 archivo2
-- "dato"

-- Ejercicio 6/8
levenshtein :: String -> String -> Integer --PaqueteModificaciones
levenshtein = error "Implementar!!! (ejercicio 6)"

-- Ejemplos:
-- Main> levenshtein "auto" "automata"
-- 4

-- Ejercicio 7/8
levenshtein2 :: String -> String -> PaqueteModificaciones
levenshtein2 = error "Implementar!!! (ejercicio 7)"

-- Ejemplos:
-- Main> levenshtein2 "auto" "automata"
-- [Insertar 4 'a',Insertar 4 't',Insertar 4 'a',Insertar 4 'm']


-- Ejercicio 8/8
agregarVersion :: String -> Archivo -> Archivo
agregarVersion = error "Implementar!!! (ejercicio 8)"

-- Ejemplos:
-- Main> agregarVersion "dato" archivo2
-- Archivo: dato

-- Funciones provistas por la c\'atedra

len :: [a] -> Integer
len xs = fromIntegral (length xs)

-- Archivos

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1
