module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Definir el lenguaje via constructores de tipo
data Dibujo a =
    Basica a
    | Rotar (Dibujo a)
    | Espejar (Dibujo a)
    | Rotar45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f n d
    | n < 0     = error "No se puede componer un número negativo de veces"
    | n == 0    = d
    | otherwise = f (comp f (n - 1) d)

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 d = comp Rotar 2 d

r270 :: Dibujo a -> Dibujo a
r270 d = comp Rotar 3 d

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) d1 d2 = Encimar d1 d2

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) d1 d2 = Juntar 1 1 d1 d2

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) d1 d2 = Apilar 1 1 d1 d2

-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (d1 /// d3) ^^^ (d2 /// d4)

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib d = Basica d

-- Definimos un tipo de figuras básicas de prueba
data FiguraBasica = Triangulo | Cuadrado deriving (Eq, Show)

-- Convierte una figura básica en una imagen de Gloss con colores distintos
interpBasica :: FiguraBasica -> Color -> Picture
interpBasica Triangulo col = color col $ polygon [(0,0), (50,100), (100,0)]
interpBasica Cuadrado col = color col $ polygon [(0,0), (0,100), (100,100), (100,0)]

-- Interpreta un dibujo y lo convierte en una imagen con colores
interp :: Dibujo FiguraBasica -> [Color] -> Picture
interp (Basica f) (col:_) = interpBasica f col
interp (Rotar d) cols = rotate 90 (interp d cols)
interp (Espejar d) cols = scale (-1) 1 (interp d cols)
interp (Rotar45 d) cols = rotate 45 (interp d cols)
interp (Apilar _ _ d1 d2) cols = case splitAt (length cols `div` 2) cols of
    (c1, c2) -> pictures [translate 0 50 (interp d1 c1), translate 0 (-50) (interp d2 c2)]
interp (Juntar _ _ d1 d2) cols = case splitAt (length cols `div` 2) cols of
    (c1, c2) -> pictures [translate (-50) 0 (interp d1 c1), translate 50 0 (interp d2 c2)]
interp (Encimar d1 d2) (c1:c2:cs) = pictures [interp d1 [c1], interp d2 [c2]]
interp _ _ = Blank

-- Función main para mostrar un cuarteto con colores distintos
main :: IO ()
main = display (InWindow "Dibujo" (600, 600) (10, 10)) black 
    (interp (cuarteto (pureDib Triangulo) (pureDib Cuadrado) (pureDib Cuadrado) (pureDib Triangulo)) 
    [red, blue, green, yellow])
