module Dibujo where

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

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (d^^^(Rotar d)) ^^^ ((r180 d) ^^^ (r270 d))



-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (Rotar d) (r180 d) (r270 d)


-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib d = Basica d

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b


-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b





