module Dibujo where

-- Definir el lenguaje.

--------------- Definición del lenguaje como un tipo de datos --------------------

data Dibujo a = Basica a | Rotar (Dibujo a) | Espejar (Dibujo a) | Rot45 (Dibujo a)
     | Apilar Int Int (Dibujo a) (Dibujo a)
     | Juntar Int Int (Dibujo a) (Dibujo a)
     | Encimar (Dibujo a) (Dibujo a)
     deriving (Show, Eq)

-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp funcion 0 x = x --Devuelve la identidad
comp funcion 1 x = funcion x --Caso base solo evalua la funcion una vez
comp funcion repeticiones x = funcion (comp funcion (repeticiones-1) x)  --Caso recursivo, la funcion se aplica 'repeticiones' veces sobre x hasta llegar al caso base


--------------------------------Combinadores-----------------------------------
-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 dibujo = comp (Rotar) 2 dibujo

r270 :: Dibujo a -> Dibujo a
r270 dibujo = comp (Rotar) 3 dibujo

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) dibujo1 dibujo2 = Apilar 1 1 dibujo1 dibujo2

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) dibujo1 dibujo2 = Juntar 1 1 dibujo1 dibujo2

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) dibujo1 dibujo2 = Encimar dibujo1 dibujo2

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto dibujo1 dibujo2 dibujo3 dibujo4 = (.-.) ((///) dibujo1 dibujo2) ((///) dibujo3 dibujo4)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a 
encimar4 dibujo = (^^^) ((^^^) (dibujo) (Rotar dibujo)) ((^^^) (r180 dibujo) (r270 dibujo))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, .bas rot esp r45 apl jnt enc.., 3}.
ciclar :: Dibujo a -> Dibujo a
ciclar dibujo = (.-.) ((///) (Rotar dibujo) (r180 dibujo)) ((///) (r270 dibujo) (dibujo))


-----------Definir esquemas para la manipulación de figuras básicas------------

-- Ver un a como una figura.
pureDib :: a -> Dibujo a
pureDib a = Basica a

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib funcion (Basica a) = Basica (funcion a)
mapDib funcion (Rotar a) = Rotar (mapDib funcion a)
mapDib funcion (Espejar a) = Espejar (mapDib funcion a)
mapDib funcion (Rot45 a) = Rot45 (mapDib funcion a)
mapDib funcion (Encimar a b) = Encimar (mapDib funcion a) (mapDib funcion b)
mapDib funcion (Juntar x y d d') = Juntar x y (mapDib funcion d) (mapDib funcion d')
mapDib funcion (Apilar x y d d') = Apilar x y (mapDib funcion d) (mapDib funcion d')

-- Verificar que las operaciones satisfagan:
-- 1. mapDib id = id, donde id es la función identidad.
-- 2. mapDib (g ∘ f) = (mapDib g) ∘ (mapDib f).

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de intro a la lógica
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Int -> Int -> b -> b -> b) -> 
       (Int -> Int -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
sem bas rot esp r45 apl jnt enc (Basica a) = bas a
sem bas rot esp r45 apl jnt enc (Rotar a) = rot (sem bas rot esp r45 apl jnt enc a)
sem bas rot esp r45 apl jnt enc (Espejar a) = esp (sem bas rot esp r45 apl jnt enc a)
sem bas rot esp r45 apl jnt enc (Rot45 a) = r45 (sem bas rot esp r45 apl jnt enc a)
sem bas rot esp r45 apl jnt enc (Apilar x y a a') = apl x y (sem bas rot esp r45 apl jnt enc a) (sem bas rot esp r45 apl jnt enc a')
sem bas rot esp r45 apl jnt enc (Juntar x y a a') = jnt x y (sem bas rot esp r45 apl jnt enc a) (sem bas rot esp r45 apl jnt enc a')
sem bas rot esp r45 apl jnt enc (Encimar a a') = enc (sem bas rot esp r45 apl jnt enc a) (sem bas rot esp r45 apl jnt enc a')
    

-------------------------Predicados sobre dibujos------------------------------
type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar pred b a = mapDib(\d -> if pred d then b else d) a

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred d = sem pred id id id (\a b c p -> c || p) (\a b c p -> c || p) (\b c -> b || c) d

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred d = sem pred id id id (\a b c p -> c && p) (\a b c p -> c && p) (\b c -> b && c) d

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP pred1 pred2 = \d -> pred1 d && pred2 d

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP pred1 pred2 = \d -> pred1 d || pred2 d

-- Describe la figura. Ejemplos: 
--   desc (const "b") (Basica b) = "b"
--   desc (const "b") (Rotar (Basica b)) = "rot (b)"
--   desc (const "b") (Apilar n m (Basica b) (Basica b)) = "api n m (b) (b)"
-- La descripción de cada constructor son sus tres primeros
-- símbolos en minúscula, excepto `Rot45` al que se le agrega el `45`.
desc :: Show a => (a -> String) -> Dibujo a -> String
desc fun a = sem (fun) (\a -> "rot (" ++ a ++ ")") (\a -> "esp (" ++ a ++ ")") (\a -> "r45 (" ++ a ++ ")")  (\a b c p -> "apl (" ++ c ++ ") (" ++ p ++ ")") (\a b c p -> "jnt (" ++ c ++ ") (" ++ p ++ ")") (\a b -> "enc (" ++ a ++ ") (" ++ b ++ ")") a


--Junta todas las figuras básicas de un dibujo.
basicas :: Dibujo a -> [a]
basicas a = sem (\a -> a : []) id id id  (\a b c p -> c ++ p) (\a b c p -> c ++ p) (\c p -> c ++ p) a

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 (Rotar (Rotar (Rotar (Rotar a)))) = True
esRot360 a = False

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 (Espejar (Espejar a)) = True
esFlip2 a = False

-- Definición de función que aplica un predicado y devuelve 
--  un error indicando fallo o una figura si no hay tal fallo.
data Superfluo = RotacionSuperflua | FlipSuperfluo

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
check :: Dibujo a -> Either [Superfluo] (Dibujo a)
check a
    | (esRot360 a) && (esFlip2 a) = Left (RotacionSuperflua : (FlipSuperfluo : []))
    | ((esRot360 a) && (not (esFlip2 a))) = Left (RotacionSuperflua : [])
    | (not (esRot360 a) && (esFlip2 a)) = Left (FlipSuperfluo : [])
    | otherwise = (Right a)

