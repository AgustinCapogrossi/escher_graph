module Basica.Escher where
import Dibujo
import Interp
import Graphics.Gloss.Data.Picture

type Escher = Bool

dibujo = escher 2 True

interpBas :: Output Escher
interpBas True = trian2
interpBas False = simple blank



-- El dibujoU.
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 p

-- El dibujo t.
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p = Encimar p (Encimar a b)
    where a = Espejar(Rot45 p)
          b = r270 a

-- -- Lado con nivel de detalle.
-- --lado(1, f) = cuarteto(blank, blank, rot(f), f)_
-- --lado(2, f) = cuarteto(lado(1, f), lado(1, f), rot(f), f)_
-- --si lado(3,f) = cuarteto(lado(2,f), lado(2,f),rot(f),f) entonces por la definición dada en el paper de Henderson:
-- --side[n]=quartet(side[n-1],side[n-1],rot(t),t)
-- --Luego por recursion
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 p = Basica False
lado n p = cuarteto(lado (n-1) p) (lado (n-1) p) (Rotar p) (p)
-- 
-- -- Esquina con nivel de detalle en base a la figura p.
-- --esquina(1, f) = cuarteto(blank, blank, blank, dibujoU(p))_
-- --esquina(2, f) = cuarteto(esquina(1, f), lado(1, f), rot(lado(1, f)), dibujoU(f))_
-- --En el paper:
-- --corner[n]=quartet(corner[n-1],side,rot(side),u)
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 p = Basica False
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) (dibujoT p)) (Rotar(lado (n-1) (dibujoT p))) (dibujoU p)
-- 
-- 

noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r)) (Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (Juntar 1 2 v (Juntar 1 1 w x)))
-- 
-- -- El dibujo de Escher:
--escher :: Int -> Escher -> Dibujo Escher
--escher n p = noneto (esquina n p') (lado n (dibujoT p')) --(Rotar(Rotar(Rotar(esquina n p')))) (Rotar(lado n (dibujoT p'))) (dibujoU p') (Rotar(Rotar(Rotar(lado n (dibujoT p'))))) (Rotar(esquina n p')) (Rotar(Rotar(lado n (dibujoT p')))) (Rotar(Rotar(esquina n p')))
--    where p' = Basica p

escher :: Int -> Escher -> Dibujo Escher
escher n p = noneto (esquina n p') (lado n (dibujoT p')) (Rotar(Rotar(Rotar(esquina n p')))) (Rotar(lado n (dibujoT p'))) (dibujoU p') (Rotar(Rotar(Rotar(lado n (dibujoT p'))))) (Rotar(esquina n p')) (Rotar(Rotar(lado n (dibujoT p')))) (Rotar(Rotar(esquina n p')))
    where p' = Basica p    

