module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basica.Ejemplo as E
import qualified Basica.Escher as ES

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }

ej x y = Conf {
                basic = E.interpBas
              , fig = E.ejemplo
              , width = x
              , height = y
              }
              
ej2 x y  = Conf {
  basic = ES.interpBas,
  fig = ES.dibujo,
  width = x,
  height = y
}

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf a) -> IO ()
initial cf = cf >>= \cfg ->
                let x  = width cfg
                    y  = height cfg
                    img = interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
                in display win white img
    where   withGrid p = pictures [p, color grey $ grid 10 (0,0) 100 10]
            grey = makeColorI 120 120 120 120

win = InWindow "Nice Window" (500, 500) (0, 0)
main = initial $ return (ej2 100 100)
