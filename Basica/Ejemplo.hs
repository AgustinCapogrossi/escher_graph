module Basica.Ejemplo where
import Dibujo
import Interp

type Basica = ()
--ejemplo :: Dibujo Basica
--ejemplo = ()

ejemplo :: Dibujo Basica
ejemplo = Apilar 1 1 (Rotar (Basica ())) (Basica ())

interpBas :: Output Basica
interpBas () = trian2
