---
Laboratorio de Programación Funcional
---

_Integrantes:_
Capogrossi Agustín (agustin.capogrossi@mi.unc.edu.ar)
Coria Maria Daiana (mariadaiana.coria@mi.unc.edu.ar)
Villalba Palmieri Santiago (santiago.villalba@mi.unc.edu.ar)


---
# Informe #
---
Se nos presentaron problemáticas a la hora de definir las funciones anyDib, allDib, orP y allP, principalmente por la dificultad inicial del uso de funciones lambda para las mismas, a su vez el uso de la función _sem_ como un foldr no se nos ocurrió hasta después de varias consultas a la profesora asignada, una vez nos destrabamos pudimos avanzar casi sin problemas por el resto del archivo _Dibujo.hs_. El módulo Interp.hs fue bastante difícil al principio, probamos de muchas maneras distintas hasta que dimos con el resultado gracias a las multiples consultas. En un principio estábamos intentando aplicar la función sem con parametros lambda para la resolución del mismo, pero finalmente optamos por utilizar las funciones que modifican la ubicación de una imagen para ello.
La figura de Escher no presentó mayores complicaciones, pues era una interpretación de lo dado en el paper de Henderson, pero si tuvimos que alterar partes del código en reiteradas ocasiones para que el dibujo final fuera fiel al dado en el material de origen. 

###1. ¿Por qué están separadas las funcionalidades en los módulos indicados?
El módulo Dibujo.hs se encarga de trabajar sobre la estructura de Dibujo, las maneras de recorrer uno, su semántica y sus tipos, a la vez revisa que un dibujo cumpla un predicado y corrobora que no haya datos superfluos, mientras que el módulo Interp.hs se encarga de la interpretación geométrica y visual de dicho dibujo, sin tener en cuenta la manera en la que el mismo está conformado, trabajando sobre sus vectores para poder cambiar la posición del mismo en un dibujo. Es necesario realizar esta separación de uno con el otro para que la programación funcional se realice mas fácilmente, pues el pase de funciones como parámetros es mas efectivo al tener los tipos de las mismas en claro.
La partición de módulos dada nos parece correcta, si bien la función sem tambien podría ser definida en Interp, es mucho mas efectivo mantenerla en Dibujo, ya que es ahí donde la estructura del Dibujo está definida.

###2. ¿Por qué las funciones básicas no están incluidas en la definición del lenguaje, y en vez es un parámetro del tipo?
Por que los dibujos básicos están definidos con vectores, los cuales son introducidos en interp debido a que son necesarios para la interpretación geométrica de la figura, de estar incluidos en la definición del lenguaje estos formarían parte de los elementos de un Dibujo que puede ser recorrido y no de una representación gráfica del mismo.

###3. Explique cómo hace `Interp.grid` para construir la grilla, explicando cada pedazo de código que interviene.
Grid recibe 4 argumentos, un tamaño, un vector, una separación y una longitud, con estos parámetros construye una grilla de n elementos con separacion sep y longitud l, para ello primero define una serie de lineas paralelas entre si y distanciadas por sep y de longitud l _(hlines v sep l)_ y luego se encarga de que todas formen parte de la misma imagen _(pictures)_, finalmente se asegura de que la grilla tenga n elementos al tener n+1 lineas _(take (n+1))_. Una vez definido el ls, forma una misma imagen _(pictures)_ que contiene al ls y al ls rotado en 90 grados _(rotate 90 ls)_, desplazado _(translate 0 (l*toEnum n) (rotate 90 ls))_ en el vector "y" por _l*toEnum n_ la cual transforma el "n" de tipo _Int_ a tipo _Float_ para poder multiplicarlo por su longitud y de de esta forma se evitar el desfasaje de la grilla.

---
# Bibliografía #
---

* [Aprende Haskell por el bien de todos](http://aprendehaskell.es/main.html).
* [Introducción a Markdown](https://programminghistorian.org/es/lecciones/introduccion-a-markdown).
* [Stack Overflow](https://es.stackoverflow.com/).
* [Documentación de gloss](http://hackage.haskell.org/package/gloss).
* [Hoogle](https://hoogle.haskell.org/).
* [Paper de Henderson](https://cs.famaf.unc.edu.ar/~mpagano/henderson-funcgeo2.pdf).
