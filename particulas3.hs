import System.Random

--Durante este fichero vamos a dividir las funciones en bloques según su importancia o cómo estén relacionadas.

{-En este primero bloque vamos a definir las particulas y algunas funciones que 
nos servirán para manejar este tipo de manera cómoda en el resto de funciones.

Como se puede apreciar el tipo Particula consta de cuatro tuplas las cuales indican posiciones del espacio
de la siguiente manera:
    La primera representa la posición actual.
    La segunda representa la velocidad de la partícula.
    La tercera es la mejor posición que esa partícula ha registrado.
    La cuarta es la mejor posición que se ha registrado de todas las partículas que conforman el enjambre.

La función mínimo se usará evidentemente para ver en que punto de las posiciones actuales está el mínimo.
    -}

data Particula = P (Float,Float,Float) (Float,Float,Float) (Float,Float,Float) (Float,Float,Float)
    deriving(Show,Read)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) 
    | x < minResto = x
    |otherwise = minResto   
    where minResto = minimo xs

--Estas tres funciones nos sirven para "aislar" las componentes cuando lo necesitemos
posicion :: Particula -> (Float,Float,Float)
posicion (P xs ys h g) = xs 

velocidad :: Particula -> (Float,Float,Float)
velocidad (P xs ys h g) = ys

global :: Particula -> (Float,Float,Float)
global (P xs ys h g) = g

--Una adaptación de fst snd para tuplas de 3 elementos
fst3 :: (a,b,c) -> a 
fst3 (x,y,z) = x

snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y

thd3 :: (a,b,c) -> c
thd3 (x,y,z) = z

{-Las tres siguientes funciones sirven para inciar con un a malla uniforme de particulas.

    La primera de ellas, distribuir, lo que hace es dado un segmento delimitado por "l" y "u" 
(Claramente con l<u) distribuye uniformemente n puntos a lo largo del segmento.

    La siguiente, inicializar lo que hace es, dados tres segmentos, uno de cada eje del espacio distribuir uniformemente
n puntos a lo largo de cada segmento y después hacer el producto cartesiano de todos esos puntos. En esta función ls representa
los límites inferiores de cada uno de los segmentos mientras que us representa los limites superiores. Es decir, si
quisiésemos buscar soluciones en el cuboide del espacio [-4,7]x[2,3]x[-12,-5] la entrada de la función recibiría
ls = [-4,2,-12] y us = [7,3,-5].

    Por último inicioPar convierte esta lista de posiciones en una lista de particulas, pero cabe observar que todavía no
constituyen un enjambre ya que cada una tiene como mejor posición global aquella en la que inicia y eso no tiene sentido.
-}
distribuir :: Int -> Float -> Float -> [Float]
distribuir 0 _ _ = []
distribuir n l u = l:(distribuir (n-1) (l+(u-l)/fromIntegral(n+1)) u)

inicializar :: Int -> [Float] -> [Float] -> [(Float,Float,Float)]
inicializar n ls us = [(x,y,z) | x<-xspace, y<-yspace, z<-zspace]
    where xspace = distribuir n (ls!!0) (us!!0)
          yspace = distribuir n (ls!!1) (us!!1)
          zspace = distribuir n (ls!!2) (us!!2)

inicioPar :: [(Float,Float,Float)] -> [Particula]
inicioPar [] = []
inicioPar (x:xs) = (P x (0,0,0) x x) : (inicioPar xs) 

{-El siguiente bloque se podría resumir como: Aquellas funciones que convierten una lista de partículas en un enjambre. Son
aquellas que verifican que las particulas "se comuniquen" para asignar la mejor posición global.

    Primero se crea la función compararEvaluado que lo que hace es: dada una lista de duplas busca en la segunda componente
un cierto valor, y cuando lo encuentra devuelve la primera componente. Esto nos permitirá conocer la posición que asegura la 
mejor posición global de las partículas.

    A continuación cambiarUnaG actualiza la mejor posición global de una partícula.

    Finalmente ponerG recibe una lista de partículas y una función y devuelve un enjambre respecto de esa función, utiliza
las funciones anteriormente mencionadas de este bloque.
-}
compararEvaluado :: Eq a => [(b,a)] ->a -> b
compararEvaluado [x] y = fst x 
compararEvaluado (x:xs) y
        | ( snd x == y ) = fst x
        | otherwise = compararEvaluado xs y

cambiarUnaG ::  (Float,Float,Float) -> Particula -> Particula 
cambiarUnaG x (P pos v h g)= P pos v h x

ponerG :: [Particula] -> ((Float,Float,Float)->Float)-> [Particula]
ponerG xs f = map ( cambiarUnaG mejorPos ) xs
    where mejor = minimo [f pos | pos <- (map posicion xs)]
          mezclados = zip (map posicion xs) (map f (map posicion xs))
          mejorPos = compararEvaluado mezclados mejor


{-Los números aleatorios no son algo que pueda caber en Haskell porque van totalmente en contra de que una misma función
si recibe los mismos parámetros debe devolver el mismo resultado.

Sin embargo podemos generar números  "pseudo-aleatorios" a partir de un generador, esto si puede hacerse ya que si das el 
mismo generados va a devolver los mismos "números aleatorios" pero para qu eno se repita siempre el mismo número en el 
algoritmo lo que hago es coger una lista de n de estos números de manera que no tengan relación entre sí.

Lo que es claro es que realmente cada vez que ejecutemos el algoritmo a cada iteración va a ir asignado uno de estos números
y siempre va a ser ese, ya que no cambia el generador. Pero al menos conseguimos que entre las iteraciones en sí estos números
sean "aleatorios"
-}

numerosAleatorios :: Int -> [Float]
numerosAleatorios n = take n (randomRs (0.0,1.0) gen)
    where gen= mkStdGen 2022


{-
Las siguientes funciones son las que se encargan de aplicar el algoritmo en sí:

    La primera de ellas, como su nombre indica, aplica una iteración del algoritmo a una única particula y es la que
nos sirve de base para la siguiente.

    La segunda función, kpasos, lo que hace es aplicar k iteraciones al conjunto de partículas, haciendo un map de la primera
función y actualizando la mejor posición global en cada una. Podemos observar que en está es donde utilizamos los números 
aleatorios anteriormente mencionados.

-}

iteracion :: ((Float,Float,Float)-> Float) -> Float -> Float -> Float -> Float -> Float -> Particula -> Particula 
iteracion f w phih phig rh rg (P pos v h g)
    | f mov < f g = P mov (v1,v2,v3) mov mov 
    | f mov < f h = P mov (v1,v2,v3) mov g 
    | otherwise = P mov (v1,v2,v3) h g 
    where mov = (fst3 pos + v1, snd3 pos + v2, thd3 pos + v3)
          v1= w*(fst3 v)+phih*rh*(fst3 h - (fst3 pos))+phig*rg*(fst3 g-fst3 pos)
          v2= w*(snd3 v)+phih*rh*(snd3 h - (snd3 pos))+phig*rg*(snd3 g-snd3 pos)
          v3= w*(thd3 v)+phih*rh*(thd3 h - (thd3 pos))+phig*rg*(thd3 g-thd3 pos)

kpasos :: Int -> [Particula] -> ((Float,Float,Float)-> Float) -> Float -> Float -> Float-> [Particula]
kpasos 0 xs f _ _ _ = ponerG xs f 
kpasos k xs f w phih phig= kpasos (k-1) (map (iteracion f w phih phig (aleatorios!!(k-1)) (aleatorios !! (2*k-1))) (ponerG xs f)) f w phih phig
    where aleatorios = numerosAleatorios (2*k) 

{-

    A continuación nos encontramos con el bloque que se va a encargar del output, concretamente del archivo que contendrá un
resumen del resultado de la optimización así como las partículas en la última de las iteraciones.

    particulasString como su nombre indica se encarga de convertir a un string una lista de partículas, concretamente
haciendo que cada una de ellas quede en una linea.

    informe crea el String completo que vamos a poner en el archivo.

-}

particulasString :: [Particula] -> String
particulasString [] = ""
particulasString (x:xs) = show x ++ "\n" ++ (particulasString xs)

informe :: [Particula]-> ((Float,Float,Float)->Float) -> String
informe xs f = resumen1 ++"\n"++resumen2++ "\n" ++ particulasString xs
    where mejorpos = global ( xs!!0)
          resumen1 = "La mejor posición es " ++ show mejorpos ++ " y el valor de la función en este punto es " ++ show (f mejorpos)
          resumen2 = "Las particulas en la última iteración quedan de la siguiente manera: "

{-
    Llegando al final, este bloque contiene varias funcioens de ejemplo para probar el programa, así cómo una función que 
permite elegir la función que queremos optimizar introduciedo un entero, así es cómo se elegirá por pantalla.

-}

func1 :: (Float,Float,Float) -> Float 
func1 (x,y,z) = abs(x*(y-5) + x*sin(y-7)+3) 

func2 :: (Float,Float,Float) -> Float 
func2 (x,y,z) = (x**2)*(y**3)+z**6

func3 :: (Float,Float,Float) -> Float 
func3 (x,y,z) = - ((sin(x+y))**2+(cos(x+z))**2) 

func4 :: (Float,Float,Float) -> Float 
func4 (x,y,z) = x**2 + y**2 + z**2

func5 :: (Float,Float,Float) -> Float
func5 (x,y,z) = abs(x+y+z) + abs(x)*abs(y)*abs(z)

func6 :: (Float,Float,Float) -> Float 
func6 (x,y,z) = x**2 + (x+y)**2 + (x+y+z)**2 

elegirF :: Int -> ((Float,Float,Float) -> Float)
elegirF 1 = func1
elegirF 2 = func2
elegirF 3 = func3 
elegirF 4 = func4 
elegirF 5 = func5 
elegirF 6 = func6

{-
    Por último se encuentra el bloque de entrada/salida que se encargará de pedir por pantalla nombres de ficheros, archivos,
número de iteraciones... Para acabar ejecutando el algoritmo PSO.

-}

main :: IO ()
particulas = do putStr "Elija la función que quiere optimizar indicando el número correspondiente: \n"
          putStr "[1] f(x,y,z) = |x*(y-5) + x*sin(y-7)+z| \n"
          putStr "[2] f(x,y,z) = x^2*y^3+z^6 \n"
          putStr "[3] f(x,y,z) = - ((sin(x+y))^2+(cos(x+z))^2) \n"
          putStr "[4] f(x,y,z) = x^2 + y^2 + z^2 \n"
          putStr "[5] f(x,y,z) = |x+y+z| + |x*y*z| \n"
          putStr "[6] f(x,y,z) = x^2 + (x+y)^2 + (x+y+z)^2 \n"
          funchar <- getLine
          putStr "Escriba el nombre del fichero que contiene los parámetros: "
          nombreIn <- getLine
          contenido <- readFile nombreIn
          putStr "Escriba el nombre del fichero de salida: "
          nombreOut <- getLine 
          putStr "Escriba el numero de pasos que quiere realizar: "
          pasos <- getLine
          putStr "Escriba el indice de particulas, si introduce el número n se distribuirán n^3 particulas: "
          part <- getLine 
          let func = elegirF (read funchar :: Int)
              lineas = lines contenido
              wchar = (words (lineas !! 0) ) !! 1  
              phihchar = (words (lineas !! 1) ) !! 1  
              phigchar = (words (lineas !! 2) ) !! 1  
              steps = read pasos :: Int 
              n = read part :: Int
              ls = read ((words (lineas !! 3) ) !! 1) :: [Float]
              us =  read ((words (lineas !! 4) ) !! 1) :: [Float]
              w = read wchar :: Float
              phih = read phihchar :: Float
              phig = read phigchar :: Float
              texto = informe (kpasos steps (ponerG (inicioPar (inicializar n ls us)) func) func w phih phig) func
          writeFile nombreOut texto 

