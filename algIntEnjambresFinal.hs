import System.Random

--ALGORITMO CCA


{-Para comenzar el algoritmo distribuimos una serie de abejas por un espacio, analiticamente tendremos una region de R2 y cada abeja 
correspondera con una posicion del mismo, el espacio esta calificado segun la calidad del alimento en cada posicion esta asignacion 
corresponde con el valor de una determinada funcion de R2 en R, el objetivo del algoritmo es maximizar dicha funcion consiguiendo la posicion 
mas optima para la busqueda del alimento. 
Mas concretamente inicializaremos con un numero n de abejas en el espacio posicionadas de forma arbitraria, y en cada iteracion las moveremos o no 
segun una formula V_ik= X_ik + phi_ik * (X_ik-X_jk) donde V_ik es la nueva posicion candidata, phi_ik es un numero aleatorio en [-1,1], X_ik es la posicion actual 
y X_jk es una posicion arbitraria del enjambre distinta de la actual, dada esta nueva posicion candidata, en caso de tener un valor mejor que 
el actual moveremos la posicion del enjambre, en caso contrario mantenemos la misma, hay un parametro numCiclo encargado de descartar una posicion
si se ha quedado "estancado" en ella tras pasar numCiclos iteraciones, en tal caso pasaremos a otra posicion arbitraria de la region de estudio. -}


{-En primer lugar definimos los tipos que vamos a usar:
Posicion: tupla de reales con la posicion actual de la abeja, el valor asociado a esa posicion (calidad del nectar), 
y el numero de iteraciones que lleva el algoritmo "estancado" en esa posicion-}
data Posicion = S ((Float,Float), Float, Int) --Posicion de una abeja, calificacion de su nectar y guardamos el numero de iteraciones en la misma posicion
    deriving (Read, Show)
type Numero_abejas = Int
type NumCiclo = Int
type Enjambre = [Posicion]
type Parada = Int 
type Semilla = Int
type Termino = Int
type Ejex = [Int] --Corresponde con el rango en el eje x
type Ejey = [Int] --Corresponde con el rango en el eje y
type Anterior = Float 


--Numeros aleatorios 
{-Necesitaremos tener una serie de numeros aleatorios, en primer lugar un numero aleatorio en el rango (a,b) con a,b reales, que correspondera con la funcion
aleatorioi_j que requiere como parametros de entrada la Semilla, el termino (que simplemente determinara elementos distintos aleatorios en ese rango para la misma semilla)
y el rango entre lo reales (a,b)-}
aleatorioi_j :: Semilla -> Termino -> Int -> Int -> Float 
aleatorioi_j q p s t = (((randomRs (0,1) gen)!!p)+(fromIntegral ((randomRs (s,(t-1)) gen)!!p)))
    where gen = mkStdGen q 
--La siguiente funcion nos permitira obtener un indice entero para la eleccion de X_jk en la formula  
num_aleatorioj :: Semilla -> Termino -> Int ->  Int   
num_aleatorioj q p t =((randomRs (0,t) gen)!!p)  
    where gen = mkStdGen q

--Inicializacion
{-Vamos a inicializar un enjambre que devolvera una lista de posiciones, para ello introduciremos como parametros la funcion que determinara el 
valor de cada posicion (la que pretendemos maximizar), la semilla para los numeros aleatorios, y la region del plano donde delimitaremos la busqueda
de la solucion, para esta habra que dar el intervalo del ejex (Ejemplo: [2,4]) y el intervalo del ejey (Ejemplo [1,2]) quedando como region su 
producto cartesiano-}

enjambre :: ((Float,Float) -> Float) -> Semilla -> Ejex -> Ejey -> Numero_abejas -> Enjambre
enjambre f q xs ys n 
    |n==1 = [S (((aleatorioi_j q 0 u v),(aleatorioi_j q 1 x y)), (f((aleatorioi_j q 0 u v),(aleatorioi_j q 1 x y))),1)]
    |otherwise = [S (((aleatorioi_j q (2*n) u v),(aleatorioi_j q (2*n+1) x y)), (f((aleatorioi_j q (2*n) u v),(aleatorioi_j q (2*n+1) x y))),1)] ++ (enjambre f q xs ys (n-1))
        where u = (xs!!0)
              v = (xs!!1)
              x = (ys!!0)
              y = (ys!!1)
              
--Iteracion para una posicion 
--Obs: Para cada iteracion vamos a tener una semilla distinta, de esta forma en cada iteracion manejaremos listas de numeros aleatorios distintos (esto se implementa en la funcion algoritmo).
iteracionA :: ((Float,Float)->Float) -> Semilla -> Ejex -> Ejey -> Enjambre -> NumCiclo -> Posicion -> Posicion
iteracionA f q xs ys zs s (S ((e,d), n, i)) 
    |i==s = (S (((aleatorioi_j (abs (q+(round d)+(round n))) 0 u v),(aleatorioi_j (abs (q+(round d)+(round n))) 1 x y)), (f ((aleatorioi_j (abs (q+(round d)+(round n))) 0 u v),(aleatorioi_j (abs (q+(round d)+(round n))) 1 x y))),1)) --En caso de haber llegado al limite pautado por  NumCiclos pasamos a otra posicion seleccionada de forma aleatoria
    |(f (a,b)) > n = (S ((a,b), (f (a,b)), 1)) --(a,b) posicion vecina tras aplicar la formula, si su valor es mejor, modificamos la posicion
    |otherwise = (S ((e,d), n, (i+1))) --En caso contrario, mantenemos la misma posicion aumentando una unidad el contador de veces que hacemos la iteracion y permanecemos quietos 
        where (a,b) = (posicion_vecina (e,d) q zs xs ys) --De forma externa calculamos la posicion vecina 
              u = (xs!!0)
              v = (xs!!1)
              x = (ys!!0)   
              y = (ys!!1)
              
--Posicion vecina aplica la formula para obtener la nueva posicion 

posicion_vecina :: (Float,Float) -> Semilla -> Enjambre -> Ejex -> Ejey -> (Float,Float) --Necesito la propia posicion actual, el enjambre para seleccionar X_jk, el inicio y el fin para no salirme de la region de estudio
posicion_vecina (x,y) q ys zs ks = (en_el_borde (suma_tuplas (x,y) (pork (aleatorioi_j q (abs (round x)) (-1) 1) (c,d))) zs ks)  
        where num = (num_aleatorioj q 0 ((length ys)-1))
              (c,d)=distintoA (x,y) q ys a b --Con la funcion distinto nos aseguramos escoger una posicion distinta de la actual para la aplicacion de la formula
              a = (x - fst (fst3A (ys!!num)))
              b = (y - snd (fst3A (ys!!num)))
              
distintoA :: (Float,Float) -> Semilla -> Enjambre -> Float -> Float -> (Float,Float) 
distintoA (x,y) q ys c d 
    |c==0 && d==0 = (distintoA (x,y) (q+1) ys e f)
    |otherwise = (c,d) 
        where num = (num_aleatorioj q 0 ((length ys)-1))
              e = (x - fst (fst3A (ys!!num)))
              f = (y - snd (fst3A (ys!!num)))
          
--en_el_borde dada una posicion, determina si una posicion esta en los limites de la region de estudio y en caso de salirse, devuelve la posicion del borde mas proxima 
en_el_borde :: (Float,Float) -> Ejex -> Ejey -> (Float,Float)
en_el_borde (a,b) xs ys 
    |a > v && x<=b && b<=y = (v,b)
    |a < u && x<=b && b<=y = (u,b)
    |u<=a && a<=v && b>y = (a,y)
    |u<=a && a<=v && b<x = (a,x)
    |a > v && b > y = (v,y)
    |a > v && b < x = (v,x)
    |a < u && b < x = (u,x)
    |a < u && b > y = (u,y)
    |otherwise = (a,b)
        where u = fromIntegral (xs!!0)
              v = fromIntegral (xs!!1)
              x = fromIntegral (ys!!0)
              y = fromIntegral (ys!!1)

{-Algoritmo: Aplicamos un numero m de iteraciones al problema inicializado dando como resultado una seleccion probabilistica entre todas las posiciones 
que resultan de finalizar la ultima iteracion, de forma que se asocia a cada posicion una probabilidad siendo esta mayor segun sea mejor su valor, para 
finalmente devolver una de forma aleatorio atendiendo a esas probabilidades -}

--Obs: Para el caso base usamos la semilla q+1 pues q es usada para hacer el enjambre y empleamos una semilla distinta para cada iteracion.
algoritmoA :: ((Float, Float)-> Float) -> Semilla -> Ejex -> Ejey -> Enjambre -> NumCiclo -> Parada -> Posicion
algoritmoA f q xs ys zs n 0 = probabilidadA (q+1) (anadir_intervalo zs)     
algoritmoA f q xs ys zs n m = (algoritmoA f q xs ys (map (iteracionA f (q+m+1) xs ys zs n) zs) n (m-1))

{-Seleccion probabilistica: En caso de tener notas negativas vamos a realizar un reajuste que consistira en determinar el menor valor de la lista de los valores 
de las posiciones, y se lo sumamos a todas las posiciones de forma que quedan todas positivas. Partiendo de esto para asignar las probabilidades, asociaremos a cada posicion un intervalo
que partira del valor acumulado hasta el valor acumulado mas su valor, de esta forma la ultima posicion tendra un intervalo que finalizara con la suma 
total de los valores de todas las posiciones y podremos tomar un valor aleatorio entre 0 y dicha suma total, de forma que nuestra posicion seleccionada
sera aquella cuyo intervalo asociado contiene al numero aleatorio generado-}

 
anadir_intervalo :: Enjambre -> [(Posicion, (Float, Float))] --Esta funcion asocia a cada posicion con su intervalo
anadir_intervalo xs 
    |(fst2 (minimoA xs))>=0 = (anadir_intervalo1 xs 0)  --En caso de que el minimo del enjambre sea mayor que cero no hace falta un reajuste podemos aplicar anidar_intervalo1 con normalidad 
    |otherwise = (corregir (anadir_intervalo1 (sumar_n n xs) 0) n) --En caso contrario aplicamos anadir_intervalo1 realizando el reajuste, para posteriormente suprimir el suplemento artificial inferido en el valor de las posiciones
        where n = 2*(abs (fst2 (minimoA xs)))

anadir_intervalo1 :: Enjambre -> Anterior -> [(Posicion, (Float, Float))] --Se inicializara con Anterior = 0, esta funcion asocia a cada posicion con su intervalo, sin considerar si ha habido un reajuste en cuyo caso las notas estaran aumentadas  
anadir_intervalo1 [S ((x,y),m,i)] n = [((S ((x,y), m,i)),(n,(n+m)))]
anadir_intervalo1 ((S ((x,y),m,i)):xs) n = [((S ((x,y),m,i)),(n, (n+m)))]++(anadir_intervalo1 xs (n+m))

--Suprime en los valores de todas las posiciones la cantidad añadida artificial para el reajuste 
corregir :: [(Posicion, (Float,Float))] -> Float -> [(Posicion, (Float,Float))]
corregir [(S ((x,y),m,i), (s,t))] n = [((S ((x,y), (m-n),i)),(s,t))]
corregir ((S ((x,y),m,i), (s,t)):xs) n = [((S ((x,y),(m-n),i)),(s,t))]++(corregir xs n)



probabilidadA :: Semilla -> [(Posicion, (Float, Float))] -> Posicion 
probabilidadA q [(x, (y,z))] = x   --Si solo queda una posicion esta sera la seleccionada
probabilidadA q (((S ((x,y),m,i)),(s,t)):xs)
    |esta (fromIntegral num) (s,t) = (S ((x,y), m,i))  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidadA q xs  --En caso contrario seguimos buscando
        where num = num_aleatorioj q 1 (round(snd (snd (last xs)))) --Cogemos un numero aleatorio entre 0 y la suma total de todos los valores 
  

minimoA :: Enjambre -> Posicion
minimoA [x] = x
minimoA (x:y:ys)
    |(fst2 x) < (fst2 y) = minimoA (x:ys)
    |otherwise = minimoA (y:ys)

{-Funciones auxiliares-}
--Empleada en la funcion reajuste

sumar_n :: Float -> Enjambre -> Enjambre  
sumar_n n xs = map (sumar_n' n) xs 
sumar_n' :: Float -> Posicion -> Posicion
sumar_n' n (S ((x,y), m, i))= (S ((x,y), (m+n), i))

--Operaciones para la aplicacion de la formula

pork :: Float -> (Float,Float) -> (Float,Float)
pork k (x,y) = (k*x,k*y)

suma_tuplas :: (Float,Float) -> (Float,Float) -> (Float,Float)
suma_tuplas (x,y) (z,k) = (x+z,y+k)

--Determinar si un numero aleatorio seleccionado esta en el correspondiente intervalo para la seleccion probabilistica 

esta :: Float -> (Float,Float) -> Bool 
esta p xs 
    |x<=p && p<y = True
    |p==x && p==y = True 
    |otherwise = False
        where x = (fst xs)
              y = (snd xs)
    
--Extraer valores de Posicion

fst2 :: Posicion -> Float
fst2 (S ((x,y),z,s)) = z

fst3A :: Posicion -> (Float, Float)
fst3A (S ((x,y),z,s)) = (x,y)



--Funciones que probaremos:

func1A :: (Float,Float) -> Float 
func1A (x,y) = x+y

func2A :: (Float,Float) -> Float
func2A (x,y) 
    |x==0 && y ==0=0
    |x==0 = -(y**2)
    |y==0 = x**2
    |otherwise = (((abs x)**2) - ((abs y)**2))
    
func3A :: (Float,Float) -> Float
func3A (x,y) 
    |x==0 && y ==0=0
    |x==0 = (y**2)
    |y==0 = -(x**2)
    |otherwise = ( ((abs y)**2)-((abs x)**2))
    
func4A :: (Float,Float)->Float
func4A (x,y)= -(abs x)- (abs y) - (abs x)*(abs y)

func5A :: (Float,Float)->Float
func5A (x,y) = -((abs (100*(y-(abs x)**2)))**2 - ((abs (x -1))**2))

elegirFA :: Int -> ((Float,Float) -> Float)
elegirFA 1 = func1A
elegirFA 2 = func2A
elegirFA 3 = func3A
elegirFA 4 = func4A
elegirFA 5 = func5A

--Entrada y salida 
abejas :: IO ()
abejas = do putStr "Elija la función que quiere optimizar indicando el número correspondiente: \n"
            putStr "[1] f(x,y) = x+y \n"
            putStr "[2] g(x,y) = (x**2) - (y**2) \n"
            putStr "[3] h(x,y) = (y**2) - (x**2) \n"
            putStr "[4] i(x,y) = -(abs x)- (abs y) - (abs x)*(abs y)) \n"
            putStr "[5] j(x,y) = -((abs (100*(y-(abs x)**2)))**2 - ((abs (x -1))**2)) \n"
            funchar <- getLine
            putStr "Escriba el nombre del fichero que contiene los parámetros: "
            nombreIn <- getLine
            contenido <- readFile nombreIn
            putStr "Dame el nombre del fichero de salida "
            outNombre <- getLine
            putStr "Dame el numero de abejas obreras que conformaran el enjambre "
            obreras <- getLine
            putStr "Dame el numero de ciclos estancado a partir del que se rechaza una posicion y se busca otra aleatoria "
            ciclos <- getLine
            putStr "Dame el numero de iteraciones del algoritmo "
            criterioparada <- getLine
            let func = elegirFA (read funchar :: Int)
                obre = read obreras :: Int
                numCiclos = read ciclos :: Int
                parada = read criterioparada :: Int
                lineas = lines contenido
                seed = read ((words (lineas !! 0) ) !! 1) :: Int 
                ejex = read ((words (lineas !! 1) ) !! 1) ::[Int] 
                ejey = read ((words (lineas !! 2) ) !! 1) ::[Int]
            let texto = "semilla = " ++ show (seed) ++ ",\n" ++"rango del ejex = " ++ show (ejex) ++ ",\n"++"rango del eje y = " ++ show (ejey)++ ",\n" ++ "obreras = " ++ show (obre) ++ ",\n"++ "ciclos = " ++show (numCiclos)++ ",\n" ++ "parada = " ++ show (parada)++ ",\n"++ "La mejor posición es: "++ show ( algoritmoA func seed ejex ejey (enjambre func seed ejex ejey obre) numCiclos parada)
            writeFile outNombre texto 


--ALGORITMO PSO

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

particulas :: IO ()
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
          
--ALGORITMO ACO

{-
En este primer bloque encontramos los tipos de datos que vamos a utilizar en este algoritmo,
    El nodo simplemente va a ser un entero,
    Una arista va a ser: un par de nodos, un peso, la última cantidad de feromonas registradas y probabilidad.
    Un grafo, como su definición formal, será una lista de nodos y una lista de aristas
    Una hormiga va a almacenar un entero que la identifique y las 
        aristas que lleva recorridas en la iteración actual, se vacia en cada nueva iteración.
-}

type Nodo = Int

data Arista = A (Nodo,Nodo) Float Float Float
    deriving(Read,Show)

instance Eq Arista where 
    (==) (A (x,y) z _ _) (A (x',y') z' _ _) = (x==x' && y==y' && z==z')

data Grafo = G [Nodo] [Arista]
    deriving(Read,Show)

data Hormiga = H Int [Arista]
    deriving(Read,Show)

{-
A continuación listamos unas funciones auxiliares que facilitarán el código más adelante
-}
aristas :: Grafo -> [Arista]
aristas (G xs ys) = ys

noRepes :: Eq a => [a] -> [a]
noRepes [] = []
noRepes (x:xs) = x : noRepes (filter (/= x) xs)

nodos :: Arista -> [Nodo]
nodos (A (x,y) _ _ _) = [x,y]

peso :: Arista -> Float
peso (A _ d _ _ ) = d

feromonas :: Arista -> Float 
feromonas (A _ _ f _ ) = f 

probabilidad :: Arista -> Float
probabilidad (A _ _ _  p) = p

inverso :: Float -> Float
inverso x = 1/x

pertenece ::Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x == y = True
    | otherwise = pertenece x ys

distancia :: Hormiga -> Float
distancia (H n xs) = sum (map peso xs)

num_aleatorioh :: Int -> Int -> Float   
num_aleatorioh q p=((randomRs (0.0,1.0) gen)!!p)  
    where gen = mkStdGen q

{- 
Este bloque lo utilizaremos para iniciar el algoritmo, que se encarga de establecer las condiciones iniciales.
    iniciarHormigas toma de dato n y entonces crea n hormigas que todavía no han recorrido ninguna arista.
    iniciarArista toma un par de nodos y un peso y crea una arista con esos nodos, ese peso y las feromonas iniciales.
-}

iniciarHormigas :: Int -> [Hormiga]
iniciarHormigas 0 = []
iniciarHormigas n = (H n []) : iniciarHormigas (n-1)

iniciarArista :: ((Nodo,Nodo) ,Float) -> Arista
iniciarArista ((x,y),p) = A (x,y) p 0.1 0

{- Cálculo de la probabilidad
    El procedimiento que vamos a seguir para calcular las probabilidades es el siguiente:
        1- Seleccionamos un nodo n
        2- Seleccionamos las aristas que se pueden tomar desde n
        3- Se hace el cálculo de las probabilidades de esas aristas
        4- Se selecciona un nuevo nodo n y se vuelve al paso  2
    Hasta que se acaben los nodos
-}

--Paso 2
caminosDesde :: Grafo -> Nodo -> [Arista]
caminosDesde (G _ []) _ = []
caminosDesde (G xs (y:ys)) n
    | (nodos y) !! 0 == n = y: caminosDesde (G xs ys) n
    | otherwise = caminosDesde (G xs ys) n

-- Dada una lista de aristas y una arista que quieres cambiar de probabilidad a una nueva, actualiza esta dentro de la lista
cambiarProb1 :: [Arista] -> (Arista, Float) -> [Arista]
cambiarProb1 [] _ = []
cambiarProb1 ((A (x,y) dis fero prob):ys) (a,p)
    | (A (x,y) dis fero prob) == a = (A (x,y) dis fero p) : ys
    | otherwise = (A (x,y) dis fero prob): cambiarProb1 ys (a,p)

-- cambia varias probabilidades a la vez
cambiarProb :: [Arista] -> [(Arista,Float)] -> [Arista]
cambiarProb xs [] = xs
cambiarProb xs (y:ys) = cambiarProb (cambiarProb1 xs y) ys

-- sustituye una arista actualizadas en una lista nueva
sustituir1 :: [Arista] -> Arista -> [Arista]
sustituir1 [] _ = []
sustituir1 (x:xs) y 
    | x == y = y:xs
    | otherwise = x : sustituir1 xs y

-- sustituye varias aristas actualizadas en una lista nueva
sustituir :: [Arista] -> [Arista] -> [Arista]
sustituir xs [] = xs
sustituir xs (y:ys) = sustituir (sustituir1 xs y) ys

-- Actualiza en un grafo los caminos que salen de un nodo n
probCaminosNodo :: Grafo -> Nodo -> Grafo
probCaminosNodo (G xs ys) n = (G xs sustituidas)
    where posibles = caminosDesde (G xs ys) n
          numeradores =  zipWith (*) (map feromonas posibles) (map (inverso) (map peso posibles))
          denominador = sum numeradores
          fracciones = map (inverso(denominador) *) numeradores
          pares = zip posibles fracciones
          cambiadas = cambiarProb posibles pares
          sustituidas = sustituir ys cambiadas

-- Recorre todos los nodos del grafo aplicando la función anterior
probCaminos :: Grafo -> Int -> Grafo
probCaminos g 0 = g
probCaminos (G xs ys) n = probCaminos ( probCaminosNodo (G xs ys) (xs !! (n-1)) ) (n-1)

{- Cálculo de las feromonas:
    Para este proceso vamos a hacer una función que registe los "Aportes de hormonas" que hace una hormiga en 
un conjunto de aristas y se repetirá con cada hormiga.  luego vamos a multiplicar las anteriores por 0.9 (por la evaporación) 
además de sumarles este nuevo aporte. Con este valor actualizaremos las aristas y posteriormente el grafo.
    -}

-- Calcula el aporte de una hormiga a una arista completa
feroHormigaArista :: Arista -> Hormiga -> Float
feroHormigaArista x (H n zs)
    | pertenece x zs = inverso ( sum (map peso zs) )
    | otherwise = 0
-- Calcula el aporte de una lista de hormigas a una lista de aristas
feroArista ::  [Hormiga] -> Arista -> Float
feroArista ys x = sum (map (feroHormigaArista x) ys)

-- multiplica las feromonas de la iteración anterior por 0.9 y le suma el nuevo aporte, asocia este valor a cada arista
totalFeromonas :: Grafo -> [Hormiga] -> [(Arista,Float)]
totalFeromonas (G xs ys) hs = pares
    where fero =zipWith (+) (map (0.9*) (map feromonas ys)) (map (feroArista hs) ys)
          pares = zip ys fero  

-- análoga a cambiarProb1
cambiarFero1 :: [Arista] -> (Arista, Float) -> [Arista]
cambiarFero1 [] _ = []
cambiarFero1 ((A (x,y) dis fero prob):ys) (a,f)
    | (A (x,y) dis fero prob) == a = (A (x,y) dis f prob) : ys
    | otherwise = (A (x,y) dis fero prob): cambiarFero1 ys (a,f)

-- análoga a cambiarProb
cambiarFero :: [Arista] -> [(Arista,Float)] -> [Arista]
cambiarFero xs [] = xs
cambiarFero xs (y:ys) = cambiarFero (cambiarFero1 xs y) ys 

-- finalmente, actualizamos el grafo
actualizarFeromonas :: Grafo -> [Hormiga] -> Grafo
actualizarFeromonas (G xs ys) hs = G xs actualizadas
    where actualizadas = cambiarFero ys (totalFeromonas (G xs ys) hs)

{- Elegir los caminos:
    Este bloque se dedica a todas las funciones relacionadas con elegir el camino que la hormiga va a recorrer.
-}

--Para determinar el nodo final del Grafo (donde se finalizaran los caminos)
maximo_nodo :: Grafo -> Nodo
maximo_nodo (G xs ys) = foldr1 (max) xs

nodos_hormiga ::  Hormiga -> [Nodo]
nodos_hormiga (H n xs) = noRepes (foldr (++) [] (map nodos1 xs))

nodos1 :: Arista -> [Nodo]
nodos1 (A (x,y) _ _ _) = [x,y]

{-En este bloque definimos el camino que realiza una hormiga partiendo de la posicion inicial 
(que inicializara con la hormiga vacia) hasta el nodo final segun las probabilidades registradas en el grafo, 
la funcion principal para este proceso es mover_hormiga, para ello implementamos la funcion elegir camino que determinara 
dado un nodo las posibles aristas por las que se puede mover (esencialmente todas las que parten de dicho nodo salvo las que vuelvan a la posicion de la iteracion anterior, este caso se suprime mediante
la funcion quitar_acabados_anterior), posteriormente añade a cada una de esas aristas un intervalo con amplitud la 
determinada por las probabilidades asociadas a esa arista segun el grafo (implementado en el bloque anterior) esto 
se realiza en anadir_intervalo, a continuacion probabilidad2 elige mediante un numero aleatorio entre 0 y 1 que arista se 
escoge segun si esta o no en el intervalo asociado con la funcion anterior, siendo finalmente esta la siguiente arista escogida 
para formar el camino. Para concluir basta definir mover_hormiga que junto con la funcion anterior va concatenando las aristas 
partiendo del nodo 1 hasta llegar a una arista que finalice en el nodo final, que hemos predefinido 
sera el mayor valor entre los nodos. -}

anadir_intervaloH :: [Arista] -> Float -> [(Arista, (Float, Float))] 
anadir_intervaloH [] _ = []
anadir_intervaloH ((A (x,y) d f p):xs) n = [((A (x,y) d f p),(n, (n+p)))]++(anadir_intervaloH xs (n+p))

probabilidad2 :: [(Arista, (Float, Float))] -> Float -> Arista -- Se inicializara con anadir_intervalo (caminosDesde xs 1) 
probabilidad2 (((A (x,y) d f p),(s,t)):xs) num
    |esta (num) (s,t) = (A (x,y) d f p)  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidad2 xs num --En caso contrario seguimos buscando

quitar_acabados_anterior :: Nodo -> [Arista] -> [Arista] --Esta funcion la aplicaremos en elegir camino para que no pueda volver al nodo anteorior 
quitar_acabados_anterior n [] = []
quitar_acabados_anterior n xs = filter (distinto n) xs

distinto :: Nodo -> Arista -> Bool
distinto n (A (x,y) p d f)
    |n==y = False
    |otherwise = True 


elegir_camino :: Int -> Nodo -> Nodo -> Grafo -> Arista --Dado un nodo determina mediante la seleccion probabilistica que arista sera la siguiente en seleccionar 
elegir_camino q m x xs = probabilidad2 (anadir_intervaloH (quitar_acabados_anterior m (caminosDesde xs x)) 0) num
    where num = num_aleatorioh q x

mover_hormiga :: Hormiga -> Grafo -> Hormiga 
mover_hormiga (H n []) ys = mover_hormiga (H n [elegir_camino n 1 1 ys]) ys
mover_hormiga (H k xs) ys 
    |n == (maximo_nodo ys) = (H k xs) --Si el maximo nodo de la hormiga coincide con el maximo nodo del grafo significa que el camino de la hormiga ya ha terminado            
    |otherwise = mover_hormiga (H k zs) ys
        where n = foldr1 max (nodos_hormiga (H k xs)) 
              m = (nodos1 (xs!!0))!!0
              zs = ((elegir_camino (k+(length xs)-1) m n ys):xs)

{- Algoritmo:
    Ahora que disponemos de las funciones necesarias procedemos a "montar" el algoritmo, de la siguiente manera. Para cada
iteración se realizarán los siguientes pasos:
    1.- Calcula las probabilidades y se asocia a la arista
    2.- Mueve todas las hormigas regustrando en ellas el camino que recorren
    3.- Actualiza las feromonas de las aristas
-}

algoritmo :: Grafo -> [Hormiga] ->  Int -> [Hormiga]
algoritmo _ hs 0 = hs -- cuando acabe las iteraciones devuelve las hormigas
algoritmo (G xs ys) hs n = algoritmo g'' caminos (n-1)  --aplico a grafo actualizado y hormigas actualizadas
    where g' = probCaminos (G xs ys) (length xs) -- asigno probabilidades
          caminos = map ((flip mover_hormiga) g') (iniciarHormigas (length hs)) -- muevo hormigas
          g'' = actualizarFeromonas g' hs -- actualizo feromonas

--Decido cual ha sido el mejor camino de mi lista de hormigas
mejorCamino :: [Hormiga] -> Hormiga
mejorCamino hs = mejorHormiga
    where distancias = map distancia hs
          pares = zip hs distancias -- asigno cada hormiga a su distancia recorrida
          mejor = minimo distancias -- me quedo con el minimo
          mejorHormiga = compararEvaluado pares mejor -- obtengo la hormiga asignada a este recorrido

{- Entrada y salida:
    En este último bloque nos ocupamos de la entrada y salida, tanto preparando nuestros datos para que sean legibles
y cómodos en un fichero como de la función principal que se usará para utilizar el algoritmo
-}

-- Una manera mejor de ver un grafo en un archivo por si se necesitase
texto :: Grafo -> String
texto (G _ []) = ""
texto (G xs (y:ys)) = show y ++ "\n" ++texto (G xs ys)

leerArista :: String -> ((Nodo,Nodo), Float)
leerArista x = read x :: ((Nodo,Nodo), Float)

leerHormiga :: String -> Hormiga
leerHormiga x = read x :: Hormiga

textoHormigas :: [Hormiga] -> String 
textoHormigas [] = ""
textoHormigas (x:xs) = show x ++ "\n" ++ textoHormigas xs 

resumen :: Hormiga -> String 
resumen (H n xs) = "El mejor camino es " ++  camino ++ " cuya distancia es " ++ show (distancia (H n xs))
    where camino = unwords (map show (map nodos (reverse xs)) )

hormigas :: IO ()
hormigas = do 
             putStr "Escriba el nombre del fichero que contiene las aristas: "
             ficheroAristas <- getLine
             putStr "Escriba el nombre del fichero que contiene los nodos: "
             ficheroNodos <- getLine
             putStr "Escriba el número de iteraciones: "
             nChar <- getLine
             putStr "Escriba el número de hormigas: "
             mChar <- getLine
             putStr "Escriba el nombre del fichero de salida: "
             nombreOut <- getLine
             nodoChar <- readFile ficheroNodos
             aristaChar <- readFile ficheroAristas
             let nodos = read nodoChar :: [Nodo]
                 lineasA = lines aristaChar
                 aristas1 = map leerArista lineasA 
                 aristas = map iniciarArista aristas1
                 grafo = G nodos aristas
                 n = read nChar :: Int
                 m = read mChar :: Int 
                 resultado = resumen (mejorCamino (algoritmo grafo (iniciarHormigas m) n))
             writeFile nombreOut resultado


--Última función que será el menú con los tres algoritmos:

menu :: IO ()
menu = do putStr "Elija el algoritmo que quiere realizar indicando el número correspondiente: \n"
          putStr "[1] CAA: Algoritmo Colonia de Abejas Artificiales \n"
          putStr "[2] PSO: Particle Swarm Optimization \n"
          putStr "[3] ACO: Ant colony optimization \n"
          putStr "[4] Salir \n"
          opcion <- getLine
          let entero = read opcion :: Int
          case entero of
              1 -> do abejas
                      menu
              2 -> do particulas
                      menu
              3 -> do hormigas
                      menu
              otherwise -> putStr "Adiós \n"


