--Algoritmo CCA
import System.Random

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
data Posicion = P ((Float,Float), Float, Int) --Posicion de una abeja, calificacion de su nectar y guardamos el numero de iteraciones en la misma posicion
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
    |n==1 = [P (((aleatorioi_j q 0 u v),(aleatorioi_j q 1 x y)), (f((aleatorioi_j q 0 u v),(aleatorioi_j q 1 x y))),1)]
    |otherwise = [P (((aleatorioi_j q (2*n) u v),(aleatorioi_j q (2*n+1) x y)), (f((aleatorioi_j q (2*n) u v),(aleatorioi_j q (2*n+1) x y))),1)] ++ (enjambre f q xs ys (n-1))
        where u = (xs!!0)
              v = (xs!!1)
              x = (ys!!0)
              y = (ys!!1)
              
--Iteracion para una posicion 
--Obs: Para cada iteracion vamos a tener una semilla distinta, de esta forma en cada iteracion manejaremos listas de numeros aleatorios distintos (esto se implementa en la funcion algoritmo).
iteracion :: ((Float,Float)->Float) -> Semilla -> Ejex -> Ejey -> Enjambre -> NumCiclo -> Posicion -> Posicion
iteracion f q xs ys zs s (P ((e,d), n, i)) 
    |i==s = (P (((aleatorioi_j (abs (q+(round d)+(round n))) 0 u v),(aleatorioi_j (abs (q+(round d)+(round n))) 1 x y)), (f ((aleatorioi_j (abs (q+(round d)+(round n))) 0 u v),(aleatorioi_j (abs (q+(round d)+(round n))) 1 x y))),1)) --En caso de haber llegado al limite pautado por  NumCiclos pasamos a otra posicion seleccionada de forma aleatoria
    |(f (a,b)) > n = (P ((a,b), (f (a,b)), 1)) --(a,b) posicion vecina tras aplicar la formula, si su valor es mejor, modificamos la posicion
    |otherwise = (P ((e,d), n, (i+1))) --En caso contrario, mantenemos la misma posicion aumentando una unidad el contador de veces que hacemos la iteracion y permanecemos quietos 
        where (a,b) = (posicion_vecina (e,d) q zs xs ys) --De forma externa calculamos la posicion vecina 
              u = (xs!!0)
              v = (xs!!1)
              x = (ys!!0)   
              y = (ys!!1)
              
--Posicion vecina aplica la formula para obtener la nueva posicion 

posicion_vecina :: (Float,Float) -> Semilla -> Enjambre -> Ejex -> Ejey -> (Float,Float) --Necesito la propia posicion actual, el enjambre para seleccionar X_jk, el inicio y el fin para no salirme de la region de estudio
posicion_vecina (x,y) q ys zs ks = (en_el_borde (suma_tuplas (x,y) (pork (aleatorioi_j q (abs (round x)) (-1) 1) (c,d))) zs ks)  
        where num = (num_aleatorioj q 0 ((length ys)-1))
              c = (x - fst (fst3 (ys!!num)))
              d = (y - snd (fst3 (ys!!num)))
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
algoritmo :: ((Float, Float)-> Float) -> Semilla -> Ejex -> Ejey -> Enjambre -> NumCiclo -> Parada -> Posicion
algoritmo f q xs ys zs n 0 = probabilidad (q+1) (anadir_intervalo zs)     
algoritmo f q xs ys zs n m = (algoritmo f q xs ys (map (iteracion f (q+m+1) xs ys zs n) zs) n (m-1))

{-Seleccion probabilistica: En caso de tener notas negativas vamos a realizar un reajuste que consistira en determinar el menor valor de la lista de los valores 
de las posiciones, y se lo sumamos a todas las posiciones de forma que quedan todas positivas. Partiendo de esto para asignar las probabilidades, asociaremos a cada posicion un intervalo
que partira del valor acumulado hasta el valor acumulado mas su valor, de esta forma la ultima posicion tendra un intervalo que finalizara con la suma 
total de los valores de todas las posiciones y podremos tomar un valor aleatorio entre 0 y dicha suma total, de forma que nuestra posicion seleccionada
sera aquella cuyo intervalo asociado contiene al numero aleatorio generado-}

 
anadir_intervalo :: Enjambre -> [(Posicion, (Float, Float))] --Esta funcion asocia a cada posicion con su intervalo
anadir_intervalo xs 
    |(fst2 (minimo xs))>=0 = (anadir_intervalo1 xs 0)  --En caso de que el minimo del enjambre sea mayor que cero no hace falta un reajuste podemos aplicar anidar_intervalo1 con normalidad 
    |otherwise = (corregir (anadir_intervalo1 (sumar_n n xs) 0) n) --En caso contrario aplicamos anadir_intervalo1 realizando el reajuste, para posteriormente suprimir el suplemento artificial inferido en el valor de las posiciones
        where n = 2*(abs (fst2 (minimo xs)))

anadir_intervalo1 :: Enjambre -> Anterior -> [(Posicion, (Float, Float))] --Se inicializara con Anterior = 0, esta funcion asocia a cada posicion con su intervalo, sin considerar si ha habido un reajuste en cuyo caso las notas estaran aumentadas  
anadir_intervalo1 [P ((x,y),m,i)] n = [((P ((x,y), m,i)),(n,(n+m)))]
anadir_intervalo1 ((P ((x,y),m,i)):xs) n = [((P ((x,y),m,i)),(n, (n+m)))]++(anadir_intervalo1 xs (n+m))

--Suprime en los valores de todas las posiciones la cantidad añadida artificial para el reajuste 
corregir :: [(Posicion, (Float,Float))] -> Float -> [(Posicion, (Float,Float))]
corregir [(P ((x,y),m,i), (s,t))] n = [((P ((x,y), (m-n),i)),(s,t))]
corregir ((P ((x,y),m,i), (s,t)):xs) n = [((P ((x,y),(m-n),i)),(s,t))]++(corregir xs n)



probabilidad :: Semilla -> [(Posicion, (Float, Float))] -> Posicion 
probabilidad q [(x, (y,z))] = x   --Si solo queda una posicion esta sera la seleccionada
probabilidad q (((P ((x,y),m,i)),(s,t)):xs)
    |esta (fromIntegral num) (s,t) = (P ((x,y), m,i))  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidad q xs  --En caso contrario seguimos buscando
        where num = num_aleatorioj q 1 (round(snd (snd (last xs)))) --Cogemos un numero aleatorio entre 0 y la suma total de todos los valores 
  

minimo :: Enjambre -> Posicion
minimo [x] = x
minimo (x:y:ys)
    |(fst2 x) < (fst2 y) = minimo (x:ys)	
	|otherwise = minimo (y:ys)

{-Funciones auxiliares-}
--Empleada en la funcion reajuste

sumar_n :: Float -> Enjambre -> Enjambre  
sumar_n n xs = map (sumar_n' n) xs 
sumar_n' :: Float -> Posicion -> Posicion
sumar_n' n (P ((x,y), m, i))= (P ((x,y), (m+n), i))

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
fst2 (P ((x,y),z,s)) = z

fst3 :: Posicion -> (Float, Float)
fst3 (P ((x,y),z,s)) = (x,y)



--Funciones que probaremos:

func1 :: (Float,Float) -> Float 
func1 (x,y) = x+y

func2 :: (Float,Float) -> Float
func2 (x,y) 
    |x==0 && y ==0=0
    |x==0 = -(y**2)
    |y==0 = x**2
    |otherwise = (((abs x)**2) - ((abs y)**2))
    
func3 :: (Float,Float) -> Float
func3 (x,y) 
    |x==0 && y ==0=0
    |x==0 = (y**2)
    |y==0 = -(x**2)
    |otherwise = ( ((abs y)**2)-((abs x)**2))
    
func4 :: (Float,Float)->Float
func4 (x,y)= -(abs x)- (abs y) - (abs x)*(abs y)

func5 :: (Float,Float)->Float
func5 (x,y) = -((abs (100*(y-(abs x)**2)))**2 - ((abs (x -1))**2))

elegirF :: Int -> ((Float,Float) -> Float)
elegirF 1 = func1
elegirF 2 = func2
elegirF 3 = func3
elegirF 4 = func4
elegirF 5 = func5

--Entrada y salida 
procesa :: IO ()
procesa = do putStr "Elija la función que quiere optimizar indicando el número correspondiente: \n"
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
             let func = elegirF (read funchar :: Int)
                 obre = read obreras :: Int
                 numCiclos = read ciclos :: Int
                 parada = read criterioparada :: Int
                 lineas = lines contenido
                 seed = read ((words (lineas !! 0) ) !! 1) :: Int 
                 ejex = read ((words (lineas !! 1) ) !! 1) ::[Int] 
                 ejey = read ((words (lineas !! 2) ) !! 1) ::[Int]
             let texto = "semilla = " ++ show (seed) ++ ", rango del ejex = " ++ show (ejex) ++ ", rango del eje y = " ++ show (ejey) ++ ", obreras = " ++ show (obre) ++ ", ciclos = " ++show (numCiclos) ++ ", parada = " ++ show (parada)++ " "++ show ( algoritmo func seed ejex ejey (enjambre func seed ejex ejey obre) numCiclos parada)
             writeFile outNombre texto 

          
    






          