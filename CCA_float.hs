--Algoritmo CCA
import System.Random

{-La idea del algoritmo es distribuir una serie de abejas por un espacio, analiticamente tendremos una region de R2 y cada abeja 
correspondera con una posicion del mismo. El espacio esta calificado segun la calidad del alimento en cada posicion esta asignacion 
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
--Obs: Para cada iteracion vamos a tener una semilla distinta, de esta forma en cada iteracion manejaremos listas de numeros aleatorios distintos 
iteracion :: ((Float,Float)->Float) -> Semilla -> Ejex -> Ejey -> Enjambre -> NumCiclo -> Posicion -> Posicion
iteracion f q xs ys zs s (P ((e,d), n, i)) 
    |i==s = (P (((aleatorioi_j (abs (q+(round d)+(round n))) 0 u v),(aleatorioi_j (abs (q+(round d)+(round n))) 1 x y)), (f ((aleatorioi_j (abs (q+(round d)+(round n))) 0 u v),(aleatorioi_j (abs (q+(round d)+(round n))) 1 x y))),1)) --En caso de haber llegado al limite pautado por  NumCiclos pasamos a otra posicion seleccionada de forma aleatoria
    |(f (a,b)) > n = (P ((a,b), (f (a,b)), 1)) --(a,b) posicion vecina tras aplicar la formula, si su valor es mejor, modificamos la posicion
    |otherwise = (P ((e,d), n, (i+1))) --En caso contrario, mantenemos la misma posicion aumentando una unidad el contador de veces que hacemos la iteracion y permanecemos quietos 
        where (a,b) = (posicion_vecina (e,d) q zs xs ys)
              u = (xs!!0)
              v = (xs!!1)
              x = (ys!!0)   
              y = (ys!!1)
              
--Posicion vecina aplica la formula para obtener la nueva posicion 

posicion_vecina :: (Float,Float) -> Semilla -> Enjambre -> Ejex -> Ejey -> (Float,Float) --Necesito la propia posicion actual, el enjambre X_jk, el inicio y el fin para no salirme de la region de estudio
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

--Obs: Para el caso base usamos la semilla q+1 pues q es usada para hacer el enjambre 
algoritmo :: ((Float, Float)-> Float) -> Semilla -> Ejex -> Ejey -> Enjambre -> NumCiclo -> Parada -> Posicion
algoritmo f q xs ys zs n 0 = probabilidad (q+1) (anadir_intervalo (reajuste zs) 0)     
algoritmo f q xs ys zs n m = (algoritmo f q xs ys (map (iteracion f (q+m+1) xs ys zs n) zs) n (m-1))

{-Seleccion probabilistica: Para evitar el caso donde tenemos notas negativas implementamos la funcion reajuste, que toma el menor valor de la lista 
y se lo suma a todas las posiciones de forma que quedan todas positivas. Partiendo de esto para asignar las probabilidades, asociaremos a cada posicion un intervalo
que partira del valor acumulado hasta el valor acumulado mas su valor, de esta forma la ultima posicion tendra un intervalo que finalizara con la suma 
total de los valores de todas las posiciones y podremos tomar un valor aleatorio entre 0 y dicha suma total, de forma que nuestra posicion seleccionada
sera aquella cuyo intervalo asociado contiene al numero aleatorio generado-}

anadir_intervalo :: Enjambre -> Anterior -> [(Posicion, (Float, Float))] --Se inicializara con Anterior = 0, esta funcion asocia a cada posicion con su intervalo 
anadir_intervalo [P ((x,y),m,i)] n = [((P ((x,y),m,i)),(n,(n+m)))]
anadir_intervalo ((P ((x,y),m,i)):xs) n = [((P ((x,y),m,i)),(n, (n+m)))]++(anadir_intervalo xs (n+m))


probabilidad :: Semilla -> [(Posicion, (Float, Float))] -> Posicion 
probabilidad q [(x, (y,z))] = x   --Si solo queda una posicion esta sera la seleccionada
probabilidad q (((P ((x,y),m,i)),(s,t)):xs)
    |esta2 (fromIntegral num) (s,t) = (P ((x,y),m,i))  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidad q xs  --En caso contrario seguimos buscando
        where num = num_aleatorioj q 1 (round(snd (snd (last xs)))) --Cogemos un numero aleatorio entre 0 y la suma total de todos los valores 
  
reajuste :: Enjambre -> Enjambre --En caso de tener valores negativos, desplazamos todos los valores "hacia la derecha" de forma que todos queden positivos 
reajuste xs
    |(fst2 (minimo xs))>=0 = xs
    |otherwise = (sumar_n n xs)
        where n = 2*(abs (fst2 (minimo xs))) --Obs:Sumamos dos veces el minimo para que al reajustar no sea cero y tambien tenga su intervalo 

minimo :: Enjambre -> Posicion
minimo [x] = x
minimo (x:y:ys)
    |(fst2 x) < (fst2 y) = minimo (x:ys)	
	|otherwise = minimo (y:ys)

{-Funciones auxiliares-}

sumar_n :: Float -> Enjambre -> Enjambre 
sumar_n n [(P ((x,y), m, i))]= [(P ((x,y), (m+n), i))]
sumar_n n ((P ((x,y), m, i)):xs) =  [(P ((x,y), (m+n), i))] ++ (sumar_n n xs)


fst2 :: Posicion -> Float
fst2 (P ((x,y),z,s)) = z

pork :: Float -> (Float,Float) -> (Float,Float)
pork k (x,y) = (k*x,k*y)

suma_tuplas :: (Float,Float) -> (Float,Float) -> (Float,Float)
suma_tuplas (x,y) (z,k) = (x+z,y+k)
 
esta :: Float -> [Int] -> Bool
esta p xs 
    |x<=p && p<y = True
    |p==x && p==y = True 
    |otherwise = False
        where x = fromIntegral (xs!!0)
              y = fromIntegral (xs!!1)

esta2 :: Float -> (Float,Float) -> Bool
esta2 p xs 
    |x<=p && p<y = True
    |p==x && p==y = True 
    |otherwise = False
        where x = (fst xs)
              y = (snd xs)
    
fst3 :: Posicion -> (Float, Float)
fst3 (P ((x,y),z,s)) = (x,y)

esta_dentro :: (Float,Float)-> Ejex-> Ejey -> Bool  
esta_dentro (x,y) xs ys = ((esta x xs) && (esta y ys))

signo2 :: Float -> Float 
signo2 n
    |n<0 = -n
	|otherwise = n 

--Funciones que probaremos:
func1 :: (Float,Float) -> Float 
func1 (x,y) = x+y

func2 :: (Float,Float)->Float
func2 (x,y) 
    |x==0 || y == 0 = 20000000000000
    |otherwise = (1/x)+(1/y)

func3 :: (Float,Float) -> Float
func3 (x,y) = x*(sin (abs x))+ y*(sin (abs y))

func4 :: (Float,Float) -> Float
func4 (x,y) 
    |x==0 && y ==0=0
    |x==0 = -(y**2)
    |y==0 = x**2
    |otherwise = ((x**2) - (y**2))
func5 :: (Float,Float) -> Float
func5 (x,y) 
    |x==0 && y ==0=0
    |x==0 = (y**2)
    |y==0 = -(x**2)
    |otherwise = ( (y**2)-(x**2))


--Entrada y salida 
procesa :: IO ()
procesa = do putStr "Dame el nombre del fichero de salida "
             outNombre <- getLine
             putStr "Dame la semilla que vamos a emplear "
             semilla <- getLine
             let seed = read semilla :: Int
             putStr "Dame el rango de Ejex que vamos a emplear "
             ejex2 <- getLine
             let ejex = read ejex2 :: [Int]
             putStr "Dame el rango de Ejey que vamos a emplear "
             ejey2 <- getLine
             let ejey = read ejey2 :: [Int]
             putStr "Dame el numero de abejas obreras que conformaran el enjambre "
             obreras <- getLine
             let obre = read obreras :: Int
             putStr "Dame el numero de ciclos estancado a partir del que se rechaza una posicion y se busca otra aleatoria "
             ciclos <- getLine
             let numCiclos = read ciclos :: Int
             putStr "Dame el numero de iteraciones del algoritmo "
             criterioparada <- getLine
             let parada = read criterioparada :: Int
             let texto = "semilla = " ++ show (seed) ++ ", rango del ejex = " ++ show (ejex) ++ ", rango del eje y = " ++ show (ejey) ++ ", obreras = " ++ show (obre) ++ ", ciclos = " ++show (numCiclos) ++ ", parada = " ++ show (parada)++ " "++ show ( algoritmo func1 seed ejex ejey (enjambre func1 seed ejex ejey obre) numCiclos parada)
             writeFile outNombre texto 

    
--No acepta casos grandes 
--Obs: Puede ampliarse facilmente al R3
--Mejorar todas las funciones auxiliares con orden superior
--func4 y func5 no funcionan con numeros negativos 
--Probar mas funciones 

          
--algoritmo func1 2000 [0,10] [0,10] (enjambre func1 2000 [0,10] [0,10] 20) 5 20
--iteracionk func1 2000 [0,10] [0,10] (enjambre func1 2000 [0,10] [0,10] 40) 5 (P ((9.003588,7.217577),16.22116,1)) 3
--posicion_vecina (7.817907,5.587888) 2000 (enjambre func1 2000 [0,10] [0,10] 40) [0,10] [0,10] 
--algoritmo func1 2000 [0,10] [0,10] (enjambre func1 2000 [0,10] [0,10] 20) 100 200
--algoritmo func1 2000 [0,10] [0,10] (enjambre func1 2000 [0,10] [0,10] 20) 40 200
--suma_tuplas (10.0,8.913813) (pork (aleatorioi_j (abs (2012)) 0 (-1) 1) (fst (fst3 ((enjambre func4 2000 [0,10] [0,10] 3)!!((num_aleatorioj q 0 ((length (enjambre func4 2000 [0,10] [0,10] 3))-1))))),snd (fst3 (ys!!((num_aleatorioj q 0 ((length (enjambre func4 2000 [0,10] [0,10] 3))-1)))))))
--suma_tuplas (10.0,8.913813) (pork (aleatorioi_j (abs (2012)) 0 (-1) 1) (fst (fst3 ((enjambre func4 2000 [0,10] [0,10] 3)!!((num_aleatorioj 2012 0 ((length (enjambre func4 2000 [0,10] [0,10] 3))-1))))),snd (fst3 ((enjambre func4 2000 [0,10] [0,10] 3)!!((num_aleatorioj 2012 0 ((length (enjambre func4 2000 [0,10] [0,10] 3))-1)))))))
--((fst (fst3 ((enjambre func4 2000 [0,10] [0,10] 3)!!(num_aleatorioj 2003 0 (length (enjambre func4 2000 [0,10] [0,10] 3)-1))))),snd (fst3 ((enjambre func4 2000 [0,10] [0,10] 3)!!((num_aleatorioj 2003 0 ((length (enjambre func4 2000 [0,10] [0,10] 3))-1))))))
--i distinto de j
--algoritmo func4 2000 [0,10] [0,10] (enjambre func4 2000 [0,10] [0,10] 20) 40 100 
--algoritmo func1 2000 [0,10] [0,10] (enjambre func1 2000 [0,10] [0,10] 20) 40 90
--algoritmo func1 2000 [0,10] [0,10] (enjambre func1 2000 [0,10] [0,10] 20) 20 30
--algoritmo func4 2000 [0,10] [0,10] (enjambre func4 2000 [0,10] [0,10] 20) 20 30
--algoritmo func5 2000 [0,10] [0,10] (enjambre func5 2000 [0,10] [0,10] 20) 20 30
-- algoritmo func5 2000 [0,10] [0,10] (enjambre func5 2000 [0,10] [0,10] 20) 20 40
--algoritmo func1 2000 [-10,10] [-10,10] (enjambre func1 2000 [-10,10] [-10,10] 20) 20 40



          