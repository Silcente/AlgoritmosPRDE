import System.Random
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

compararEvaluado :: Eq a => [(b,a)] ->a -> b
compararEvaluado [x] y = fst x 
compararEvaluado (x:xs) y
        | ( snd x == y ) = fst x
        | otherwise = compararEvaluado xs y

num_aleatorioj :: Int -> Int -> Float   
num_aleatorioj q p=((randomRs (0.0,1.0) gen)!!p)  
    where gen = mkStdGen q

distancia :: Hormiga -> Float
distancia (H n xs) = sum (map peso xs)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) 
    | x < minResto = x
    |otherwise = minResto   
    where minResto = minimo xs

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

esta :: Float -> (Float,Float) -> Bool 
esta p xs 
    |x<=p && p<y = True
    |p==x && p==y = True 
    |otherwise = False
        where x = (fst xs)
              y = (snd xs)

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

anadir_intervalo :: [Arista] -> Float -> [(Arista, (Float, Float))] 
anadir_intervalo [] _ = []
anadir_intervalo ((A (x,y) d f p):xs) n = [((A (x,y) d f p),(n, (n+p)))]++(anadir_intervalo xs (n+p))

probabilidad2 :: [(Arista, (Float, Float))] -> Float -> Arista -- Se inicializara con anadir_intervalo (caminosDesde xs 1) 
probabilidad2 (((A (x,y) d f p),(s,t)):xs) num
    |esta (num) (s,t) = (A (x,y) d f p)  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidad2 xs num --En caso contrario seguimos buscando

quitar_acabados_anterior :: Anterior -> [Arista] -> [Arista] --Esta funcion la aplicaremos en elegir camino para que no pueda volver al nodo anteorior 
quitar_acabados_anterior n [] = []
quitar_acabados_anterior n xs = filter (distinto n) xs

distinto :: Anterior -> Arista -> Bool
distinto n (A (x,y) p d f)
    |n==y = False
    |otherwise = True 

type Anterior = Nodo 

elegir_camino :: Int -> Anterior -> Nodo -> Grafo -> Arista --Dado un nodo determina mediante la seleccion probabilistica que arista sera la siguiente en seleccionar 
elegir_camino q m x xs = probabilidad2 (anadir_intervalo (quitar_acabados_anterior m (caminosDesde xs x)) 0) num
    where num = num_aleatorioj q x

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

proceso :: IO ()
proceso = do 
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





