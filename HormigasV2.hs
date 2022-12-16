import System.Random
{-
En este primer bloque encontramos los tipos de datos que vamos a utilizar en este algoritmo,
    El nodo simplemente va a ser un entero,
    Una arista va a ser: un par de nodos, un peso, la última cantidad de feromonas registradas y probabilidad.
    Un grafo, como su definición formal, será una lista de nodos y una lista de aristas
    Una hormiga va a almacenar las aristas que lleva recorridas en la iteración actual, se vacia en cada nueva iteración.
-}

type Nodo = Int

data Arista = A (Nodo,Nodo) Float Float Float
    deriving(Read,Show)

instance Eq Arista where 
    (==) (A (x,y) z _ _) (A (x',y') z' _ _) = (x==x' && y==y' && z==z')

data Grafo = G [Nodo] [Arista]
    deriving(Read,Show)

data Hormiga = H [Arista]
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

{- 
Este bloque lo utilizaremos para iniciar el algoritmo, que se encarga de establecer las condiciones iniciales.

    iniciarHormigas toma de dato n y entonces crea n hormigas que todavía no han recorrido ninguna arista.
    iniciarArista toma un par de nodos y un peso y crea una arista con esos nodos, ese peso y las feromonas iniciales.
    iniciarGrafo toma una lista de aristas y crea un grafo a partir de estas.
-}

iniciarHormigas :: Int -> [Hormiga]
iniciarHormigas 0 = []
iniciarHormigas n = (H []) : iniciarHormigas (n-1)

iniciarArista :: ((Nodo,Nodo) ,Float) -> Arista
iniciarArista ((x,y),p) = A (x,y) p 0.1 0

caminosDesde :: Grafo -> Nodo -> [Arista]
caminosDesde (G _ []) _ = []
caminosDesde (G xs (y:ys)) n
    | (nodos y) !! 0 == n = y: caminosDesde (G xs ys) n
    | otherwise = caminosDesde (G xs ys) n

cambiarProb1 :: [Arista] -> (Arista, Float) -> [Arista]
cambiarProb1 [] _ = []
cambiarProb1 ((A (x,y) dis fero prob):ys) (a,p)
    | (A (x,y) dis fero prob) == a = (A (x,y) dis fero p) : ys
    | otherwise = (A (x,y) dis fero prob): cambiarProb1 ys (a,p)

cambiarProb :: [Arista] -> [(Arista,Float)] -> [Arista]
cambiarProb xs [] = xs
cambiarProb xs (y:ys) = cambiarProb (cambiarProb1 xs y) ys

sustituir1 :: [Arista] -> Arista -> [Arista]
sustituir1 [] _ = []
sustituir1 (x:xs) y 
    | x == y = y:xs
    | otherwise = x : sustituir1 xs y

sustituir :: [Arista] -> [Arista] -> [Arista]
sustituir xs [] = xs
sustituir xs (y:ys) = sustituir (sustituir1 xs y) ys


probCaminosNodo :: Grafo -> Nodo -> Grafo
probCaminosNodo (G xs ys) n = (G xs sustituidas)
    where posibles = caminosDesde (G xs ys) n
          numeradores =  zipWith (*) (map feromonas posibles) (map (inverso) (map peso posibles))
          denominador = sum numeradores
          fracciones = map (inverso(denominador) *) numeradores
          pares = zip posibles fracciones
          cambiadas = cambiarProb posibles pares
          sustituidas = sustituir ys cambiadas

probCaminos :: Grafo -> Int -> Grafo
probCaminos g 0 = g
probCaminos (G xs ys) n = probCaminos ( probCaminosNodo (G xs ys) (xs !! (n-1)) ) (n-1)

texto :: Grafo -> String
texto (G _ []) = ""
texto (G xs (y:ys)) = show y ++ "\n" ++texto (G xs ys)

leerArista :: String -> ((Nodo,Nodo), Float)
leerArista x = read x :: ((Nodo,Nodo), Float)

leerHormiga :: String -> Hormiga
leerHormiga x = read x :: Hormiga

feroHormigaArista :: Arista -> Hormiga -> Float
feroHormigaArista x (H zs)
    | pertenece x zs = inverso ( sum (map peso zs) )
    | otherwise = 0

feroArista ::  [Hormiga] -> Arista -> Float
feroArista ys x = sum (map (feroHormigaArista x) ys)

totalFeromonas :: Grafo -> [Hormiga] -> [(Arista,Float)]
totalFeromonas (G xs ys) hs = pares
    where fero =zipWith (+) (map (0.9*) (map feromonas ys)) (map (feroArista hs) ys)
          pares = zip ys fero  

cambiarFero1 :: [Arista] -> (Arista, Float) -> [Arista]
cambiarFero1 [] _ = []
cambiarFero1 ((A (x,y) dis fero prob):ys) (a,f)
    | (A (x,y) dis fero prob) == a = (A (x,y) dis f prob) : ys
    | otherwise = (A (x,y) dis fero prob): cambiarFero1 ys (a,f)

cambiarFero :: [Arista] -> [(Arista,Float)] -> [Arista]
cambiarFero xs [] = xs
cambiarFero xs (y:ys) = cambiarFero (cambiarFero1 xs y) ys 

actualizarFeromonas :: Grafo -> [Hormiga] -> Grafo
actualizarFeromonas (G xs ys) hs = G xs actualizadas
    where actualizadas = cambiarFero ys (totalFeromonas (G xs ys) hs)
--ÁLVARO
num_aleatorioj :: Int -> Int ->  Int   
num_aleatorioj q p=((randomRs (0,1) gen)!!p)  
    where gen = mkStdGen q
--Para determinar el nodo final del Grafo (donde se finalizaran los caminos)
maximo_nodo :: Grafo -> Nodo
maximo_nodo (G xs ys) = foldr1 (max) xs

nodos_hormiga ::  Hormiga -> [Nodo]
nodos_hormiga (H xs) = noRepes (foldr (++) [] (map nodos1 xs))

nodos1 :: Arista -> [Nodo]
nodos1 (A (x,y) _ _ _) = [x,y]

esta :: Float -> (Float,Float) -> Bool 
esta p xs 
    |x<=p && p<y = True
    |p==x && p==y = True 
    |otherwise = False
        where x = (fst xs)
              y = (snd xs)

anadir_intervalo :: [Arista] -> Float -> [(Arista, (Float, Float))] --Se inicializara con Anterior = 0, esta funcion asocia a cada posicion con su intervalo, sin considerar si ha habido un reajuste en cuyo caso las notas estaran aumentadas  
anadir_intervalo [(A (x,y) d f p)] n = [((A (x,y) d f p),(n,(n+p)))]
anadir_intervalo ((A (x,y) d f p):xs) n = [((A (x,y) d f p),(n, (n+p)))]++(anadir_intervalo xs (n+p))

probabilidad2 :: [(Arista, (Float, Float))] -> Arista -- Se inicializara con anadir_intervalo (caminosDesde xs 1) 
probabilidad2 [(x, (y,z))] = x   --Si solo queda una posicion esta sera la seleccionada
probabilidad2 (((A (x,y) d f p),(s,t)):xs)
    |esta (fromIntegral num) (s,t) = (A (x,y) d f p)  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidad2 xs  --En caso contrario seguimos buscando
        where num = num_aleatorioj 2022 (round d ) --Cogemos un numero aleatorio entre 0 y la suma total de todos los valores

quitar_acabados_anterior :: Anterior -> [Arista] -> [Arista] --Esta funcion la aplicaremos en elegir camino para que no pueda volver al nodo anteorior 
quitar_acabados_anterior n [] = []
quitar_acabados_anterior n xs = filter (distinto n) xs
distinto :: Anterior -> Arista -> Bool
distinto n (A (x,y) p d f)
    |n==y = False
    |otherwise = True 
 --Hay que añadir Eq
type Anterior = Nodo --Totalmente prescindible pero para que se entienda 
--REVISAR LA ELECCION DEL NUMERO ALEATORIO 
elegir_camino :: Anterior -> Nodo -> Grafo -> Arista --Dado un nodo determina mediante la seleccion probabilistica que arista sera la siguiente en seleccionar 
elegir_camino m x xs = probabilidad2 (anadir_intervalo (quitar_acabados_anterior m (caminosDesde xs x)) 0)

mover_hormiga :: Hormiga -> Grafo -> Hormiga 
mover_hormiga (H []) ys = mover_hormiga (H [elegir_camino 1 1 ys]) ys
mover_hormiga (H xs) ys 
    |n == (maximo_nodo ys) = (H xs) --Si el maximo nodo de la hormiga coincide con el maximo nodo del grafo significa que el camino de la hormiga ya ha terminado            
    |otherwise = mover_hormiga (H zs) ys
        where n = foldr1 max (nodos_hormiga (H xs)) --Maximo nodo de la hormiga 
              m = (nodos1 (xs!!0))!!0
              zs = ((elegir_camino m n ys):xs)
--

algoritmo :: Grafo -> [Hormiga] ->  Int -> [Hormiga]
algoritmo _ hs 0 = hs
algoritmo (G xs ys) hs n = algoritmo g'' caminos (n-1) 
    where g' = probCaminos (G xs ys) (length xs)
          caminos = map ((flip mover_hormiga) g') (iniciarHormigas (length hs))
          g'' = actualizarFeromonas g' hs

distancia :: Hormiga -> Float
distancia (H xs) = sum (map peso xs)

mejorCamino :: [Hormiga] -> Hormiga
mejorCamino hs = mejorHormiga
    where distancias = map distancia hs
          pares = zip hs distancias
          mejor = minimo distancias
          mejorHormiga = compararEvaluado pares mejor

resumen :: Hormiga -> String 
resumen (H xs) = "El mejor camino es " ++  camino ++ " cuya distancia es " ++ show (distancia (H xs))
    where camino = unwords (map show (map nodos (reverse xs)) )

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) 
    | x < minResto = x
    |otherwise = minResto   
    where minResto = minimo xs

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




{-

actualizarProb :: Grafo -> Grafo

iteracion :: Hormiga -> Grafo -> [Aristas] -- Grafo se va a inicializar con las probabilidades actualizadas

actualizarArista :: [Hormigas] -> Arista -> Arista

mapArista 

actualizarFero

algoritmo :: Cosas 
kpasos n = algoritmo G'' (iniciar n hormigas) 
where G' = actualizarProb G
      caminosRecorridos = map iteracion G' [hormigas]
      G'' actualizarFeromonas
-}

{-
1. Probabilidades
2. moverHormigas
3. feromonas
-}






