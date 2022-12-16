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

proceso :: IO ()
proceso = do 
             putStr "Escriba el nombre del fichero que contiene las aristas: "
             ficheroAristas <- getLine
             putStr "Escriba el nombre del fichero que contiene los nodos: "
             ficheroNodos <- getLine
             putStr "Escriba el nombre del fichero de salida: "
             nombreOut <- getLine
             nodoChar <- readFile ficheroNodos
             aristaChar <- readFile ficheroAristas
             let nodos = read nodoChar :: [Nodo]
                 lineas = lines aristaChar
                 aristas1 = map leerArista lineas 
                 aristas = map iniciarArista aristas1
                 grafo = G nodos aristas
                 resultado = texto(probCaminos grafo (length nodos))
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






