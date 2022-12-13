import System.Random 

type Nodo = Int
data Arista = A (Nodo, Nodo) Float deriving (Show, Read)
data Grafo = G [Nodo] [Arista] deriving (Show, Read)

instance Eq Arista where
  (==) (A (x,y) z) (A (x',y') z') = (x==x' && y==y' && z==z') || (x==y' && y==x' && z==z')

--EJEMPLOS DE GRAFOS:
--G [1,2,3,4] [(A (1,2) 2), (A (1,3) 3), (A (2,4) 3), (A (3,4) 4)] 
--G [1,2,3,4,5] [(A (1,2) 12),(A (1,3) 34),(A (1,5) 78),(A (2,4) 55),(A (2,5) 32),(A (3,4) 61),(A (3,5) 44),(A (4,5) 93)]

uno :: Arista -> Nodo
uno (A (x, y) z) = x

dos :: Arista -> Nodo
dos (A (x, y) z) = y

distancia :: Arista -> Float
distancia (A (x, y) z) = z


distanciaCamino :: [Arista] -> Float
distanciaCamino = foldr ((+) . distancia) 0

distanciasPosibles :: [[Arista]] -> [Float]
distanciasPosibles = map distanciaCamino

inverso :: Float -> Float
inverso x = 1/x

inversoDistancias :: [Float] -> [Float]
inversoDistancias = map inverso

delete :: Eq a => a -> [a] -> [a]
delete m [] = []
delete m (x:xs) 
    | x == m = delete m xs 
    | otherwise = x : (delete m xs)

--ITERACIÓN 1

probabilidadAristaParte1 :: Grafo -> Arista -> Float --la necesito para calcular la probabilidad de una arista
probabilidadAristaParte1 grafo arista  = inverso (distancia arista) * 0.1 --0.1 son las feromonas en una arista cuando no la ha recorrido ninguna hormiga inicialmente

listasProbabilidad :: Grafo -> [Arista] -> [Float] --lista de porbablidadesparte1 de los posibles caminos a partir de un nodo, [arista] se coge de aristasnodoa
listasProbabilidad grafo = map (probabilidadAristaParte1 grafo)

probabilidadArista :: Grafo -> Arista ->  Float --probabilidad de la arista
probabilidadArista grafo arista  = probabilidadAristaParte1 grafo arista / sum (listasProbabilidad grafo (aristasNodoa grafo (uno arista)))

probabilidades :: Grafo -> [Arista] -> [Float] --lista de porbablidades de los posibles caminos a partir de un nodo, [arista] se coge de aristasnodoa
probabilidades grafo = map (probabilidadArista grafo)

aristasNodoa :: Grafo -> Nodo -> [Arista] --me devuelve todas las posibles aristas desde el nodo x
aristasNodoa (G xs []) _ = []
aristasNodoa (G xs (y:ys)) x
  |x == uno y = y : aristasNodoa (G xs ys) x
  |otherwise = aristasNodoa (G xs ys) x

arista :: Grafo -> Float -> Arista
arista (G xs (y:ys)) m
  |m==probabilidadArista (G xs (y:ys)) y = y
  |otherwise = arista (G xs ys) m

ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena xs = m : ordena (delete m xs)
    where m = minimum xs

elegirArista1 :: Grafo -> [Float] -> Arista --la lista de float es listasprobabilidad grafo de una arista ordenada
elegirArista1 grafo (x:xs)
  | p < x =  arista grafo x
  |otherwise = elegirArista1 grafo xs
  where p = 0.5

elegirCamino1 :: Grafo -> Nodo -> Nodo -> [Arista]
elegirCamino1 grafo m n
  |m/=n = arista : elegirCamino1 grafo (dos arista) n
  |otherwise = []
  where arista = elegirArista1 grafo (ordena (probabilidades grafo (aristasNodoa grafo m)))


elegirCaminoN :: Grafo -> Nodo -> Nodo -> Int -> [[Arista]] -- m es el número de hormigas
elegirCaminoN grafo x y 0 = []
elegirCaminoN grafo x y m = elegirCamino1 grafo x y : elegirCaminoN grafo x y (m-1)


--ITERACION N

caminosConA :: [[Arista]] -> Arista -> [[Arista]]
caminosConA [] _ = []
caminosConA (x:xs) arista
  |arista `elem` x = x : caminosConA xs arista
  |otherwise = caminosConA xs arista


feromonasAristaN :: Grafo -> Arista -> Int -> Int -> Float --feromonas en la iteración n con m hormigas
feromonasAristaN (G xs ys) arista 0 m = 0.1
feromonasAristaN (G xs ys) arista n m = 0.99 * feromonasAristaN (G xs ys) arista (n-1) m + (sum (distanciasPosibles caminos))
  where caminos = caminosConA (elegirCaminoNM (G xs ys) (head xs) (last xs) n m) arista


probabilidadAristaParte1M :: Grafo -> Arista -> Int -> Int  -> Float --la necesito para calcular la probabilidad de una arista, el int es numero de interacciones y de hormigas
probabilidadAristaParte1M grafo arista n m = inverso (distancia arista) * feromonasAristaN grafo arista (n-1) m --generalizar

listasProbabilidadM :: Grafo -> [Arista] -> Int -> Int -> [Float] --lista de porbablidadesparte1 de los posibles caminos a partir de un nodo, [arista] se coge de aristasnodoa
listasProbabilidadM _ [] _ _ = []
listasProbabilidadM grafo (x:xs) n m = (probabilidadAristaParte1M grafo x n m) : listasProbabilidadM grafo xs n m

probabilidadAristaM :: Grafo -> Arista -> Int -> Int -> (Float, Arista) --probabilidad de la arista
probabilidadAristaM grafo arista n m = ((probabilidadAristaParte1M grafo arista n m / sumas), arista)
  where sumas = sum (listasProbabilidadM grafo (aristasNodoa grafo (uno arista)) n m)

probabilidadesM :: Grafo -> [Arista] -> Int -> Int -> [(Float, Arista)] --lista de porbablidades de los posibles caminos a partir de un nodo, [arista] se coge de aristasnodoa
probabilidadesM _ [] _ _ = []
probabilidadesM grafo (x:xs) n m = (probabilidadAristaM grafo x n m) : probabilidadesM grafo xs n m

{-
probAristas :: Grafo -> [Arista] -> Int -> Int -> [(Float, Arista)]
probAristas g xs n m = zip probabilidades xs
  where probabilidades = map (funcionprobabilidadunaarista g n m) xs 
-}


intervaloProb :: [(Float, Arista)] -> [(Float, Arista)]
intervaloProb [] = []
intervaloProb [x] = [(1, snd x)]
intervaloProb (x:y:ys) = (fst x, snd x) : intervaloProb (((primero+segundo), snd y):ys)
  where primero = fst x
        segundo = fst y

  
elegirArista1M :: Grafo -> [(Float, Arista)] -> Int -> Int -> Float -> Arista --la lista de float es listasprobabilidad grafo de una arista ordenada
elegirArista1M _ [x] _ _ _= snd x
elegirArista1M grafo (x:xs) n m p
  | p < (fst x) =  snd x
  |otherwise = elegirArista1M grafo xs n m p
  

elegirCamino1M :: Grafo -> Nodo -> Nodo -> Int -> Int -> Float -> [Arista]
elegirCamino1M grafo x y n m p
  |x/=y = arista : elegirCamino1M grafo (dos arista) y n m p
  |otherwise = []
  where arista = elegirArista1M grafo (intervaloProb (probabilidadesM grafo (aristasNodoa grafo x) n m)) n m p


elegirCaminoNM :: Grafo -> Nodo -> Nodo -> Int -> Int -> [[Arista]] -- m es el número de hormigas
elegirCaminoNM grafo x y n 0 = []
elegirCaminoNM grafo x y n m = elegirCamino1M grafo x y n m p : elegirCaminoNM grafo x y n (m-1) 
  where p = last( numerosAleatorios (n+m))

numerosAleatorios :: Int -> [Float]
numerosAleatorios n = take n (randomRs (0.0,1.0) gen)
    where gen= mkStdGen 2022

noRepes :: Eq a => [a] -> [a]
noRepes [] = []
noRepes (x:xs) = x : noRepes (filter (/= x) xs)

fin :: IO ()
fin = do putStr "Elige el número de hormigas iniciales: "
         hormichar <- getLine
         putStr "Elige el número de iteraciones: "
         itechar <- getLine
         putStr "Escribe el nombre del fichero de entrada: "
         nombreIn <- getLine
         contenido <- readFile (nombreIn)
         putStr "Escribe el nombre del fichero de salida: "
         nombreOut <- getLine
         let listanodos = ordena (noRepes (nodos (lines contenido)))
             hormigas = read hormichar :: Int
             iteraciones = read itechar :: Int
             aristas = aristasFichero (lines contenido)
             grafo = G listanodos aristas
             caminos = elegirCaminoNM grafo (head listanodos) (last listanodos) iteraciones hormigas
             texto = caminosFichero caminos
         writeFile nombreOut texto
         
aristasFichero :: [String] -> [Arista]
aristasFichero [] = []
aristasFichero (x:xs) = ( A (nodo1,nodo2) dist) : aristasFichero xs
  where nodo1 = read (head (words x) ) :: Int
        nodo2 = read (head (tail (words x))) :: Int
        dist = read (last (words x)) :: Float

nodos :: [String] -> [Int]
nodos [] = []
nodos (x:xs) = nodo1 : nodo2 : nodos xs
  where nodo1 = read (head (words x)) :: Int
        nodo2 = read (head (tail (words x))) :: Int

grafoFichero :: [Arista] -> String
grafoFichero [] = ""
grafoFichero (x:xs) = show x ++ "\n" ++ grafoFichero xs 

caminosFichero :: [[Arista]] -> String
caminosFichero [] = ""
caminosFichero (x:xs) = intro ++ "\n" ++ grafoFichero x ++ "\n" ++ caminosFichero xs
  where intro = "Un camino elegido por las hormigas es:"