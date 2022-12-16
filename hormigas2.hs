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

{-la sigueinte función nos devuelve todas las aristas que tienen el mismo 'primer' nodo -}

aristasNodoa :: Grafo -> Nodo -> [Arista] 
aristasNodoa (G xs []) _ = []
aristasNodoa (G xs (y:ys)) x
  |x == uno y = y : aristasNodoa (G xs ys) x
  |otherwise = aristasNodoa (G xs ys) x


{-la sigueinte función nos devuelve todos los caminos que tienen la arista dada -}
caminosConA :: [[Arista]] -> Arista -> [[Arista]]
caminosConA [] _ = []
caminosConA (x:xs) arista
  |arista `elem` x = x : caminosConA xs arista
  |otherwise = caminosConA xs arista

{-la sigueinte función nos devuelve la ferormona de una arista recurriendo a las fermonas en la iteración anterior, y los caminos recorridos en la iteración -}
ferormonasAristaN :: Grafo -> Arista -> Int -> Int -> Float --ferormonas en la iteración n con m hormigas
ferormonasAristaN (G xs ys) arista 0 m = 0.1
ferormonasAristaN (G xs ys) arista n m = 0.99 * ferormonasAristaN (G xs ys) arista (n-1) m + (sum (distanciasPosibles caminos))
  where caminos = caminosConA (elegirCaminoNM (G xs ys) (head xs) (last xs) n m) arista

{-lista de la multiplicación de las feromonas y el inversona de la distancia de unas aristas dadas-}
probabilidadAristaParte1M :: Grafo -> Int -> Int -> [Arista] -> [Float] --la necesito para calcular la probabilidad de una arista, el int es numero de interacciones y de hormigas
probabilidadAristaParte1M _ _ _ [] = []
probabilidadAristaParte1M grafo n m (x:xs) = (inverso (distancia x) * ferormonasAristaN grafo x (n-1) m) : probabilidadAristaParte1M grafo n m xs

{-la multiplicación de las feromonas y el inversona de la distancia de una arista entre la suma de los elementos de las función anterior-}
probabilidadAristaM :: Grafo -> Int -> Int -> [Arista] -> [(Float, Arista)] --probabilidad de la arista
probabilidadAristaM _ _ _ [] = []
probabilidadAristaM grafo n m (x:xs) = (((inverso (distancia x) * ferormonasAristaN grafo x (n-1) m)/sumas), x) : probabilidadAristaM grafo n m xs
  where sumas = sum (probabilidadAristaParte1M grafo n m (aristasNodoa grafo (uno x)))

{-creamos, teniento ya las probabilidades de una lista de aristas, un intervalo con las probabilidades-}
intervaloProb :: [(Float, Arista)] -> [(Float, Arista)]
intervaloProb [] = []
intervaloProb [x] = [(1, snd x)]
intervaloProb (x:y:ys) = (fst x, snd x) : intervaloProb (((primero+segundo), snd y):ys)
  where primero = fst x
        segundo = fst y
  
{-utilizando el intervalo anterior, si nuestro numero aleatorio está en uno de los iintervalos, elegimos esa arista-}
elegirArista1M :: Grafo -> [(Float, Arista)] -> Int -> Int -> Float -> Arista 
elegirArista1M _ [x] _ _ _ = snd x
elegirArista1M grafo (x:xs) n m p
  | p < (fst x) =  snd x
  |otherwise = elegirArista1M grafo xs n m p
  
{-da el recorrido de una hormiga desde el nodo inicial hasta el nodo final-}
elegirCamino1M :: Grafo -> Nodo -> Nodo -> Int -> Int -> Float -> [Arista]
elegirCamino1M grafo x y n m p
  |x/=y = arista : elegirCamino1M grafo (dos arista) y n m p
  |otherwise = []
  where arista = elegirArista1M grafo (intervaloProb (probabilidadAristaM grafo n m (aristasNodoa grafo x))) n m p


elegirCaminoNM :: Grafo -> Nodo -> Nodo -> Int -> Int -> [[Arista]] -- m es el número de hormigas
elegirCaminoNM grafo x y n 0 = []
elegirCaminoNM grafo x y n m = elegirCamino1M grafo x y n m p : elegirCaminoNM grafo x y n (m-1) 
  where p = last( numerosAleatorios (n+m))

numerosAleatorios :: Int -> [Float]
numerosAleatorios n = take n (randomRs (0.0,1.0) gen)
   where gen= mkStdGen 2022


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
         let listanodos = sort (nub (nodos (lines contenido)))
             hormigas = read hormichar :: Int
             iteraciones = read itechar :: Int
             aristas = aristasFichero (lines contenido)
             grafo = G listanodos aristas
             caminos = mejorCamino (elegirCaminoNM grafo (head listanodos) (last listanodos) iteraciones hormigas)
             costo = show (distanciaCamino caminos)
             texto = "El camino más óptimo recorrido por las hormigas es: " ++ "\n" ++ grafoFichero caminos ++ "\n" ++ costo
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

{-caminosFichero :: [[Arista]] -> String
caminosFichero [] = ""
caminosFichero (x:xs) = intro ++ "\n" ++ grafoFichero x ++ "\n" ++ caminosFichero xs
  where intro = "El camino más óptimo recorrido por las hormigas es: "-}

mejorCamino :: [[Arista]] -> [Arista]
mejorCamino xs = maximun (map distanciasPosibles xs)