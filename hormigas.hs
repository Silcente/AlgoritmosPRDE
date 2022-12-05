
type Nodo = Int
data Arista = A (Nodo, Nodo) Float deriving (Show, Read)
data Grafo = G [Nodo] [Arista] deriving (Show, Read)

instance Eq Arista where
  (==) (A (x,y) z) (A (x',y') z') = (x==x' && y==y' && z==z') || (x==y' && y==x' && z==z')

camino :: Grafo -> [Arista] --está función crea un camino desde el punto inicial al final, pero aparecen repeticiones de aristas
camino (G _ []) = []
camino (G _ [x]) = [x]
camino (G xs (y:z:ys))
  |last xs /= dos y && dos y == uno z = y:z:camino (G xs (z:ys)) -- :( y : camino (G (dropWhile xs == dos y) aristasnodo x))
  |last xs /= dos y && dos y /= uno z = y:camino (G xs (y:ys))
  |last xs == dos y && dos y == uno z = y:z:camino (G xs [])
  |last xs == dos y && dos y /= uno z = y:camino (G xs [])
  |otherwise = camino (G xs ys)
--G [1,2,3,4] [(A (1,2) 2), (A (1,3) 3), (A (2,4) 3), (A (3,4) 4)] ejemplo de grafo
--G [1,2,3,4,5] [(A (1,2) 12),(A (1,3) 34),(A (1,5) 78),(A (2,4) 55),(A (2,5) 32),(A (3,4) 61),(A (3,5) 44),(A (4,5) 93)]
estaArista :: Grafo -> Arista -> Bool

caminosNodoA :: Grafo -> [[Arista]]
caminosNodoA (G xs []) = []
caminosNodoA (G xs ys) = quitarRepes (camino (G xs ys)) : caminosNodoA (G xs arista)
  where arista = eliminarLista (G xs ys) (head (tail (quitarRepes(camino (G xs ys)))))

caminosNN :: Grafo -> Nodo -> Nodo -> [[Aristas]]
caminosNN (G (x:xs) ys) n1 n2
    | estaArista (G (x:xs) ys) (n1,n2) = (n1,n2) -- 
    | estaArista (G (x:xs) ys) (n1,x) =( [(n1,x)] ++ caminosNN (G xs ys) x n2) --
    |otherwise = caminos NN (G xs ys) 

eliminarLista :: Grafo -> Arista -> [Arista]
eliminarLista (G xs []) arista = []
eliminarLista (G xs (y:ys)) arista 
  |arista /= y = y: eliminarLista (G xs ys) arista 
  |otherwise = eliminarLista (G xs ys) arista 

--aristasNodoA grafo nodo -> [arsita]

quitarRepes :: [Arista] -> [Arista] --quita todas las aristas repetidas de un caminu
quitarRepes [] = []
quitarRepes (x:xs) = x : quitarRepes (filter (/= x) xs)

esCamino :: Grafo -> [Arista] -> Bool --está función es para quitar aquellos caminos que no empiecen por el punto inicial del grafo
esCamino g [] = True
esCamino (G ys zs) (x:xs) = head ys == uno x

uno :: Arista -> Int
uno (A (x, y) z) = x

dos :: Arista -> Int
dos (A (x, y) z) = y

distancia :: Arista -> Float
distancia (A (x, y) z) = z

caminosPosibles :: Grafo -> [[Arista]]
caminosPosibles (G _ []) = []
caminosPosibles (G xs (y:ys))
  |esCamino (G xs (y:ys)) cam = cam : caminosPosibles (G xs ys)
  |otherwise = caminosPosibles (G xs ys)
  where cam = quitarRepes (camino (G xs (y:ys)))
--G [1,2,3,4,5] [(A (1,2) 12),(A (1,3) 34),(A (1,5) 78),(A (2,4) 55),(A (2,5) 32),(A (3,4) 61),(A (3,5) 44),(A (4,5) 93)]


distanciaCamino :: [Arista] -> Float
distanciaCamino = foldr ((+) . distancia) 0

distanciasPosibles :: [[Arista]] -> [Float]
distanciasPosibles = map distanciaCamino

inverso :: Float -> Float
inverso x = 1/x

inversoDistancias :: [Float] -> [Float]
inversoDistancias = map inverso

caminosConA :: [[Arista]] -> Arista -> [[Arista]]
caminosConA [] _ = []
caminosConA (x:xs) arista
  |arista `elem` x = x : caminosConA xs arista
  |otherwise = caminosConA xs arista

ferormonasArista1 :: Grafo -> Arista -> Float --ferormonas en la primera iteración
ferormonasArista1 (G xs (y:ys)) arista = 0.099 + sum (inversoDistancias (distanciasPosibles (caminosConA (caminosPosibles (G xs (y:ys))) arista)))
--estas dos se pueden juntar si se quiere
ferormonasAristaN :: Grafo -> Arista -> Int -> Float --ferormonas en la iteración n
ferormonasAristaN grafo arista 1 = ferormonasArista1 grafo arista
ferormonasAristaN grafo arista n
  |n/=1 = 0.99 * ferormonasAristaN grafo arista (n-1) + sum (distanciasPosibles (caminosConA (caminosPosibles grafo) arista))
  |otherwise = ferormonasAristaN grafo arista 1

probabilidadAristaParte1 :: Grafo -> Arista -> Int -> Float --la necesito para calcular la probabilidad de una arista
probabilidadAristaParte1 grafo arista x = inverso (distancia arista) * ferormonasAristaN grafo arista x

listasProbabilidad :: Grafo -> [Arista] -> Int -> [Float] --lista de porbablidadesparte1 de los posibles caminos a partir de un nodo, [arista] se coge de aristasnodoa
listasProbabilidad grafo [] _ = []
listasProbabilidad grafo (y:ys) x = probabilidadAristaParte1 grafo y x : listasProbabilidad grafo ys x


probabilidadArista :: Grafo -> Arista -> Int -> Float --probabilidad en la iteración n
probabilidadArista grafo arista x = probabilidadAristaParte1 grafo arista x / sum (listasProbabilidad grafo (aristasNodoa grafo (uno arista)) x)

aristasNodoa :: Grafo -> Nodo -> [Arista] --me devuelve todas las posibles aristas desde el nodo x
aristasNodoa (G xs []) _ = []
aristasNodoa (G xs (y:ys)) x
  |x == uno y = y : aristasNodoa (G xs ys) x
  |otherwise = aristasNodoa (G xs ys) x

probabilidadesCamino :: Grafo -> [Arista] -> Int -> [Float] --las porbabilidades de cada arista de un camino
probabilidadesCamino _ [] _ = []
probabilidadesCamino grafo (x:xs) m = probabilidadArista grafo x m : probabilidadesCamino grafo xs m 

elegirCamino :: Grafo -> Int -> [[Arista]] 
elegirCamino (G xs []) m = []
elegirCamino (G xs (y:ys)) m
  |probabilidadArista (G xs (y:ys)) y m == maximum (probabilidadesCamino (G xs (y:ys)) (aristasNodoa (G xs (y:ys)) (uno y)) m) = noCaminosRepes (añadir (caminosPosibles (G xs (y:ys))) y ++ elegirCamino (G xs aristas) m)
  |otherwise = noCaminosRepes (eliminar (caminosPosibles (G xs (y:ys))) y ++ elegirCamino (G xs aristas) m) --crep que eliminar es innecesario, aunque a lo me lo acorta ya que quita elementos de la lista de caminos posibles
  where aristas = quitarRepes (concat (caminosConA (caminosPosibles (G xs ys)) y))

añadir :: [[Arista]] -> Arista -> [[Arista]]
añadir [] _ = []
añadir (x:xs) arista
  | arista `elem` x = x : añadir xs arista
  |otherwise = añadir xs arista

eliminar :: [[Arista]]  -> Arista -> [[Arista]]
eliminar [] _ = []
eliminar (x:xs) arista 
  | arista `elem` x = eliminar xs arista
  |otherwise = x : eliminar xs arista

noCaminosRepes :: [[Arista]] -> [[Arista]]
noCaminosRepes [] = []
noCaminosRepes (x:xs) = x : noCaminosRepes (filter (/= x) xs)



-- (caminosPosibles (G [1,2,3,4,5] [(A (1,2) 4), (A (1,3) 3), (A (2,4) 3), (A (3,4) 4)])) (A (1,2) 4)
--G [1,2,3,4,5] [(A (1,2) 12),(A (1,3) 34),(A (1,5) 78),(A (2,4) 55),(A (2,5) 32),(A (3,4) 61),(A (3,5) 44),(A (4,5) 93)]