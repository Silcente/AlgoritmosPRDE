{-
En este primer bloque encontramos los tipos de datos que vamos a utilizar en este algoritmo,
    El nodo simplemente va a ser un entero,
    Una arista va a ser: un par de nodos, un peso, la última cantidad de feromonas registradas y probabilidad.
    Un grafo, como su definición formal, será una lista de nodos y una lista de aristas
    Una hormiga va a almacenar las aristas que lleva recorridas en la iteración actual, se vacia en cada nueva iteración.
-}

type Nodo = Int

data Arista = A (Nodo,Nodo) Float Float Float

data Grafo = G [Nodo] [Arista]

data Hormiga = H [Arista]

{-
A continuación listamos unas funciones auxiliares que facilitarán el código más adelante
-}

noRepes :: Eq a => [a] -> [a]
noRepes [] = []
noRepes (x:xs) = x : noRepes (filter (/= x) xs)

nodos :: Arista -> [Nodos]
nodos (A (x,y) _ _ ) = [x,y]

peso :: Arista -> Float
peso (A _ p _ ) = p

feromonas :: Arista -> Float 
feromonas (A _ _ _ f) = f 

{- 
Este bloque lo utilizaremos para iniciar el algoritmo, que se encarga de establecer las condiciones iniciales.

    iniciarHormigas toma de dato n y entonces crea n hormigas que todavía no han recorrido ninguna arista.
    iniciarArista toma un par de nodos y un peso y crea una arista con esos nodos, ese peso y las feromonas iniciales.
    iniciarGrafo toma una lista de aristas y crea un grafo a partir de estas.
-}

iniciarHormigas :: Int -> [Hormigas]
iniciarHormigas 0 = []
iniciarHormigas n = (H []) : iniciarHormigas (n-1)

iniciarArista :: (Nodo,Nodo) -> Float -> Arista
iniciarArista (x,y) p = A (x,y) p 0.1

iniciarGrafo :: [Arista] -> Grafo
iniciarGrafo xs = G nodos xs
    where nodos = noRepes (foldr (++) (map nodos xs))

caminosDesde :: Grafo -> Nodo -> [Arista]
caminosDesde (G _ []) _ = []
caminosDesde (G xs (y:ys)) n
    | (nodos y) !! 0 == n = y: caminosDesde (G xs ys) n
    | otherwise = caminosDesde (G xs ys) n

probCaminosNodo :: Grafo -> Nodo -> Grafo
proCaminosNodo (G xs ys) n = G'
    where posibles = caminosDesde (G xs ys) n
          denominador = sum (zipWith (*) (map feromonas ys) (map (**(-1)) (map peso ys)))
          numerador =

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

{-
1. Probabilidades
2. moverHormigas
3. feromonas
-}






