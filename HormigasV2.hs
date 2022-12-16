{-
En este primer bloque encontramos los tipos de datos que vamos a utilizar en este algoritmo,
    El nodo simplemente va a ser un entero,
    Una arista va a ser: un par de nodos, un peso y la última cantidad de feromonas registradas.
    Un grafo, como su definición formal, será una lista de nodos y una lista de aristas
    Una hormiga va a almacenar las aristas que lleva recorridas en la iteración actual, se vacia en cada nueva iteración.
-}

type Nodo = Int

data Arista = A (Nodo,Nodo) Float Float

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




