import System.Random
{-
En este primer bloque encontramos los tipos de datos que vamos a utilizar en este algoritmo,
    El nodo simplemente va a ser un entero,
    Una arista va a ser: un par de nodos, un peso y la última cantidad de feromonas registradas.
    Un grafo, como su definición formal, será una lista de nodos y una lista de aristas
    Una hormiga va a almacenar las aristas que lleva recorridas en la iteración actual, se vacia en cada nueva iteración.
-}

type Nodo = Int

data Arista = A (Nodo,Nodo) Float Float Float -- Distancia, fermonas, probabilidad

data Grafo = G [Nodo] [Arista]

data Hormiga = H [Arista]

{-
A continuación listamos unas funciones auxiliares que facilitarán el código más adelante
-}
--Para los numeros aleatorios
num_aleatorioj :: Int -> Int -> Int ->  Int   
num_aleatorioj q p t =((randomRs (0,t) gen)!!p)  
    where gen = mkStdGen q
--Para determinar el nodo final del Grafo (donde se finalizaran los caminos)
maximo_nodo :: Grafo -> Nodo
maximo_nodo (G xs ys) = foldr1 (max) xs

noRepes :: Eq a => [a] -> [a]
noRepes [] = []
noRepes (x:xs) = x : noRepes (filter (/= x) xs)

iniciarGrafo :: [Arista] -> Grafo
iniciarGrafo xs = G nodos xs
    where nodos = noRepes (foldr (++) [] (map nodos1 xs))
nodos_hormiga ::  Hormiga -> [Nodo]
nodos_hormiga (H xs) = noRepes (foldr (++) [] (map nodos1 xs))

nodos1 :: Arista -> [Nodo]
nodos1 (A (x,y) _ _ _) = [x,y]

aristas :: Grafo -> [Arista]
aristas (G xs ys) = ys

nodos :: Arista -> [Nodo]
nodos (A (x,y) _ _ _ ) = [x,y]

peso :: Arista -> Float
peso (A _ p _ _ ) = p

feromonas :: Arista -> Float 
feromonas (A _ _ _ f) = f 

esta :: Float -> (Float,Float) -> Bool 
esta p xs 
    |x<=p && p<y = True
    |p==x && p==y = True 
    |otherwise = False
        where x = (fst xs)
              y = (snd xs)
{- 
Este bloque lo utilizaremos para iniciar el algoritmo, que se encarga de establecer las condiciones iniciales.
    iniciarHormigas toma de dato n y entonces crea n hormigas que todavía no han recorrido ninguna arista.
    iniciarArista toma un par de nodos y un peso y crea una arista con esos nodos, ese peso y las feromonas iniciales.
    iniciarGrafo toma una lista de aristas y crea un grafo a partir de estas.
-}

iniciarHormigas :: Int -> [Hormiga]
iniciarHormigas 0 = []
iniciarHormigas n = (H []) : iniciarHormigas (n-1)

iniciarArista :: (Nodo,Nodo) -> Float -> Arista
iniciarArista (x,y) p = A (x,y) p 0.1


    
caminosDesde :: Grafo -> Nodo -> [Arista]
caminosDesde (G _ []) _ = []
caminosDesde (G xs (y:ys)) n
    | (nodos y) !! 0 == n = y: caminosDesde (G xs ys) n
    | otherwise = caminosDesde (G xs ys) n
   
anadir_intervalo :: [Arista] -> Anterior -> [(Arista, (Float, Float))] --Se inicializara con Anterior = 0, esta funcion asocia a cada posicion con su intervalo, sin considerar si ha habido un reajuste en cuyo caso las notas estaran aumentadas  
anadir_intervalo [A (x,y) d f p] n = [((A (x,y) d f p),(n,(n+p)))]
anadir_intervalo ((A (x,y) d f p):xs) n = [(A (x,y) d f p),(n, (n+p))]++(anadir_intervalo xs (n+p))

probabilidad :: [(Arista, (Float, Float))] -> Arista -- Se inicializara con anadir_intervalo (caminosDesde xs 1) 
probabilidad [(x, (y,z))] = x   --Si solo queda una posicion esta sera la seleccionada
probabilidad (((A (x,y) d f p),(s,t)):xs)
    |esta (fromIntegral num) (s,t) = (A (x,y) d f p)  --Si el valor aleatorio esta en el intervalo asociado esa es la posicion elegida  
    |otherwise = probabilidad xs  --En caso contrario seguimos buscando
        where num = num_aleatorioj 2022 1 (round(snd (snd (last xs)))) --Cogemos un numero aleatorio entre 0 y la suma total de todos los valores 
quitar_acabados_anterior :: Anterior -> [(Arista, (Float,Float))] -> [(Arista, (Float,Float))] --Esta funcion la aplicaremos en elegir camino para que no pueda volver al nodo anteorior 
quitar_acabados_anterior n [] = []
quitar_acabados_anterior n xs = filter (distinto n) xs
distinto :: Anterior -> (Arista, (Float,Float)) -> Bool
distinto n (((A (x,y) p d f), (Float,Float)), (s,t))
    |n==y = False
    |otherwise = True 
 --Hay que añadir Eq
type Anterior = Nodo --Totalmente prescindible pero para que se entienda 
--REVISAR LA ELECCION DEL NUMERO ALEATORIO 
elegir_camino :: Anterior -> Nodo -> Grafo -> Arista --Dado un nodo determina mediante la seleccion probabilistica que arista sera la siguiente en seleccionar 
elegir_camino x xs = probabilidad (anadir_intervalo (quitar_acabados_anterior x (caminosDesde (aristas xs) x)) 0)

mover_hormiga :: Hormiga -> Grafo -> Hormiga 
mover_hormiga (H []) ys = (H [elegir_camino 1 ys])
mover_hormiga (H xs) ys
    |n == (maximo_nodo ys) = xs --Si el maximo nodo de la hormiga coincide con el maximo nodo del grafo significa que el camino de la hormiga ya ha terminado            
    |otherwise = mover_hormigas (H [(elegir_camino n ys):xs])
        where n = foldr1 max (nodos_hormiga (H xs)) --Maximo nodo de la hormiga 
{-
iteracion :: Hormiga -> Grafo -> [Aristas] -- Grafo se va a inicializar con las probabilidades actualizadas

actualizarArista :: [Hormigas] -> Arista -> Arista

mapArista 

actualizarFero

algoritmo :: Cosas 
kpasos n = algoritmo G'' (iniciar n hormigas) 
where G' = actualizarProb G
      caminosRecorridos = map iteracion G' [hormigas]
      G'' actualizarFeromonas


1. Probabilidades
2. moverHormigas
3. feromonas
-}