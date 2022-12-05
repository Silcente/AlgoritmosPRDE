--Algoritmo CAA 
{-Vamos a disponer de una posicion entre (0,n)*(0,n) de enteros para poder inicializarlo asignando una nota a cada posicion,  son su calificacion del nectar entre 0 y 10 en floats -}
import System.Random
type Semilla = Int
numerosAleatorios0_1 :: Semilla -> Int -> [Float]
numerosAleatorios0_1 q n = take n (randomRs (0,1) gen)
    where gen = mkStdGen q

numerosAleatoriosj :: Semilla -> Int -> Tamano ->  [Int] --Para elegir el indice de la formula 
numerosAleatoriosj q n t = take n (randomRs (0,t) gen)  
    where gen = mkStdGen q

mallado :: Semilla -> Int -> Int -> Tablero -- Rellenamos una malla de n*m con posiciones cada una con sus datos (falta rellenar las calidades de forma arbitraria) esto conforma un campo inicial de estudio 
mallado q 0 n = (anadir_fila q 0 (n-1))
mallado q m n = (anadir_fila q (m-1) (n-1)) ++ (mallado q (m-2) (n-1))

--Anadir_fila añade una fila de mas, pero mallado ya respeta el tamaño	
anadir_fila :: Semilla -> Int -> Int -> [((Int,Int), Float)] --Para hacer el mallado primero rellenamoas las filas y para luego irlas añadiendo en la lista de listas 
anadir_fila q m n
    |n==0 = [((m,0), ((numerosAleatorios0_1 q 5000)!!m)*(fromIntegral 10))] 
    |otherwise = [((m,n), ((numerosAleatorios0_1 q 5000)!!(m*n))*(fromIntegral 10))] ++ (anadir_fila q m (n-1))
--Obs: tamaño es del tablero 
enjambre :: Semilla -> Tablero -> (Int,Int) -> Tamano -> Enjambre -- Nos dan el numero de abejas de la colmena, el tamaño del tablero y da como resultado el enjambre
enjambre q xs (n,0) t = [(P ((((numerosAleatoriosj q 5000 n)!!0),((numerosAleatoriosj q 5000 n)!!0)), (buscar_nota xs (((numerosAleatoriosj q 5000 n)!!0),((numerosAleatoriosj q 5000 n)!!0)))  , 1))]
enjambre q xs (n,m) t = [(P(((numerosAleatoriosj q 5000 n)!!(m*2),((numerosAleatoriosj q 5000 n)!!(m*3))), (buscar_nota xs (((numerosAleatoriosj q 5000 n)!!(m*2)),((numerosAleatoriosj q 5000 n)!!(m*3))))  , 1))] ++ (enjambre q xs (n, (m-1)) t)
--No es t en el numero aleatorio deberia ser el tamaño del enjambre, uso la notacion (Int, Int) uno para preservar el tamaño del enjambre y el otro para usarlo a modo de contador n y m han de ser lo mismo


{-Obs Lo suyo es poner el rango entre [-1,1] segun la wikipedia pero eso cabria la posibilidad de salirse del mapa
numerosAleatorios1_1 :: Int -> Tamaño ->  [Float] --Para elegir el indice de la formula 
numerosAleatorios0_t n t = take n (randomRs (0,t) gen)
    where gen = mkStdGen 2022
-}
data Posicion = P ((Int, Int), Float, Int) --Posicion de una abeja, calificacion de su nectar y guardamos el numero de iteraciones en la misma posicion
    deriving (Read, Show) 
type Tamano = Int
type NumCiclo = Int
type Enjambre = [Posicion]
type Tablero = [((Int,Int), Float)] --Posicion y nota
type Parada = Int 
{-La inicializacion debera ser de la forma [((x,y), nota)]-}

buscar_nota :: Tablero -> (Int, Int) -> Float --Saca la nota de una posicion del tablero 
buscar_nota [] _ = 0 --Si da la lista vacia es que nos hemos salido del cuadrado, lo pongo por precaucion pero no deberia
buscar_nota (x:xs) (s,t)
    |(s,t)==(fst x) = (snd x)
	|otherwise = (buscar_nota xs (s,t))
	
suma_tuplas :: (Int,Int) -> (Int, Int) -> (Int, Int)
suma_tuplas (x,y) (z,s) = (x+z, y+s)

resta_tuplas :: (Int, Int) -> (Int, Int) -> (Int, Int)
resta_tuplas (x,y) (z,s) = (x-z, y-s)

iteracion :: Semilla -> Tablero -> Enjambre -> NumCiclo -> Tamano -> Posicion -> Posicion --Introduce una posicion y el criterio de cambio de zona
iteracion q xss ys s t (P ((x,y), n, i))
    |i==s = (P (((aleatorioj!!(x+y)), (aleatorioj!!(x+y))), (buscar_nota xss ((aleatorioj!!(x+y)), (aleatorioj!!(x+y)))), 1))  
    |nota_vecina > n = P (posicion_vecina, nota_vecina, 1)
    |otherwise = P ((x,y), n, i+1)
        where posicion_vecina = suma_tuplas (x,y) (map1 q (map2 signo (resta_tuplas (x, y) (z,t))))
              z = fst (fst3(ys !! (aleatoriok!!(2*(fromIntegral x)+3*(fromIntegral y)))))--Hay que mejorar este numero aleatorio
              t = snd (fst3(ys !! (aleatoriok!!(4*(fromIntegral x)+(fromIntegral y)))))
              nota_vecina = (buscar_nota xss posicion_vecina)
              --aleatorio1 = numerosAleatorios0_1 5000 --En un futuro habra que dejarlo en funcion de t preferiblemente t**2 pero da problemas con elevar enteros 
              aleatorioj = numerosAleatoriosj q 5000 t
              aleatoriok = numerosAleatoriosj q 5000 (length ys-1) 			  
              --phi= fromIntegral (round(aleatorio1!!(x+y))) --Se puede mejor la eleccion del numero aleatorio

signo :: Int -> Int
signo n
    |n<0 = -n
	|otherwise = n 
map2 :: (Int -> Int) -> (Int, Int) -> (Int, Int)
map2 f (x,y) = (f x, f y)

map1 :: Integral a => Semilla -> (Int, Int) -> (a, a)
map1 q (x,y) = (f x, f y)
    where f x =round ((phi* (fromIntegral x)))
          aleatorio1 = numerosAleatorios0_1 q 5000 --En un futuro habra que dejarlo en funcion de t preferiblemente t**2 pero da problemas con elevar enteros 
          phi= ((aleatorio1!!(x+y))) 

fst3 :: Posicion -> (Int, Int)
fst3 (P ((x,y),z,s)) = (x,y)

fst2 :: Posicion -> Float
fst2 (P ((x,y),z,s)) = z

algoritmo :: Semilla -> Tablero -> Enjambre -> NumCiclo -> Tamano -> Parada -> Posicion
algoritmo q xss ys s t 0 = maximo ys  --Añadir la seleccio probabilistica 
algoritmo q xss ys s t n = (algoritmo q xss (map (iteracion q xss ys s t) ys) s t (n-1))
--Obs Es autoreferencial modifica la lista que usa no se si dara problemas 

maximo :: Enjambre -> Posicion
maximo [x] = x
maximo (x:y:ys)
    |(fst2 x) > (fst2 y) = maximo (x:ys)
	|otherwise = maximo (y:ys)
  
--Obs: He puesto el tablero como una unica lista ya que ordenarla como matriz en una lista de listas no tiene ninguna ventaja
-- Obs: se puede mejorar el solo pedir el tamaño ya que estamos considerando cuadrado y no tener que replicar el mismo n n 		

--Entrada/Salida 

procesa :: IO ()
procesa = do putStr "Dame el nombre del fichero de entrada"
             inNombre <- getLine 
             contenido <- readFile inNombre
             putStr "Dame el nombre del fichero de salida "
             outNombre <- getLine
             putStr "Dame la semilla del tablero que vamos a emplear "
             semilla <- getLine
             let seed = read semilla :: Int
             putStr "Dame el tamaño del tablero que vamos a emplear "
             tamano <- getLine
             let tam = read tamano :: Int
             putStr "Dame el numero de abejas obreras que conformaran el enjambre "
             obreras <- getLine
             let obre = read obreras :: Int
             putStr "Dame el numero de ciclos estancado a partir del que se rechaza una posicion y se busca otra aleatoria "
             ciclos <- getLine
             let numCiclos = read ciclos :: Int
             putStr "Dame el numero de iteraciones del algoritmo "
             criterioparada <- getLine
             let parada = read criterioparada :: Int
             let texto = "semilla = " ++ show (seed) ++ ", tamano = " ++ show (tam) ++ ", obreras = " ++ show (obre) ++ ", ciclos = " ++show (numCiclos) ++ ", parada = " ++ show (parada)++ " "++ show ( algoritmo seed (mallado seed tam tam) (enjambre seed (mallado seed tam tam) (obre,obre) tam) numCiclos tam parada)
             writeFile outNombre texto 
            
             
--Falta añadir la seleccion probabilistica  
--Estaria bien que no sobreescriba salida e ir acumulando las soluciones
--Dar los parametros en un archivo que lea de fuera




 