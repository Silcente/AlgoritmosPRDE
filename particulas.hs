import System.Random
--Tipo, las particulas van a tener asignada una posicion, una velocidad, una mejor posicion local y la mejor posicion global
data Particula = P (Float,Float) (Float,Float) (Float,Float) (Float,Float)
    deriving(Show,Read)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) 
    | x < minResto = x
    |otherwise = minResto
    where minResto = minimo xs

--Esta funcion, como dice su nombre, la vamos a utilizar para desplazar las particulas
mover :: Particula -> (Float,Float) -> Particula 
mover (P xs ys h g) v = P (fst xs + fst v,snd xs + snd v) ys h g

posicion :: Particula -> (Float, Float)
posicion (P xs ys h g) = xs 

velocidad :: Particula -> (Float,Float)
velocidad (P xs ys h g) = ys

global :: Particula -> (Float, Float)
global (P xs ys h g) = g

--Las tres siguientes funciones sirven para inciar con un a malla uniforme de particulas.
distribuir :: Int -> Float -> Float -> [Float]
distribuir 0 _ _ = []
distribuir n l u = ((l+(u-l)/fromIntegral(n+1))):(distribuir (n-1) (l+(u-l)/fromIntegral(n+1)) u)

inicializar :: Int -> [Float] -> [Float] -> [(Float,Float)]
inicializar n ls us = [(x,y) | x<-(distribuir n (ls!!0) (us!!0)), y<-(distribuir n (ls!!1) (us!!1))]

inicioPar :: [(Float,Float)] -> [Particula]
inicioPar [] = []
inicioPar (x:xs) = (P x (0,0) x x) : (inicioPar xs) 

compararEvaluado :: Eq a => [(b,a)] ->a -> b
compararEvaluado [x] y = fst x 
compararEvaluado (x:xs) y
        | ( snd x == y ) = fst x
        | otherwise = compararEvaluado xs y

cambiarUnaG ::  (Float,Float) -> Particula -> Particula 
cambiarUnaG x (P pos v h g)= P pos v h x

--iniciar el maximo global
ponerG :: [Particula] -> ((Float,Float)->Float)-> [Particula]
ponerG xs f = map ( cambiarUnaG mejorPos ) xs
    where mejor = minimo [f pos | pos <- (map posicion xs)]
          mezclados = zip (map posicion xs) (map f (map posicion xs))
          mejorPos = compararEvaluado mezclados mejor

--La siguiente iteracion actualiza una particula:
numerosAleatorios :: Int -> [Float]
numerosAleatorios n = take n (randomRs (0.0,1.0) gen)
    where gen= mkStdGen 2022

iteracion :: ((Float,Float)-> Float) -> Float -> Float -> Float -> Float -> Float -> Particula ->Particula 
iteracion f w phih phig rh rg (P pos v h g)
    | f mov < f g = P mov (v1,v2) mov mov 
    | f mov < f h = P mov (v1,v2) mov g 
    | otherwise = P mov (v1,v2) h g 
    where mov = (fst pos + v1, snd pos + v2)
          v1= w*(fst v)+phih*rh*(fst h - (fst pos))+phig*rg*(fst g-fst pos)
          v2= w*(snd v)+phih*rh*(snd h - (snd pos))+phig*rg*(snd g-snd pos)

--hacer k iteraciones en todas las particulas 
kpasos :: Int -> [Particula] -> ((Float,Float)-> Float) -> Float -> Float -> Float-> [Particula]
kpasos 0 xs f _ _ _ = ponerG xs f 
kpasos k xs f w phih phig= kpasos (k-1) (map (iteracion f w phih phig (aleatorios!!(k-1)) (aleatorios !! (2*k-1))) (ponerG xs f)) f w phih phig
    where aleatorios = numerosAleatorios (2*k) 

roundPar :: Particula -> Particula
roundPar (P pos v h g) = P roundpos roundv roundh roundg 
    where roundpos = (fromIntegral(round((fst pos)*1000))/1000, fromIntegral(round((snd pos)*1000))/1000)  
          roundv = (fromIntegral(round((fst v)*1000))/1000, fromIntegral(round((snd v)*1000))/1000)  
          roundh = (fromIntegral(round((fst h)*1000))/1000, fromIntegral(round((snd h)*1000))/1000)  
          roundg = (fromIntegral(round((fst g)*1000))/1000, fromIntegral(round((snd g)*1000))/1000)   

particulasString :: [Particula] -> String
particulasString [] = ""
particulasString (x:xs) = show x ++ "\n" ++ (particulasString xs)

informe :: [Particula]-> ((Float,Float)->Float) -> String
informe xs f = resumen1 ++"\n"++resumen2++ "\n" ++ particulasString (map roundPar xs) 
    where mejorpos = global ((map roundPar xs)!!0)
          resumen1 = "La mejor posición es " ++ show mejorpos ++ " y el valor de la función en este punto es " ++ show (f mejorpos)
          resumen2 = "Las particulas en la última iteración quedan de la siguiente manera: "
--La función de optimizar
optimizar :: ((Float,Float) -> Float) -> Int -> Int ->[Float]-> [Float] -> Float -> Float -> Float -> (Float,Float)
optimizar f k n ls us w phih phig = g
        where (P pos v h g) = (kpasos k (ponerG (inicioPar (inicializar n ls us)) f) f w phih phig) !! 0

func :: (Float,Float) -> Float 
func (x,y) = abs(x*(y-5) + x*sin(y-7)) 

proceso :: IO ()
proceso = do putStr "Escriba el nombre del fichero de salida: "
             nombreOut <- getLine 
             putStr "Escriba el numero de pasos que quiere realizar: "
             pasos <- getLine
             let steps = read pasos :: Int 
             putStr "Escriba el indice de particulas, si introduce el número n se distribuirán n^2 particulas: "
             part <- getLine 
             let n = read part :: Int 
             putStr "Escriba en una lista los limites inferiores de busqueda: "
             liminf <- getLine
             let ls = read liminf :: [Float]
             putStr "Escriba en una lista los limites superiores de busqueda: "
             limsup <- getLine
             let us = read limsup :: [Float]
             putStr "Escriba el parametro w: "
             wchar <- getLine 
             let w = read wchar :: Float
             putStr "Escriba el parametro phih: "
             phihchar <- getLine 
             let phih = read phihchar :: Float
             putStr "Escriba el parametro phig: "
             phigchar <- getLine 
             let phig = read phigchar :: Float
             let texto = informe (kpasos steps (ponerG (inicioPar (inicializar n ls us)) func) func w phih phig) func
             writeFile nombreOut texto 
