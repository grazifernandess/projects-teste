import Data.Char

---ficha 1


primUlt :: [a] -> (a,a)
--primUlt :: [Int] -> (Int, Int)
primUlt (h:t) = (h, last t)

multiplo :: Int -> Int -> Bool 
multiplo m n | mod m n == 0 = True
             |otherwise = False

truncaImpar :: [Int] -> [Int]
truncaImpar l | (mod (length l) 2) /= 0 = tail l
              | otherwise = l

max2 :: Int -> Int -> Int 
max2 n m = if (n>m)              
           then n 
           else m 

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 a (max2 b c)


delta :: Float -> Float -> Float -> Float
delta a b c = b ^ 2 - 4 * a * c 

nRaizes :: Float -> Float -> Float -> Float
nRaizes a b c | (delta a b c) < 0 = 0
              | (delta a b c) == 0 = 1
              | otherwise = 2


--2.b)
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | (nRaizes a b c) == 0 = []
             | (nRaizes a b c) == 1 = [-b / (2*a)]
             | otherwise = [ ((-b + sqrt (b ^ 2 - 4 *a *c)) / 2 * 
 a), ((-b - sqrt (b ^ 2 - 4 *a *c)) / 2 * a)]




--3
type Hora = (Int , Int)

horaValida :: Hora -> Bool
horaValida (h,m) = (h >= 0) && (h <= 23) && (m >= 0) && (m <= 59)

---3b

hora :: Hora -> Hora -> Bool
hora (h1, m1) (h2,m2)   = h1 > h2 || h1 == h2 && m1 > m2
                          

---3c

horaconverter :: (Int, Int) -> (Int)
horaconverter (h,m) = h * 60 + m 


--3d
horamin :: (Int) -> (Int, Int)
horamin m = (div m 60 ,  mod 60 m)



---3e 

horasdif :: (Int,Int) -> (Int,Int) -> (Int)
horasdif (h1,m1) (h2,m2) = abs ((h1-h2) *60 +(m1-m2))

----3f

addmin :: (Int) -> Hora -> Hora
addmin m2 (h,m) = if m + m2 >= 60 
                  then (h + div (m+m2) 60, mod (m+m2) 60)
                  else if m + m2 < 60
                  then  (h,m+ m2)
                  else (h,m)   


---5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--a
next:: Semaforo -> Semaforo
next s | s== Verde = Amarelo 
next s | s== Amarelo =  Vermelho
next s | s== Vermelho = Verde


---b
stop :: Semaforo -> Bool
stop s   | s== Verde = False
         | s== Amarelo = False
         | s== Vermelho = True


---c
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True 
safe _ Vermelho = True
safe _ _        = False



--data Ponto2 = Cartesiano Double Double 
           -- | Polar Double Double deriving (Show, Eq)

---7

type Ponto = (Double,Double)
data Figura = Circulo Ponto Double 
             | Rectangulo Ponto Ponto 
             | Triangulo Ponto Ponto Ponto 
             deriving (Show,Eq)
{-
--a
poligono :: Figura -> Bool 
poligono (Circulo p r) = False
poligono (Rectangulo (x1,y1) (x2,y2)) = x1 /= x2 && y1 /= y2
poligono (Triangulo (x1,y1) (x2,y2) (x3,y3)) = (y2 - y1) / (x2 - x1) /= (y3 - y2) / (x3 - x2)

vertices :: Figura -> [Ponto]
vertices (Circulo p r) = []
vertices (Rectangulo p1 p2) = [p1 Cartesiano (posx p1) (posy p2) , Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

-}
--8


isLower2 :: Char -> Bool 
isLower2 c = (ord c >= ord 'a'  && ord c <= ord 'z')

isDigit' :: Char -> Bool
isDigit' d = (ord d >= '48' && ord d <= ord '57')


            
        








--1b
dist :: (Float, Float) -> (Float, Float) -> Float
dist  (x1, y1) (x2, y2) = sqrt ((x2-x1) ^ 2) + ((y2-y1) ^ 2)
