--1a check
type Ponto = (Double, Double)


data Movimento = Norte | Sul | Este | Oeste deriving (Show)
move :: Ponto -> Movimento -> Ponto
move (x,y)  Norte = (x, y+1)
move (x,y)  Sul   = (x, y-1)
move (x,y)  Este  = (x+1, y)
move (x,y)  Oeste = (x-1, y)


--1b
dist :: Ponto -> Ponto -> Double
dist  (x1, y1) (x2, y2) = sqrt (((x2-x1) ^ 2) + ((y2-y1) ^ 2))


---2
novaOrigem :: Ponto ->  Double -> Ponto
novaOrigem (x,y) lado = (x, y - lado)

novaOrigem' :: Ponto -> Double -> Ponto
novaOrigem' (x,y) lado = (x-(lado/2) , (y-lado/2))

---3
type Velocidade = Double
type Tempo = Double

move2H:: Ponto -> Velocidade -> Tempo -> Ponto
move2H(x,y) v t = (x + v * t,y)

move2V :: Ponto ->  Velocidade -> Tempo -> Ponto
move2V (x, y) v t =( x,y + v*t)


---6

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto deriving (Show, Eq)

verifica :: Ponto -> Figura -> Bool
verifica (x,y) (Circulo (x2, y2)raio) = dist (x,y) (x2, y2) <= raio
verifica (x,y) (Rectangulo (x2,y2)( x3,y3)) = let cond1= x >=  x2 && x <= x3 
                                                  cond2 = y <= y2 && y>= y3 
                                                  in cond1 && cond2



---7

data Ponto2 = Cartesiano Double Double 
            | Polar Double Double deriving (Show, Eq)
posx :: Ponto2 -> Double
posx (Cartesiano x y) = x
posx (Polar raio angulo) = raio * cos angulo

posy :: Ponto2 -> Double
posy (Cartesiano x y) = y
posy (Polar raio angulo) = raio * sin angulo

raio :: Ponto2 -> Double 
raio (Polar raio angulo) = raio
raio (Cartesiano x y) = dist (x,y) (0,0)



angulo :: Ponto2 -> Double
angulo (Polar raio angulo) = angulo
angulo (Cartesiano x y) = atan2 y x 


dista :: Ponto2 -> Ponto2 -> Double
dista p1 p2 = let (x1, y1) = (posx p1, posy p1)
                  (x2, y2) = (posx p2, posy p2)
              in dist (x1, y1) (x2, y2) 





head2 :: [a] -> Maybe a 
head2 [] = Nothing 
head2 (x:t) = Just x









                                  
















