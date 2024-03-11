--sum :: Num a => [a] -> a 
--(*) :: Num a => a -> a -> a


--product :: Num a => [a] -> a 


--twice :: (a -> a ) -> a -> a 




data Cor = Amarelo | Verde | Azul | Vermelho deriving (Show)
fria :: Cor -> Bool
fria Verde = True
fria Azul =  True
fria x = False


--03/10
null' :: [a] -> Bool
null' l = length l == 0

null'' :: [a] -> Bool
null'' [] = True
null'' (h:t) = False 

null''' :: Eq a => [a] -> Bool 
null''' l = l == []


data Curso = LEI Int Float -- numeroAlunos Media Entrada
           | LCC Int Float 
           deriving Show

lei :: Curso 
lei = LEI 170 16.7

cursos :: [Curso]
cursos = [lei]


somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (x : xs) = x + somatorio xs

length' :: [a] -> Int 
length' [] = 0
length' (_ :t) = 1 + length' t


elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False
elem' n (x:xs) | n == x  = True
               | otherwise = elem' n xs

last' :: [a] -> a
last' (x:[]) = x 
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [x] = []
init' (h:t) = 
      h : init' t


reverse' :: [a] -> [a]
reverse' [] = [] 
reverse' (h:t) = reverse' t ++ [h]

reverse'' [] = []
reverse'' l =  reverse'' (tail l)
                ++ [head l]
                