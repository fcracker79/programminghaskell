module Chapter9 where

data Op = Addo | Subo | Mulo | Divo
instance Show Op where
    show Addo = "+"
    show Subo = "-"
    show Mulo = "*"
    show Divo = "/"


valid :: Op -> Int -> Int -> Bool
valid Divo x y = y > 0 && x `mod` y == 0
valid Subo x y = x - y >= 0
valid _ _ _ = True

apply :: Op -> Int -> Int -> Int
apply Addo x y = x + y
apply Subo x y = x - y
apply Mulo x y = x * y
apply Divo x y = x `div` y

data Expro = Valo Int | Appo Op Expro Expro

data Maybeo = Nothingo | Justo Int deriving (Show, Eq)


instance Show Expro where
    show (Valo x) = show x
    show (Appo op l r) = bracket l ++ show op ++ bracket r
        where bracket (Valo x) = show x
              bracket x = "(" ++ show x ++ ")"


applymaybe :: Op -> Maybeo -> Maybeo -> Maybeo
applymaybe _ Nothingo _ = Nothingo
applymaybe _ _ Nothingo = Nothingo
applymaybe o (Justo x) (Justo y) | valid o x y = Justo (apply o x y)
                                 | otherwise = Nothingo


evalo :: Expro -> Maybeo
evalo (Valo x) | x >= 0 = Justo x
              | otherwise = Nothingo
evalo (Appo o l r) = applymaybe o x y
                  where x = evalo l
                        y = evalo r


subs :: [a] -> [[a]]
subs [] =[[]]
subs (x:xs) = ys ++ map (x:) ys
              where ys = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . (map perms) . subs

valueso :: Expro -> [Int]
valueso (Valo x) = [x]
valueso (Appo _ l r) = (valueso l) ++ (valueso r)

solution :: Expro -> [Int] -> Int -> Bool
solution expr values exp = elem (valueso expr) (choices values) && evalo expr == Justo exp
