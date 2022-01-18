import Control.Monad (liftM2)
import Control.Applicative (liftA2)


crossProd :: [a] -> [b] -> [(a, b)]
crossProd = liftA2 (,)

and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool
and' = (&&)
or'= (||)
equ' = (==)
nand' = (not .) . and'
nor' = (not .) . or'
xor' = (not .) . equ'
impl' = or' . not

table :: (Bool -> Bool -> Bool) -> IO()
table p = mapM_ (putStrLn . showP p) (crossProd [True, False] [True, False])
    where showP p (a, b) = show a ++ " " ++ show b ++ " " ++ show (p a b)


 