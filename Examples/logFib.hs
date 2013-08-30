import Data.List(unfoldr)
import Data.Tuple(swap)

data P a b = P {fstP :: {-# UNPACK #-} !a, sndP :: {-# UNPACK #-} !b}

fib4 ::  (Integral a, Num c) => a -> c
fib4 = sndP . foldr fib' (P 1 0) . unfoldr unBit
  where
    fib' 0 (P f g) = P (f^2 + g^2) (g*(2*f-g))
    fib' _ (P f g) = P (f*(2*g+f)) (f^2 + g^2)
    unBit 0 = Nothing
    unBit x = Just . swap . (`quotRem` 2) $ x
