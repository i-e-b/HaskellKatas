{-# LANGUAGE TypeOperators #-}

-- decomposition types
data Child a b = Child a b deriving (Show)
data Tag a = Tag a deriving (Show)

-- composition operators
type a :>: b = Child a b
infixr 2 :>:

data Test a b = (Tag a) :>: (Tag b)
