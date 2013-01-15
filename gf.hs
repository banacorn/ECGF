module GF (Order, GF(..), Show(..), Eq(..), (.+), (.-), (.*), (./), (.~)) where

type Order = Integer
data GF = GF Order Integer

instance Show GF where
    show (GF p n) = show (n `mod` p) -- ++ " (mod " ++ show p ++ ")"
instance Eq GF where
    (==) (GF p a) (GF _ b) = (a `mod` p) == (b `mod` p)

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./
infixl 8 .~

-- extened gcd, for modular multiplicative inverse
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

(.+) (GF p a) (GF _ b) = GF p (a + b `mod` p)
(.-) (GF p a) (GF _ b) = GF p (a + p - b `mod` p)
(.*) (GF p a) (GF _ b) = GF p (a * b `mod` p)
(./) a b = a .* (.~)b
(.~) (GF p a) = GF p (i `mod` p)
    where   (i, _, g) = gcdExt a p
