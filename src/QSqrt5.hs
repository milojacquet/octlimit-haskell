

module QSqrt5 
    ( QSqrt5(QSqrt5), phi
    ) where

-- QSqrt5 represents an element of the field Q[√5]
-- QSqrt5 a0 a1 = a0 + a1 √5
data QSqrt5 = QSqrt5 Rational Rational deriving (Eq, Show)


qSqrt5Ge0 :: QSqrt5 -> Bool
qSqrt5Ge0 (QSqrt5 a0 a1) = ((a0^(2::Int) >= 5*a1^(2::Int)) && a0 > 0) || ((a0^(2::Int) <= 5*a1^(2::Int)) && a1 > 0)


instance Ord QSqrt5 where
    x <= y = qSqrt5Ge0 $ y - x

instance Num QSqrt5 where
    (QSqrt5 a0 a1) + (QSqrt5 b0 b1) = QSqrt5 (a0+b0) (a1+b1)
    (QSqrt5 a0 a1) - (QSqrt5 b0 b1) = QSqrt5 (a0-b0) (a1-b1)
    (QSqrt5 a0 a1) * (QSqrt5 b0 b1) = QSqrt5 (a0*b0 + 5*a1*b1) (a0*b1 + a1*b0)
    abs x = max x (-x)
    signum x = case x `compare` 0 of
        GT -> 1
        EQ -> 0
        LT -> -1
    fromInteger a = QSqrt5 (fromInteger a) 0

instance Fractional QSqrt5 where
    fromRational a = QSqrt5 a 0
    recip (QSqrt5 a0 a1) = QSqrt5 (a0/norm) (-a1/norm)
        where norm = a0^(2::Int) - 5*a1^(2::Int)

-- golden ratio
phi :: QSqrt5
phi = QSqrt5 (1/2) (1/2)

