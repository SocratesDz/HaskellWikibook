module Advanced.Monoids where

{-
    There are several possible monoid instances for Bool. 
    Write at least two of them using newtypes, as in the 
    Sum and Product examples. Be sure to verify the 
    monoid laws hold for your instances.
-}

newtype And = And { getAnd :: Bool }
    deriving ( Eq
            , Ord
            , Read
            , Show
            , Bounded
            )

newtype Or = Or { getOr :: Bool }
    deriving ( Eq
            , Ord
            , Read
            , Show
            , Bounded
            )

instance Semigroup And where


instance Monoid And where
    mempty = And True
    And x `mappend` And y = And (x && y)

instance Semigroup Or where

instance Monoid Or where
    mempty = Or False
    Or x `mappend` Or y = Or (x || y)