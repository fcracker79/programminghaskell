module BookOfMonads.Chapter5 where

newtype LMaybe a = LMaybe (Maybe a)
newtype RMaybe a = RMaybe (Maybe a)


instance Show a => Show (LMaybe a) where
    show (LMaybe (Just x)) = "LJust " ++ (show x)
    show (LMaybe Nothing) = "LNothing"

instance Show a => Show (RMaybe a) where
    show (RMaybe (Just x)) = "RJust " ++ (show x)
    show (RMaybe Nothing) = "RNothing"

-- OR
instance Semigroup (LMaybe a) where
    (<>) (LMaybe (Just a)) _ = LMaybe (Just a)
    (<>) (LMaybe Nothing) a = a

instance Monoid (LMaybe a) where
    mempty = LMaybe Nothing


-- AND
instance Semigroup a => Semigroup (RMaybe a) where
    (<>) (RMaybe (Just _)) b = b
    (<>) (RMaybe Nothing) _ = RMaybe Nothing

instance Monoid a => Monoid (RMaybe a) where
    mempty = RMaybe $ Just mempty
