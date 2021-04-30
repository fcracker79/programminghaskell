module HaskellInDepth.CH1.PlayWithQuantifiers where




import Data.Typeable (Typeable)
import Data.Data (cast)
ishowyou2thingsthewayyouwant :: (forall a. a -> String) -> String
ishowyou2thingsthewayyouwant yourshow = yourshow (1 :: Int) ++ " / " ++ yourshow "and more" ++ "."


coolshow :: Typeable a => a -> String
coolshow a = case cast a :: (Maybe Int) of
  Just _ -> "THIS IS AN INT"
  Nothing -> case cast a :: (Maybe String) of
    Just _ -> "THIS IS A STRING"
    Nothing -> "DUNNO"

-- x = ishowyou2thingsthewayyouwant coolshow