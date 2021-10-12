{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaskellInDepth.CH11.Types101_5 where
import GHC.TypeLits ( Nat, KnownNat, natVal, Symbol, symbolVal, KnownSymbol )
import Data.Proxy ( Proxy(..) )


newtype Pointer (align :: Nat) = Pointer Integer deriving(Show)

newtype PrefixedString (prefix :: Symbol) = PrefixedString String


instance KnownSymbol p => Show (PrefixedString p) where
    show (PrefixedString s) = symbolVal (Proxy :: Proxy p) ++ s


type Base4 = Pointer 4
type Base32 = Pointer 32
type HiPrefixed = PrefixedString "Hi, "
type HelloPrefixed = PrefixedString "Hello, "

ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer p) = p * natVal (Proxy :: Proxy align)


mix :: PrefixedString x -> PrefixedString x -> String
mix = undefined 

main :: IO ()
main = do
    let x4 = Pointer 123 :: Base4
    let x32 = Pointer 123 :: Base32
    print x4
    print $ ptrValue x4
    print $ ptrValue x32
    let hiMirko = (PrefixedString "Mirko" :: HiPrefixed)
    let helloMirko = (PrefixedString "Mirko" :: HelloPrefixed)
    print hiMirko
    print helloMirko
    -- Couldn't match type ‘"Hello, "’ with ‘"Hi, "’
    -- print $ mix hiMirko helloMirko
