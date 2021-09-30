Questions
=========

Proxies vs Type Applications
----------------------------
Why can't I get rid of proxies? 
In the end, I do not need to declare a function that takes a proxy if I can use type applications.

E.g.

```haskell
data Proxy x = Proxy

class DinoProxy u where
    dinoProxy :: Proxy u -> String

instance DinoProxy String where
    dinoProxy _ = "stringa"

instance DinoProxy Int where
    dinoProxy _ = "intero"


class DinoTypeVar u where
    dinoTypeVar :: String

instance DinoTypeVar String where
    dinoTypeVar = "stringa"

instance DinoTypeVar Int where
    dinoTypeVar = "intero"

main :: IO ()
main = do
    putStrLn "Using proxy"
    putStrLn $ dinoProxy (Proxy :: Proxy Int)
    putStrLn "Using type applications"
    putStrLn $ dinoTypeVar @Int
```
TODO see https://www.youtube.com/watch?v=FFZXWoqviBo.
