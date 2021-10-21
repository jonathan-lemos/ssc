module Parser.Utils where
import GHC.Unicode (isAlphaNum)

unite :: Either a a -> a
unite (Left a) = a
unite (Right a) = a

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) = (f a, b)

mapSecond :: (b -> c) -> (a, b) -> (a, c)
mapSecond f (a, b) = (a, f b)
