module Utils where

unite :: Either a a -> a
unite (Left a) = a
unite (Right a) = a

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) = (f a, b)

mapSecond :: (b -> c) -> (a, b) -> (a, c)
mapSecond f (a, b) = (a, f b)

trimStart :: (t -> Bool) -> [t] -> [t]
trimStart predicate (x:xs) =
  if predicate x then trimStart predicate xs else x:xs
trimStart _predicate [] = []

trimEnd :: (t -> Bool) -> [t] -> [t]
trimEnd predicate list = result where
  trimFolder current (value, count) =
    if not $ predicate value then count else current
  listWithCounts = zip list [1..]
  trimIdx = foldl trimFolder 0 listWithCounts
  result = take trimIdx list

trim :: (t -> Bool) -> [t] -> [t]
trim predicate = trimStart predicate . trimEnd predicate
