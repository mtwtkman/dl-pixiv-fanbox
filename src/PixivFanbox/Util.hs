module PixivFanbox.Util where

chunkedList :: Int -> [a] -> [[a]]
chunkedList _ [] = []
chunkedList size xs
  | size < 1 = error "size must be greater than 1"
  | otherwise = chunk : chunkedList size rest
  where
    (chunk, rest) = splitAt size xs
