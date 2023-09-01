module Main where

import PixivFanbox (fromConfigFile, runBrickApp)

main :: IO ()
main = do
  loaded <- fromConfigFile "dl-pixiv-fanbox.ini"
  let conf = case loaded of
        Left _ -> Nothing
        Right c -> Just c
  runBrickApp conf
