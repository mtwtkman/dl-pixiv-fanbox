module Main where

import PixivFanbox (runBrickApp)

main :: IO ()
main = runBrickApp 3 "dl-pixiv-fanbox.ini"
