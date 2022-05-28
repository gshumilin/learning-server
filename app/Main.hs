module Main where

import Routing (application)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    run 3000 application