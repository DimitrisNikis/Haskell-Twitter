module Main where

import           Data.Acid                (openLocalState)
import           Lib
import           Network.Wai.Handler.Warp (run)
import           Servant                  (Proxy (..), serve)

main :: IO ()
main = do
  acid <- openLocalState initialAppState
  putStrLn "Server running at http://localhost:8080"
  run 8080 $ rejectUnknownParams ["from", "mentions", "tags"] (serve (Proxy :: Proxy API) (server acid))
