{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens         ((^.))
import           Control.Monad        (sequence_)
import           Data.Attoparsec.Text (parseOnly)
import           Data.ByteString.Lazy (toStrict)
import           Data.List            (intersperse)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.Wreq         (get, responseBody)
import           Parsing
import           Types

main :: IO ()
main = do
  text <- content
  let scenario = parseOnly parseScenario text
  case scenario of
    Right scene -> do let result = (Types.find scene)
                      case result of
                        Nothing -> print "There is no path"
                        Just path -> do
                          print (sceneSeed scene)
                          printRoute path
    Left _ -> putStrLn "Cannot get the competition data"


printRoute :: [ID] ->  IO ()
printRoute l= do
  sequence_ . intersperse (putStr ",") $ printID <$> l
  putStrLn ""

content = do
  r <- get "https://space-fast-track.herokuapp.com/generate"
  return.decodeUtf8.toStrict$ r ^. responseBody
