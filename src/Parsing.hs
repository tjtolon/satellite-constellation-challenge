{-# LANGUAGE OverloadedStrings #-}
module Parsing where
import Types
import Data.Attoparsec.Text
import Control.Applicative (many)
parseSeed :: Parser Seed
parseSeed = do
  string "#SEED: "
  v <- double
  endOfLine
  return v

parseSatellite :: Parser Satellite
parseSatellite = do
  string "SAT"
  identifier <- decimal
  char ','
  lat <- double
  char ','
  long <- double
  char ','
  elevation <- double
  endOfLine
  return $ Satellite (Sat identifier) (lat, long, elevation + 6371.0)

parseRoute :: Parser (PolarCoordinate, PolarCoordinate)
parseRoute = do
  string "ROUTE,"
  latStart <- double
  char ','
  longStart <- double
  char ','
  latStop <- double
  char ','
  longStop <- double
  return ((latStart, longStart, 6371.0001), (latStop, longStop, 6371.0001))

parseScenario :: Parser Scenario
parseScenario = do
  scenSeed <- parseSeed
  sats <- many parseSatellite
  (start, stop) <- parseRoute
  endOfInput
  return $ Scenario scenSeed sats start stop
