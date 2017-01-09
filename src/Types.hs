module Types where

import Linear
import Data.Graph.AStar (aStar)
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.HashSet as S


type Seed = Double
type PolarCoordinate = (Double, Double, Double)
data ID = Sat Int | Start | Goal deriving (Eq, Show)

instance Ord ID where
  compare (Sat a) (Sat b) = compare a b
  compare (Sat _) Start = GT
  compare (Sat _) Goal = LT
  compare Start Goal = LT
  compare Start (Sat _) = LT
  compare Start Start = EQ
  compare Goal (Sat _) = GT
  compare Goal Start = GT
  compare Goal Goal = EQ

data Satellite = Satellite ID PolarCoordinate deriving (Show)
data Scenario = Scenario
                { sceneSeed :: Seed
                , sceneSatellites :: [Satellite]
                , sceneStartingPosition :: PolarCoordinate
                , sceneGoalPosition :: PolarCoordinate} deriving (Show)

data Vertex = Vertex ID V3 deriving (Eq)


getID :: Vertex -> ID
getID (Vertex i _) = i

toInt :: ID -> Int
toInt Start = -1
toInt Goal = -2
toInt (Sat i) = i

printID :: ID -> IO ()
printID Start = putStr "START"
printID Goal = putStr "GOAL"
printID (Sat i) = putStr ("SAT" ++ show i)

instance Ord Vertex where
  compare (Vertex i0 _) (Vertex i1 _) = compare i0 i1

instance Hashable Vertex where
  hashWithSalt salt (Vertex i _) = salt + toInt i

polarToCartesian :: PolarCoordinate -> V3
polarToCartesian (lat, long, alt) = (x, y , z)
  where x = alt * cos theta * sin phi
        y = alt * cos theta * cos phi
        z = alt * sin theta
        theta = lat / 180.0 * pi -- polar [-pi/2 .. pi/2] north south
        phi  =  long / 180.0 * pi -- azimuth [-pi .. pi] east west

toCartesian :: Scenario -> S.HashSet Vertex
toCartesian scenario = S.fromList (start : stop : satellites)
  where start = Vertex Start (polarToCartesian $ sceneStartingPosition scenario)
        stop = Vertex Goal (polarToCartesian $ sceneGoalPosition scenario)
        satellites = map (\(Satellite i pc) -> Vertex i (polarToCartesian pc)) (sceneSatellites scenario)

cartesianDistance :: Vertex -> Vertex -> Double
cartesianDistance (Vertex _ v1) (Vertex _ v2) = vLength (minus v2 v1)

neighbors :: S.HashSet Vertex -> Vertex -> S.HashSet Vertex
neighbors ps (Vertex _ v0) =
  S.filter notHittingEarth ps
  where notHittingEarth :: Vertex -> Bool
        notHittingEarth (Vertex _ v1) = distanceToOrigo v0 v1 > 6371.0

atGoal :: Vertex -> Bool
atGoal (Vertex Goal _) = True
atGoal _ = False

find :: Scenario -> Maybe [ID]
find s =
   let start = Vertex Start (polarToCartesian $ sceneStartingPosition s)
       stop = Vertex Goal (polarToCartesian $ sceneGoalPosition s)
       vertices = toCartesian s
       astarResult = aStar (neighbors vertices) cartesianDistance (cartesianDistance stop) atGoal start
   in astarResult >>= return . map getID . init
