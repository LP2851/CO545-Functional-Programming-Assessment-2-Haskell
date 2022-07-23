module Base where

import Data.List

-- # Core types

-- ## Game State
-- The game state has a player and a room (the current room)
data GameState = GS { player :: Player, room :: Room }

-- The `Next` type is used to represent what happens next in a game
-- either things stay the `Same` or `Progress` is made to a new game state
data Next a =
      Same String       -- Message about why things are staying the same
    | Progress String a -- Success message and next thing `a`

-- ## Player
-- Has a name a list of items
data Player =
  Player
    {   playerName :: String
      , inventory  :: [Item]
    }

-- ## Room (where all the action happens)

data Room =
  Room
    {  name        :: String
     , description :: String
     , isWinRoom   :: Bool
     , requires    :: Maybe Item
       -- ^ whether an item is required to enter this room or not
     , items       :: [(Item, String)]
       -- ^ Association list of items and some description about where they are
     , monsters    :: [Monster]
       -- ^ Some monsters in this room
     , doors       :: [(Direction, Room)]
       -- ^ Association list between directions and a room
     , actions     :: Item -> GameState -> Next GameState
       -- ^ Function taking an item and a game state and returning what to do next
    }
    

data Direction = North | South | East | West
  deriving Eq

instance Show Direction where
  show North  = "north"
  show South  = "south"
  show East   = "east"
  show West   = "west"

-- ## Items and monsters
data Item = Key | Spoon
  | Chessboard -- bonus question
  deriving Eq

instance Show Item where
  show Key   = "key"
  show Spoon = "spoon"
  show Chessboard = "chessboard" -- bonus question

data Monster = WoodTroll { health :: Int, holding :: Item }

instance Show Monster where
  show (WoodTroll health item) = "wood troll holding a " ++ show item

-- ## Command interface

data Command =
  Move Direction | Use Item | PickUp Item | End 
  | Help -- This is for me for a bonus question

-- ## Key type class

class Parsable t where
  parse :: String -> Maybe t

-- ## Helpers for outputing to the user

tellContextLine :: String -> IO ()
tellContextLine s = putStrLn $ "   " ++ s ++ "."

tellDoors :: [(Direction, Room)] -> IO ()
tellDoors [] = tellContextLine $ "There are no doors."
tellDoors [(dir, _)] = tellContextLine $ "There is a door to the " ++ show dir
tellDoors doors =
  tellContextLine $ "There are doors to the " ++ (intercalate " and " (map (show . fst) doors))

tellItem :: (Item, String) -> IO ()
tellItem (item, pos) = tellContextLine $ pos ++ " there is a " ++ show item

tellMonster :: Monster -> IO ()
tellMonster monster = tellContextLine $ "There is a " ++ show monster

-- Main help for taking a game state and outputting information about it
tellContext :: GameState -> IO ()
tellContext (GS p r) = do
  putStrLn ""
  tellContextLine $ "You are in a " ++ name r ++ ". It is " ++ description r
  tellDoors (doors r)
  mapM tellItem (items r)
  mapM tellMonster (monsters r)
  putStrLn ""
  return ()