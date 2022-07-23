
import Base
--- CHUNK 1 -----------------------------------------------------------------------------
-- Question 1
opposite :: Direction -> Direction 
opposite North = South 
opposite East = West 
opposite South = North
opposite West = East

-- Question 2
noActions :: Item -> GameState -> Next GameState 
noActions item _ = Same ("There are no actions that can be done with the item: " ++ show item)

-- Question 3
winRoom :: Room 
winRoom = 
    Room "the winning room" "this is the room you win in" True (Just Key) [] [] [(North, startingRoom)] noActions

-- Question 4
startingRoom :: Room 
startingRoom = 
    Room "the starting room" "this is the room you start in" False Nothing [(Spoon, "A golden spoon")] [] [(North, secondRoom), (East, gameRoom), (South, winRoom)] noActions

-- Question 5 
secondRoom :: Room 
secondRoom = 
    Room "another room" "looks almost exactly like the last one" False Nothing [] [WoodTroll 10 Key] [(South, startingRoom)] action

action :: Item -> GameState -> Next GameState 
action Spoon (GS _ Room {monsters=[]}) = Same "There are no monsters in this room."
action Spoon gs = 
    let (m : ms) = monsters (room gs)
    in 
        if attack m == 0
            then Progress ("The " ++ show m ++ " has been defeated") $ killMonster (m : ms) gs
            else Progress ("The " ++ show m ++ " took 5 damage from the " ++ show Spoon ++ " and now has " ++ show (attack m) ++ " health.") gs{room=(room gs){monsters=(m{health=attack m}): ms}}
    
    where 
        killMonster :: [Monster] -> GameState -> GameState
        killMonster (m:ms) (GS _ room) = 
            gs{room=room{monsters=ms, items= (holding m, "Dropped by the monster") : items room}}

-- BONUS--------------------------------------------------------
action Chessboard gs = 
    let (m : ms) = monsters (room gs)
    in 
        case m of -- Used case so I could add more monsters and only wood trolls could have this action
            WoodTroll _ h -> Progress "You beat the wood troll at chess. Well done! He gives you his key and runs away crying.." $ winItem ms h gs
    where 
        winItem :: [Monster] -> Item -> GameState -> GameState
        winItem ms i (GS p r) = GS (p{inventory=i : inventory p}) (r{monsters=ms})
-- BONUS--------------------------------------------------------

action i gs = noActions i gs

monsterHealth:: Monster -> Int -> Int 
monsterHealth (WoodTroll health _) attackDamage = health - attackDamage

attack :: Monster -> Int
attack m = monsterHealth m 5

-- Question 6
game0 :: GameState 
game0 = GS (Player "player name" []) startingRoom

--- CHUNK 2 -----------------------------------------------------------------------------
-- Question 7
instance Parsable Item where
    parse "key" = Just Key
    parse "spoon" = Just Spoon 
    parse "chessboard" = Just Chessboard -- BONUS
    parse _ = Nothing

-- Question 8
instance Parsable Direction where
    parse "north" = Just North
    parse "east" = Just East
    parse "south" = Just South
    parse "west" = Just West
    parse _ = Nothing

-- Question 9
instance Parsable Command where
    parse ('g' : 'o' : ' ' : dir ) =  
        case parse dir of 
            Nothing -> Nothing
            Just x -> Just (Move x)
    parse ('g' : 'r' : 'a' : 'b' : ' ' : item) = 
        case parse item of 
            Nothing -> Nothing 
            Just x -> Just (PickUp x)
    parse ('u' : 's' : 'e' : ' ' : item) = 
        case parse item of 
            Nothing -> Nothing 
            Just x -> Just (Use x)
    parse "end" = Just End
    parse "help" = Just Help -- BONUS
    parse _ = Nothing  

-- Question 10
tellResponse :: String -> IO ()
tellResponse s = putStrLn $ "< " ++ s ++ "." 

-- Question 11
readCommand :: IO (Maybe Command)
readCommand = do
    putStr "> "
    cmd <- getLine 
    putStrLn ""
    return (parse cmd)


--- CHUNK 3 -----------------------------------------------------------------------------
-- Question 12
deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom i [] = []
deleteFrom i ((k, v) : items)   | k == i    = items
                                | otherwise = (k, v) : deleteFrom i items

-- Question 13
leaveRoom :: Room -> Direction -> Room -> Room 
leaveRoom fromRoom dir toRoom = toRoom{doors=updateRoom fromRoom dir toRoom}
    where 
        updateRoom from d to= (opposite d, from) : deleteFrom (opposite d) (doors to) 

-- Question 14
step :: Command -> GameState -> Next GameState 
step (Move d) gs = 
    case lookup d (doors (room gs)) of
        Nothing -> Same $ "There is no door in the direction: " ++ show d
        Just x -> checkForItems x (inventory (player gs))
        where 
            checkForItems :: Room -> [Item] -> Next GameState
            checkForItems r items = 
                case requires r of
                    Nothing -> Progress (constructMoveSentence r) gs{room=leaveRoom (room gs) d r}
                    Just x -> 
                        if x `elem` items 
                            then Progress (constructMoveSentence r ++ " You used the item: " ++ show x) gs{room=leaveRoom (room gs) d r}
                            else Same $ "You do not have the required item: " ++ show x 
            constructMoveSentence r = "You move into the " ++ name r ++ "."

                
step (PickUp item) gs =
    case lookup item (items (room gs)) of
        Nothing -> Same $ "The item: " ++ show item ++ " is not in this room."
        Just _ -> Progress ("You take the item: " ++ show item) $ pickUpItem item
            where
                pickUpItem :: Item -> GameState
                pickUpItem x = GS{room=removeItemFromRoom x (room gs), player=addItemToPlayer x (player gs)} 
                removeItemFromRoom :: Item -> Room -> Room
                removeItemFromRoom i r = r{items=deleteFrom i (items r)}
                addItemToPlayer :: Item -> Player -> Player
                addItemToPlayer i p = p{inventory= i : inventory p}


step (Use item) gs = 
    if item `elem` inventory (player gs)
        then actions (room gs) item gs
        else Same $ "The item: " ++ show item ++ " is not in your inventory."

-- Question 15

play :: GameState -> IO ()
play gs = do {
    tellContext gs;
    playLoop gs
}

playLoop :: GameState -> IO ()
playLoop (GS player room)   | isWinRoom room = do {
                                tellResponse "You have won!"
                            }
                            | otherwise = do {
                                cmd <- readCommand;
                                runGame (GS player room) cmd;
                                
                            }
                                
                            where
                                runGame :: GameState -> Maybe Command -> IO ()
                                runGame gs cmd = 
                                    case cmd of 
                                        Nothing -> do {tellResponse "That command doesn't exist."; play gs}
                                        Just x -> doWithCommand gs x
                                        
                                doWithCommand :: GameState -> Command -> IO ()
                                doWithCommand gs End = do tellResponse "Ending the game"
                                doWithCommand gs Help = do {helpMenu; play gs} -- BONUS
                                doWithCommand gs cmd = 
                                    case step cmd gs of
                                        Same s -> do {tellResponse s; playLoop gs}
                                        Progress s ngs -> do {tellResponse s; play ngs}

-- Question 16
main :: IO() 
main = do play game0

-- BONUS- Add a help menu
-- Added Help as a Command option in data Command on Base.hs
-- Added Help as an option for playLoop (inside of doWithCommand)
-- Added Help in Question 9 code
helpMenu :: IO () 
helpMenu =  do {
    putStrLn "";
    putStrLn "Help Menu: List of Commands";
    putStrLn "---------------------------";
    putStrLn "   go <direction>  : Moves player in the corresponding direction (can be north, south, east or west).";
    putStrLn "   grab <item>     : Pick up item from room (can be spoon, key or chessboard).";
    putStrLn "   use <item>      : Uses the item in the room you are in (try 'use spoon' when you are in a room with a monster and you have a spoon).";
    putStrLn "   help            : Opens the help menu (this menu). ";
    putStrLn "   end             : Ends the game.";
    putStrLn ""
}

-- BONUS A new item and some action demonstrating it
-- Added Chessboard item to Base.hs
-- Added show Chessboard = "chessboard" to Base.hs
-- Added an action relating to the chessboard.
-- When use chessboard in room with a wood troll you beat them at chess and their held item
gameRoom :: Room -- Room to store the item
gameRoom = Room "room full of games to play" "room containing games" False Nothing [(Chessboard, "A game loved by wood trolls (even though they arent that smart).")] [] [(West, startingRoom)] noActions