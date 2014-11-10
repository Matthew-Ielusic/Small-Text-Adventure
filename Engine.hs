module Engine ( 
				describestate,
				describeinventory,
				playerget,
				playerdrop,
				playermove,
				splitLines80,
				winning
			  ) where
			  
import Types
import Layout
import TextFormatting
import Data.List
import Data.Char



--Creates a description of the current state, and splits it into lines.  An empty line seperates the room name from the room description, and the description from the list of exits and items.
describestate :: GameState -> [String]
describestate state = showRoomName:emptyLine:descrLines ++ (emptyLine:showExits) ++ showItems
	where showRoomName = "--" ++ map toUpper ("the " ++ name (playerLoc state)) ++ "--"
	      emptyLine = ""     
	      descrLines = (splitLines80 . descr . playerLoc) state
	      showExits = ["There is a room to the " ++ show d ++ "." | (d,_) <- (exits . playerLoc) state]
	      itemsInRoom = map fst $ filter (\(_, Place rm) -> rm == playerLoc state) $ filter (not . isInventoryIL) (itemLocs state)
	      showItems = ["The " ++ show i ++ " is in the room." | i <- (itemsInRoom)]


describeinventory :: GameState -> String
describeinventory state 
	| null invItems   = "INVENTORY: Empty."
	| otherwise       = "INVENTORY: " ++ (intercalate ", " invItems) ++ "."
	where invItems = map (show . extractItem) $ filter isInventoryIL (itemLocs state)
	--invItems extracts a list of Items from a list of ItemLocations, then maps show over the result
	

--playerget, playerdrop, and playermove all return Nothing to indicate that the game's state hasn't changed, and Just <whatever> if it has.

playerget :: GameState -> Item -> Maybe GameState
playerget state itm
	| itemIndex == Nothing      = Nothing
	| otherwise                 = Just $ GameState (playerLoc state) (newLocs $ itemIndex) (rooms state)
	where locs = itemLocs state
	      itemIndex = elemIndex (itm, Place (playerLoc state)) locs
	      newLocs (Just index) = take index locs ++ [(itm, Inventory)] ++ (drop (index + 1) locs)
	

playerdrop :: GameState -> Item -> Maybe GameState
playerdrop state itm
	| itemIndex == Nothing      = Nothing
	| otherwise                 = Just $ GameState (playerLoc state) (newLocs $ itemIndex) (rooms state)
	where locs = itemLocs state
	      itemIndex = elemIndex (itm, Inventory) locs
	      newLocs (Just index) = take index locs ++ [(itm, Place (playerLoc state))] ++ (drop (index + 1) locs)

		  	  
playermove :: GameState -> Direction -> Maybe GameState
playermove state dir
	| direcPr == Nothing                  = Nothing
	| otherwise                           = Just $ GameState (findRoom direcPr) (itemLocs state) (rooms state)
	where direcPr = find (\(d,_) -> d == dir) $ (exits . playerLoc) state
	      findRoom (Just (_,rmName)) = head $ filter (\rm -> (name rm) == rmName) (rooms state)


winning :: GameState -> Bool
winning gs = foldl (\b il -> (isInventoryIL il) && b) True (itemLocs gs)