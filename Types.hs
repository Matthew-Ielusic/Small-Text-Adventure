module Types (
			   Direction (..),
               Room (..),
			   Item (..),
			   readItem,
			   allItems,
			   Location (..),
			   isInventory,
			   GameState (..),
			   ItemLocation,
			   extractItem,
			   isInventoryIL
			 ) where
			 
import Data.Char
import Data.List


data Direction = North | South | East | West
	deriving (Eq, Show, Read)

	
data Room = Room { 
				   name :: String,
                   descr :: String,
                   exits :: [(Direction, String)]
				 } deriving (Eq, Read, Show)



data Item = Crown | Ruby | Sapphire | Emerald | Diamond
	deriving (Eq, Show, Read, Enum)
	
	
--Converts a string into an Item without throwing No Parse exceptions.  Assumes the passed-in string is all lower-case.
readItem :: String -> Maybe Item
readItem s 
	| findItm == Nothing  = Nothing
	| otherwise           = fmap read findItm
	where toRead = (toUpper $ head s):(tail s)
	      findItm = find (== toRead) $ map show allItems


allItems :: [Item]
allItems = [Crown .. Diamond]


data Location = Place Room | Inventory
	deriving (Eq, Show, Read)


isInventory :: Location -> Bool
isInventory Inventory = True
isInventory _         = False


type ItemLocation = (Item, Location)


data GameState = GameState { 
							 playerLoc :: Room,
                             itemLocs :: [ItemLocation],
                             rooms :: [Room]
						   } deriving (Eq, Show, Read)
						   
		
						   				   
extractItem :: ItemLocation -> Item
extractItem (itm,_) = itm


isInventoryIL :: ItemLocation -> Bool
isInventoryIL (_, Inventory) = True
isInventoryIL _              = False
