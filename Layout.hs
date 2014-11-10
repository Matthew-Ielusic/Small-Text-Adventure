module Layout (
				randomState
			  ) where
			   
import Types
import Data.Maybe
import Data.List
import System.Random




--Here's the plan:


{-    Set in some sort of castle... you start outside it, with description of quest (get the Crown, all 4 gems).
      The Waterfall was once a part of the grand hall, but an earthquake opened a ravine between them.  You can jump into the ravine, 
	  But not climb out.  A path connects the Waterfal and dungeon.
	                     
						  
						   
						   Waterfall    --       Path	
								
								|		           | 										 
								|		           |													 						 
						    
							  Ravine     --      Dungeon
							
								|										 
								|	
																														 
			  Vault  --  Grand Hall  --  Throne Room   																															 													 
				  																                 

-}
					   
					   
--Returns a GameState, with all the rooms defined in Layout but randomized player and item locations.
randomState :: IO GameState
randomState = do
	playerStart <- randomL roomsList
	itemLocations <- sequence [randomL roomsList | _ <- allItems] --Generate one random room per item.
	let locList = zipWith (\itm rm -> (itm, Place rm)) allItems itemLocations
	return $ GameState (playerStart) (locList) (roomsList)
	
	
randomL :: [a] -> IO a
randomL xs = do
	seed <- newStdGen
	let index = fst $ randomR (0,(length xs) - 1) seed
	return (xs !! index)					   
	
					   				   
roomsList = [grandHall, throneRoom, ravine, dungeon, waterfall, path, vault]


grandHall = Room {
					name = "Grand Hall",
					descr = ghDesc,
					exits = [(North, name ravine), (East, name throneRoom), (West, name vault)]
				 }
				
throneRoom = Room {
					name = "Throne Room",
					descr = trDesc,
					exits = [(West, name grandHall)]
				  }

ravine = Room {
				name = "Ravine",
				descr = rvDesc,
				exits = [(South, name grandHall), (East, name dungeon), (North, name waterfall)]
			  }

dungeon = Room {
				 name = "Dungeons",
				 descr = dnDesc,
				 exits = [(North, name path), (West, name ravine)]
			   }
			   

waterfall = Room {
					name = "Waterfall",
					descr = wfDesc,
					exits = [(South, name ravine), (East, name path)]
				 }
				 
path = Room {
				name = "Path",
				descr = ptDesc,
				exits = [(West, name waterfall), (South, name dungeon)]
			}
			
vault = Room {
				name = "Vault",
				descr = vtDesc,
				exits = [(East, name grandHall)]
			 }





ghDesc = "They call this area of the castle the Grand Hall, and with reason: not even the withering of time can remove the granduer of this massive room, with gold-lined walls, a ceiling a hundred feet off the ground, and to the far north Coltiz Castle's famed artificial waterfall.  However, between you and the waterfall is a massive ravine."

rvDesc = "The ravine you are in, about 50 feet deep and 30 feet wide, was created in the earthquake that ruined this castle.  You can't see where it ends to the west, but to the east it connects to the castle's dungeons.  To the north, you see that the castle's artificial waterfall somehow still runs; the falling water has collected into a pool, and machinery pumps that water back up."

trDesc = "The throne room of Coltiz Castle is one of the grandest sights imaginable.  The room was built around the Golden Throne, a chair over thirty feet tall and plated with solid gold that gleams as brightly as the day it was built."

dnDesc = "You're in the dungeons.  Much of Coltiz Castle was built to appeal to visitors, but this area wasn't.  Dug deep into the ground, the dungeons are dark and damp; the corridors are narrow and walled with bare stone and earth.  Abandoned cells litter the halls, some with skeletons inside.."

ptDesc = "A bare path connects the guard barracks in the dungeon to the top of the waterfall.  Carved out of the rock the castle was built into, it is steeply sloped and surrounded by the machinery that powers the castle's artificial waterfall.  Centuries after the castle was abandoned, the machinery still hums with life."

wfDesc = "One of Coltiz Castle's greatest treasures was its artificial waterfall, a hundred feet tall at the end of the grand hall.  Although now it empties into the ravine, it still runs."

vtDesc = "You're in the vault of Coltiz Castle.  Although rumors abound of the treasures this place should hold, inspired by the epic wealth of the castle, it seems that looters have made off with everything this vault once held.  You're not here for gold anyway."


