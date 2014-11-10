import Types  --Contains data types related to gameplay, and some convienent functions
import Engine --Contains functions related to gameplay, that do not perform IO
import Layout --Contains all rooms, and a function to create a new gamestate
import TextFormatting --Contains functions related to manipulating strings

import Data.Char
import Data.Maybe
import System.Directory
-----------------------------------
{-
		The four main functions that control program flow:
			-"main" is responsible for startup -- splash screen, creating a starting state, etc.
			-"programStep" is responsible for keeping track of the game's state, reading and formatting user input, using "parseCommand" and "doCommand" 
				to execute changes to the game's state, and handling the Quit command.  
			-"doCommand" is responsible for executing all non-Quit commands the user gives.  It returns altered game states, outputs descriptions to the terminal,
				and writes to/reads from save files.  It does not take raw user input, only parsed commands.
			-"parseCommand" is responsible for... parsing commands.  It takes raw user input and turns converts it into an instance of the Command type.
			
-}

{- 
	The "Command" type exists to help split the jobs of parsing and using user input.  Every possible command is enumerated by this type.
	Note that Error and Null are special commands the user is not allowed to input; the former is used to indicate errors in parsing or
	using input, while the latter is a dummy command that tells doCommand to do nothing.
-}
data Command = Go Direction | Get Item | Drop Item | Look | LookInventory | Help | Quit | Save | Load | Null | Error String
	deriving (Eq)
	
noCommandError = Error "Invalid command.  Enter (?) for help."

findSavePath :: IO FilePath
findSavePath = do
	direc <- getCurrentDirectory
	return $ direc ++ "/TreasureColtizCastleSave.txt"

launchgame :: IO ()
launchgame = main			 


--Main displays the splash screen, and lets the user choose whether to start a new game or load an old one.
main :: IO ()
main = do
	--Step one, print the splash screen
	printLineCentered80 "The treasure of Coltiz Castle!" --printLineCentered80 is defined in TextFormatting.  Assumes the terminal is 80 characters wide.
	putStr "\n"
	printLineCentered80 "Enter 'n' to start a new game."
	savePath <- findSavePath
	saveExists <- doesFileExist savePath
	if (saveExists) --Only let the player load their saved game if it exists
			then printLineCentered80 "Enter 'l' to load your saved game."
			else return ()
	printLineCentered80 "Enter 'q' to quit."
	--Step two, start the game using input from the player.  This step gets its own function so it can call itself if the user inputs an invalid command. 
	chooseStart
	
	
chooseStart :: IO ()
chooseStart = do
	input <- getLine
	if (input == "n")
		then do
			putStr "\n\n"
			let lore = "Hundreds of years ago, the castle Coltiz was struck by a massive earthquake, its riches destroyed and its legendary Crown lost... can YOU be the first to find it, and reunite it with its four precious gems?"
			mapM printLineCentered80 (splitLines80 lore) --Formats the lore so it looks nice, assuming that the window is 80 characters wide, and prints it to the screen.  splitLines80 is defined in TextFormatting.
			putStr "\n\n"
			putStrLn "Generating castle..."
			putStr "\n"
			start <- randomState --randomState is defined in Layout, it randomly generates a new gamestate.
			doCommand Look start
			programStep start
		else do
			if (input == "l")
				then do
					savePath <- findSavePath
					saveExists <- doesFileExist savePath
					if (saveExists)
						then do
							putStrLn "Loading saved game..."
							putStr "\n"
							save <- fmap read (readFile savePath)
							doCommand Look save 
							programStep save
						else do
							chooseStart
				else do
					if (input == "q")
						then putStrLn "Goodbye!"
						else chooseStart
						
						
						
						
--programStep is responsible for gameplay.  It uses parseCommand to interpret user input, and doCommand to execute it (unless that input is Quit)
--It keeps track of the changing game state by recursively calling itself.
programStep :: GameState -> IO()	
programStep state = do
	--I don't want user the user to have to care about capitalization or errant whitespace, hence this crazy function composition.
	input <- fmap (parseCommand . trimTailWhitespace . map toLower) getLine  --trimTailWhitespace is defined in TextFormatting
	if (input /= Quit)
		then do
			newState <- doCommand input state
			if (winning newState)
				then do
					putStr "\n"
					printLineCentered80 "You got all the items!"
					mapM_ printLineCentered80 $ splitLines80 "Congragulations, you have reforged the lengendary Crown of Coltiz!  Your name will be remembered for ages to come!"
				else do
					programStep newState
		else do
			--Check if the user has recently saved, and warn the user if they haven't. 
			--I don't use doCommand Load because that prints some stuff I don't want printed.
			savePath <- findSavePath
			saveExists <- doesFileExist savePath
			if (saveExists)
				then do
					saveState <- fmap read (readFile savePath)
					if (saveState /= state)
						then do
							putStrLn "You haven't saved!  Are you sure you want to quit?"
							putStrLn "(y/n)"
							choice <- getLine
							if (choice /= "y")
								then programStep state
								else putStrLn "Goodbye!"
						else do
							putStrLn "Goodbye!"
				else do
					putStrLn "You haven't saved!  Are you sure you want to quit?"
					putStrLn "(y/n)"
					choice <- getLine
					if (choice /= "y")
						then programStep state
						else putStrLn "Goodbye!"
						




--parseCommand turns a string into a command
parseCommand :: String -> Command
parseCommand []    = Null --If the user input nothing, don't do anything
parseCommand [c]   = case c of
					'n' -> Go North
					's' -> Go South
					'e' -> Go East
					'w' -> Go West
					'l' -> Look
					'i' -> LookInventory
					'h' -> Help
					'?' -> Help
					'q' -> Quit
					_   -> noCommandError
parseCommand com  = case (com) of
					"north"           -> Go North
					"south"           -> Go South
					"east"            -> Go East
					"west"            -> Go West
					('g':'o':' ':dir) -> parseCommand dir --"clever" pattern to match "go <whatever>".
					"look"            -> Look
					"inv"             -> LookInventory
					"inventory"       -> LookInventory
					"help"            -> Help
					"quit"            -> Quit
					"save"            -> Save
					"load"            -> Load
					other             -> tryParseGetDrop other
					
		  
tryParseGetDrop :: String -> Command
tryParseGetDrop command
	| length phrases /= 2      = noCommandError --All items have one-word names, so if the user didn't input two words they didn't input a valid item
	| firstWord == "d"         = tryParseDrop lastWord
	| firstWord == "drop"      = tryParseDrop lastWord
	| firstWord == "g"         = tryParseGet lastWord
	| firstWord == "get"       = tryParseGet lastWord
	| otherwise                = noCommandError
	where phrases = words command
	      firstWord = head phrases
	      lastWord = last phrases


tryParseDrop :: String -> Command
tryParseDrop itm
	| parseItm == Nothing  = noCommandError
	| otherwise            = Drop (fromJust parseItm)
	where parseItm = readItem itm   --readItem is defined in Types.  It is case-insensitive, and turns a String into a Maybe Item.
		  	
		  	  
tryParseGet :: String -> Command
tryParseGet itm
	| parseItm == Nothing  = noCommandError
	| otherwise            = Get (fromJust parseItm)
	where parseItm = readItem itm




--doCommand takes an already-parsed command, a gamestate, and returns a resulant gamestate.  It handles all output and save/load functionality, but is not responsible for handling Quit.
doCommand :: Command -> GameState -> IO GameState
doCommand (Go dir) state       = do
			let newState = playermove state dir  --I've changed some engine functions to return Maybe GameState.  Nothing indicates no change happened.
			if (isJust newState)
				then do
					let ns = fromJust newState
					doCommand Look ns
				else do
					doCommand (Error "You can't go that way!") state
doCommand (Get itm) state      = do
			let newState = playerget state itm
			if (isJust newState)
				then do
					putStrLn $ "You got the " ++ show itm ++ "."
					return (fromJust newState)
				else do
					putStrLn $ "The " ++ show itm ++ " isn't in this room."
					return state
doCommand (Drop itm) state     = do
			let newState = playerdrop state itm
			if (isJust newState)
				then do
					putStrLn $ "You dropped the " ++ show itm ++ "."
					return (fromJust newState)
				else do
					putStrLn $ "You don't have the " ++ show itm ++ "."
					return state
doCommand Look state           = do
		putStr "\n"
		mapM putStrLn $ describestate state --describestate returns a list of lines to print, formatted to look nice on an 80-character wide terminal.
		return state
doCommand Help state           = do
		displayHelp
		return state
doCommand LookInventory state  = do
		putStrLn $ describeinventory state
		return state
doCommand Save state           = do
		savePath <- findSavePath
		writeFile savePath (show state)
		putStrLn "Game saved."
		return state
doCommand Load state            = do
		putStrLn "Loading saved game..."
		putStr "\n"
		savePath <- findSavePath
		saveExists <- doesFileExist savePath
		if (saveExists)
			then do
				save <- readFile savePath
				doCommand Look (read save)
			else do
				doCommand (Error "Error: No save file exists.") state
doCommand (Error s) state      = do
		putStrLn s
		return state
doCommand _ state              = return state


displayHelp :: IO ()
displayHelp = do
	putStrLn "All available commands: \n"
	displayOneCommand ("help, h, ?", "Display all available commands.")
	displayOneCommand ("go <direction>","Move to the room to the <direction> of your current location, if your current room has an exit that way.  'go' may be ommitted; <direction> may be abbreviated to its first letter.")
	displayOneCommand ("get <item>", "Move the specified item to your inventory, if it is in the room.  May be abbreviated to g <item>.")
	displayOneCommand ("drop <item>", "Move the specified item (if it is in your inventory) to the room you are in.  May be abbreviated to d <item>.")
	displayOneCommand ("look", "Look around the room you are in.  May be abbreviated to l.")
	displayOneCommand ("inventory", "Examine your inventory.  May be abbreviated to inv or i.")
	displayOneCommand ("save", "Save your game.  Overwrites your previous save!")
	displayOneCommand ("load", "Load your saved game.")
	displayOneCommand ("quit", "Quit the game.  You will be warned if you haven't saved.  May be abbreviated to q.")
	
--Displays one command, formatted to look nice.
displayOneCommand :: (String,String) -> IO ()
displayOneCommand (name,effect) = do
	let formatName = ' ':name
	let offset = 20
	let effectLines = splitLines40 effect
	let line1 = formatName ++ replicate (offset - 1 - length formatName) ' ' ++ '-':head effectLines
	putStrLn line1
	if (length effectLines) > 1
		then mapM_ (putStrLn . (replicate offset ' ' ++)) $ tail effectLines
		else return ()



