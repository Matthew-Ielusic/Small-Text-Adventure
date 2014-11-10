module TextFormatting (
						printLineCentered80,
						splitLines80,
						splitLines40,
						trimTailWhitespace
					  ) where
import Data.Char
						
					
						
--Prints the line so it's centered in the window.  Assumes the window is 80 characters wide.
printLineCentered80 :: String -> IO ()
printLineCentered80 str = putStrLn $ (replicate offset ' ') ++ str
	where offset = (80 - length str) `div` 2
		  
		  
--Breaks the passed-in string into groups of length 80 or less, that do not end in partial words.
splitLines80 :: String -> [String]
splitLines80 ""            = []
splitLines80 str 
	| length (rest) >= 80  = line:(splitLines80 rest)
	| null rest            = [line]
	| otherwise            = line:[rest]
	where (line,rest) = seperateLine str 80 
	

splitLines40 :: String -> [String]
splitLines40 ""            = []
splitLines40 str 
	| length (rest) >= 40  = line:(splitLines40 rest)
	| null rest            = [line]
	| otherwise            = line:[rest]
	where (line,rest) = seperateLine str 40 


--Given a string s and line length n, splits s into (a,b) such that a does not end in a partial word, length a <= n, and a ++ [Some number of ' '] ++ b == s
seperateLine :: String -> Int -> (String, String)
seperateLine str n
	| length str <= n   = (str, "")
	| otherwise          = (a, trimLeadingWhitespace b)
	where a = (trimTailWhitespace . trimPartialWord) $ take n str
	      b = drop (length a) str
	
		        
trimLeadingWhitespace :: String -> String		  
trimLeadingWhitespace (' ':str) = trimLeadingWhitespace str
trimLeadingWhitespace str       = str
	      
	
trimPartialWord :: String -> String
trimPartialWord str
	| (last str) `elem` alphabet   = trimPartialWord (init str)
	| otherwise                    = str
	where alphabet = ['a'..'z'] ++ ['A'..'Z'] 
	
		  
		  
--Removes all whitespace from the end of a string.																
trimTailWhitespace :: String -> String
trimTailWhitespace ""   = ""
trimTailWhitespace s
	| isSpace (last s)  = trimTailWhitespace (init s)
	| otherwise         = s

