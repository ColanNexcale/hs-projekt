-- main game loop
-- when user inputs a character corresponding to a direction or an action
-- (in the proper location), the corresponding game dialogue runs

gameLoop :: Game -> IO Game
gameLoop (location, character, s)
    = do -- you can change these
        emptyLine
        hashLine
        emptyLine
        putStrLn ("You are in " ++ locationName location) -- ++ location)
        putStrLn ("You are " ++ character) -- charater
        putStrLn ("You can travel to Haskell-Sea, but watch out, Lambda-Monsters are said to lurk around!") -- ++ new_location
        putStrLn ("Anyway, we hope that maybe you can see some magic - functionally - happening.") -- an object maybe, a location ahead
        putStrLn (" ")
        input <- getLine
        putStrLn (input) -- return what was the input

-- input analysis
        if input `elem` quitCharacter -- user wants to quit the game
        then do
          let  g = (location, character, s)
          return (g)
        else do
            gameLoop(location, character, s)


-- starts the game loop with the initial Game
game :: IO ()
game
  = do
      gameLoop start
      return ()

-- input that exits the game
quitCharacter = [":q", ":Q", ":e", ":E"]

 -- ------------------- GAME ---------------------

--  type Game  = ([Char],[Char], Char) -- you can modify the type
type Game = (Location, [Char], Char)
start :: Game
start =  (templateLocation , "Harry Curry", 'C') -- to be changed


------------- data types ----------------

 -- data Direction = ..

-- data Movement = TurnLeft

data Location = Location1 {locationName :: String} | Location2 {locationName :: String}

data Person = Maincharacter {characterName:: String, health :: Int, sunExposure :: Int, hydration :: Int, items :: [Item]}

data Item = Tool {itemName :: String} | Weapon {itemName:: String, damage :: Int, range :: Int} | Vessel {itemName :: String, capacity :: Int, fill :: Int}

data Day = Day {day :: Int, }

data Daytime = Day | Night
 ---------------- constants ----------------
lineWidth = 80
linesPerScreen = 24

headerAreaHeight = 3
mainAreaHeight = 18
bottomAreaHeight = 3

emptyLine :: IO ()
emptyLine = printCharLine ' '

hashLine :: IO ()
hashLine = printCharLine '#'


---------------- pre build locations ----------------
templateLocation :: Location
templateLocation = Location1 "Raum1"

---------------- functions ----------------
printLines :: Int -> IO ()
printLines 0 = return ()
printLines count
  = do
      emptyLine
      printLines (count-1)

-- prints a line of length lineWidth of given char
printCharLine :: Char -> IO ()
printCharLine char = pCL lineWidth char []
    where pCL 0 char line = putStrLn line
          pCL count char line = pCL (count - 1) char (char:line)


createHeaderArea :: String -> IO()
createHeaderArea textToBeDisplayed = return ()
