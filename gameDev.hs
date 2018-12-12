import GameMap
import Items
-- main game loop
-- when user inputs a character corresponding to a direction or an action
-- (in the proper location), the corresponding game dialogue runs

gameLoop :: Game -> IO Game
gameLoop (position, person)
    = do -- you can change these

        printList $ createHeaderArea person
        putStrLn $ getLocationName $ getLocationAt position
        emptyLine
        hashLine
        emptyLine
        input <- getLine


-- input analysis
        if elem input quitCommands
          then do
            return (position, person)
          else do
            gameLoop $ processInput (position, person) input

-- starts the game loop with the initial Game
game :: IO ()
game
  = do
      gameLoop start
      return ()

-- input that exits the game
quitCommands = [":q", ":Q", ":e", ":E"]
upCommands = ["w", "W"]
downCommands = ["s", "S"]
leftCommands = ["a", "a"]
rightCommands = ["d", "D"]

 -- ------------------- GAME ---------------------

type Game = (Position, Person)
start :: Game
start =  (startPosition , startCharacter)

data Person = Person {personName:: String, health :: Int, sunExposure :: Int, hydration :: Int, items :: [Item]}

startCharacter = Person {
  personName = "Mosh",
  health = startHealth,
  sunExposure = startSunExposure,
  hydration = startHydration,
  items = startItems
}


 ---------------- constants ----------------
lineWidth = 80
linesPerScreen = 24

startPosition = (1,1)

startHealth = 100
startSunExposure = 0
startHydration = 100
startItems = [waterBottle]

headerAreaHeight = 3
mainAreaHeight = 18
bottomAreaHeight = 3

emptyLine :: IO ()
emptyLine = printCharLine ' '

hashLine :: IO ()
hashLine = printCharLine '#'




---------------- functions ----------------

processInput :: Game -> String -> Game
processInput ((x,y), person) input
  | elem input upCommands = ((x - 1, y), person)
  | elem input downCommands = ((x + 1, y), person)
  | elem input leftCommands = ((x, y - 1), person)
  | elem input rightCommands = ((x, y + 1), person)
  | otherwise = ((x,y), person)


printLines :: Int -> IO ()
printLines 0 = return ()
printLines count
  = do
      emptyLine
      printLines (count-1)

printList :: [String] -> IO ()
printList [] = return ()
printList (l:ls) = do
                      putStrLn l
                      printList ls

getAllItemNames :: Person -> String
getAllItemNames (Person  _  _  _  _  items)
  = concat (map (\i -> getItemName i) items)

-- prints a line of length lineWidth of given char
printCharLine :: Char -> IO ()
printCharLine char = pCL lineWidth char []
    where pCL 0 char line = putStrLn line
          pCL count char line = pCL (count - 1) char (char:line)
getEmptyLine :: String
getEmptyLine = build lineWidth
  where build 0 = []
        build count = ' ': build (count - 1)

createHeaderArea :: Person -> [String]
createHeaderArea p
  = getEmptyLine
    : ("Status > Leben: " ++ (show $ health p)
       ++ " > Wasserhaushalt: " ++ (show $ hydration p)
        ++ " > " ++ (getAllItemNames p))
        : getEmptyLine : []
