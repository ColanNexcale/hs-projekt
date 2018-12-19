import GameMap
import Items
import Person
import Data.List
-- main game loop
-- when user inputs a character corresponding to a direction or an action
-- (in the proper location), the corresponding game dialogue runs

main = do
  game

gameLoop :: Game -> IO Game
gameLoop (position, person)
    = do
        let currentLocation = getLocationAt position
        let currentLocationDescription = getLocationDescription currentLocation
        printList $ createHeaderArea person
        putStrLn $ getLocationName currentLocation
        printLines 1
        printList currentLocationDescription
        printLines (mainAreaHeight - 1 - (length currentLocationDescription))
        printList $ createFooterArea
        putStr ">>: "
        input <- getLine


-- input analysis
        if elem input quitCommands
          then do
            return (position, person)
          else do
            if elem input exploreCommands
              then do
                  let waterSearch = getDiscovery location
                  let modPerson = modifyPersonStats person
                else do
                  let newPosition = getNewPosition input position
                  let modPerson = modifyPersonStats person $ getLocationAt newPosition
                  gameLoop (newPosition, modPerson)

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
exploreCommands = ["u", "U"]


 -- ------------------- GAME ---------------------

type Game = (Position, Person)
start :: Game
start =  (startPosition , startCharacter)

 ---------------- constants ----------------
lineWidth = 80
linesPerScreen = 24

startPosition = (1,1)


headerAreaHeight = 3
mainAreaHeight = 18
bottomAreaHeight = 3

emptyLine :: IO ()
emptyLine = printCharLine ' '

hashLine :: IO ()
hashLine = printCharLine '#'




---------------- functions ----------------
processInput :: String -> Person -> Location -> Game
processInput input person location
  | elem input  = (person)

getNewPosition :: String -> Position -> Position
getNewPosition input (x,y)
  | elem input upCommands = (x - 1, y)
  | elem input downCommands = (x + 1, y)
  | elem input leftCommands = (x, y - 1)
  | elem input rightCommands = (x, y + 1)
  | otherwise = (x,y)

modifyPersonStats :: Person -> Location -> Person
modifyPersonStats person location
  = let sunExMod = getLocationSunExposure location
    in modifyHydration person sunExMod


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



-- prints a line of length lineWidth of given char
printCharLine :: Char -> IO ()
printCharLine char = pCL lineWidth char []
    where pCL 0 char line = putStrLn line
          pCL count char line = pCL (count - 1) char (char:line)

getCharLine :: Char -> [Char]
getCharLine char = replicate lineWidth char

getEmptyLine :: String
getEmptyLine = build lineWidth
  where build 0 = []
        build count = ' ': build (count - 1)

createHeaderArea :: Person -> [String]
createHeaderArea p
  = ("Status # Leben: " ++ (show $ getHealth p)
       ++ " # Wasserhaushalt: " ++ (show $ getHydration p)
        ++ " # " ++ (getItemNames p))
        : (getCharLine '-') : []


createFooterArea :: [String]
createFooterArea
  = (getCharLine  '-')
    : ("Wohin möchtest gehen?")
      : (getCharLine ' ') : []
