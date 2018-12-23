import GameMap
import Items
import Person
import Data.List
import GameView

-- main game loop
-- when user inputs a character corresponding to a direction or an action
-- (in the proper location), the corresponding game dialogue runs

main = do
  game

gameLoop :: Game -> IO Game
gameLoop (position, person, explorationNote)
    = do
        let currentLocation = getLocationAt position
        let currentLocationDescription = getLocationDescription currentLocation
        if getHealth person <= 0 || isFinal currentLocation
          then do
            --printDeath
            showGameEnd person currentLocation
            putStr ">>: "
            newGame <- getLine
            if newGame == "j" || newGame == "J"
              then do gameLoop start
              else do return(position, person, "")
          else do
            printHeader person
            printBody currentLocation explorationNote
            printFooter
            putStr ">>: "
            input <- getLine

          -- input analysis
            if elem input quitCommands
              then do
                return (position, person, "")
              else do
                let newGameState = processInput input person position
                gameLoop newGameState

-- starts the game loop with the initial Game
game :: IO ()
game = do
        gameLoop start
        return ()

-- input collections
quitCommands = [":q", ":Q", ":e", ":E"]
upCommands = ["w", "W"]
downCommands = ["s", "S"]
leftCommands = ["a", "a"]
rightCommands = ["d", "D"]
exploreCommands = ["u", "U"]
validInput = quitCommands
            ++ upCommands
            ++ downCommands
            ++ leftCommands
            ++ rightCommands
            ++ exploreCommands

 -- ------------------- GAME ---------------------

type Game = (Position, Person, String)
start :: Game
start =  (startPosition , startCharacter, "")



 ---------------- constants ----------------

startPosition = (1,1)

---------------- functions ----------------
processInput :: String -> Person -> Position -> Game
processInput input person position
  = if elem input exploreCommands
    then let  location = getLocationAt position
              discovery = getDiscovery location
              modPerson = modifyPersonStats person location discovery
              explorationNote = getExplorationNote discovery
              in (position, modPerson, explorationNote)

    else if elem input validInput
        then let  newPosition = getNewPosition input position
                  newLocation = getLocationAt newPosition
                  modPerson = modifyPersonStats person newLocation Nothing
                  in (newPosition, modPerson, "")
        else (position, person, "> Ungültige Eingabe")

getNewPosition :: String -> Position -> Position
getNewPosition input (x,y)
  | elem input upCommands = (x - 1, y)
  | elem input downCommands = (x + 1, y)
  | elem input leftCommands = (x, y - 1)
  | elem input rightCommands = (x, y + 1)
  | otherwise = (x,y)

showGameEnd :: Person -> Location -> IO ()
showGameEnd person location
  | getHealth person <= 0 = printDeath
  | isFinal location = printSolved $ getLocationDescription location
  | otherwise = return ()

getExplorationNote :: Maybe Water -> String
getExplorationNote Nothing = "> Du findest nichts"
getExplorationNote (Just (amount, risk))
  | amount == 100 = " > Du findest einen Bachlauf. Wie erfrischend!"
  | amount == 10 = "> Du findest eine fast ausgetrocknete Pfütze Wasser. Besser als nichts..."
  | otherwise = "Du findest etwas Wasser"

modifyPersonStats :: Person -> Location -> Maybe Water -> Person
modifyPersonStats person location Nothing
  = let sunExMod = getLocationSunExposure location
    in modifyHydration person sunExMod
modifyPersonStats person location (Just (amount, risk))
  = modifyHydration person (-amount)
