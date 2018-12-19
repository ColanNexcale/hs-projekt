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
gameLoop (position, person)
    = do
        let currentLocation = getLocationAt position
        let currentLocationDescription = getLocationDescription currentLocation
        if getHealth person <= 0
          then do
            printDeath
            putStr ">>: "
            newGame <- getLine
            if newGame == "j" || newGame == "J"
              then do gameLoop start
              else do return(position, person)
          else do
            printHeader person
            printBody currentLocation
            printFooter
            putStr ">>: "
            input <- getLine

          -- input analysis
            if elem input quitCommands
              then do
                return (position, person)
              else do
                let newPosition = getNewPosition input position
                let newLocation = getLocationAt newPosition
                let modPerson = modifyPersonStats person newLocation
                gameLoop (newPosition, modPerson)

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


 -- ------------------- GAME ---------------------

type Game = (Position, Person)
start :: Game
start =  (startPosition , startCharacter)



 ---------------- constants ----------------

startPosition = (1,1)


---------------- functions ----------------


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
