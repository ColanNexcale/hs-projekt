import GameMap
import Items
import Person
import Data.List
import GameView

-- main function for immediate gamestart via compiled executable
main = do game

  -- main game loop
  -- when user inputs a character corresponding to a direction or an action
  -- (in the proper location), the corresponding game dialogue runs
gameLoop :: Game -> IO Game
gameLoop (position, person, explorationNote)
    = do
        let currentLocation = getLocationAt position
        if getHealth person <= 0 || isFinal currentLocation
          then do
            showGameEnd person currentLocation
            putStr ">>: "
            newGame <- getLine
            if newGame == "j" || newGame == "J"
              then do gameLoop start
              else do return(position, person, [])
          else do
            printHeader person
            printBody currentLocation explorationNote
            printFooter
            putStr ">>: "
            input <- getLine

            if elem input quitCommands
              then do
                return (position, person, [])
              else do
                let newGameState = processInput input person position
                gameLoop newGameState

-- starts the game loop with the initial Game
game :: IO ()
game = do
        printPrologue
        x <- getLine
        if x == "j" || x == "J"
          then do
                  gameLoop start
                  return()
        else do
                putStrLn "Bye"
                return ()

-- input collections for distinction
quitCommands    = [":q", ":Q", ":e", ":E"]
upCommands      = ["w", "W"]
downCommands    = ["s", "S"]
leftCommands    = ["a", "a"]
rightCommands   = ["d", "D"]
exploreCommands = ["u", "U"]
validInput = quitCommands
            ++ upCommands
            ++ downCommands
            ++ leftCommands
            ++ rightCommands
            ++ exploreCommands

 -- ------------------- GAME ---------------------

type Game = (Position, Person, [String])
start :: Game
start =  (startPosition , startCharacter, [])



 ---------------- constants ----------------

startPosition = (1,1)

---------------- functions ----------------

-- distinguishes between movement and explore and modifies person/location
-- according to intended game logic
processInput :: String -> Person -> Position -> Game
processInput input person position
  | elem input exploreCommands = handleExploration person position
  | elem input validInput      = handleMovement input person position
  | otherwise                  = (position, person, ["> Ungültige Eingabe"])
--  = if elem input exploreCommands
  --  then
    --      handleExploration person position
    --else if elem input validInput
    --    then
    --    else (position, person, ["> Ungültige Eingabe"])

getNewPosition :: String -> Position -> Position
getNewPosition input (x,y)
  | elem input upCommands     = (x - 1, y)
  | elem input downCommands   = (x + 1, y)
  | elem input leftCommands   = (x, y - 1)
  | elem input rightCommands  = (x, y + 1)
  | otherwise = (x,y)


-- processes exploration events i.e. modifying persons health/hydration status
-- and adding items
handleExploration :: Person -> Position -> Game
handleExploration person position
  = let location        = getLocationAt position
        newItem         = getItemDiscovery location
        waterDiscovery  = getWaterDiscovery location
        modPerson       = addItem newItem
          $ modifyPersonStats person location waterDiscovery
        hintDiscovery   = getHintDiscovery location $ isNewItem person newItem
    in (position, modPerson, hintDiscovery)

-- processes movement and modifies person health/hydration according to the
-- upcomming location
handleMovement :: String -> Person -> Position -> Game
handleMovement input person position
  = let newPosition = getNewPosition input position
        newLocation = getLocationAt newPosition
        modPerson   = modifyPersonStats person newLocation Nothing
    in (newPosition, modPerson, [])

-- distinguishes between death and successful game end and delegates screen
-- output to GameView methods
showGameEnd :: Person -> Location -> IO ()
showGameEnd person location
  | getHealth person <= 0 = printDeath
  | isFinal location      = printSolved $ getLocationDescription location
  | otherwise = return ()


-- used to change person states according to discovery events or the standard
-- hydration modifier from a locations attribute
modifyPersonStats :: Person -> Location -> Maybe Water -> Person
modifyPersonStats person location Nothing
  = let sunExMod = getLocationSunExposure location
    in modifyHydration person sunExMod
modifyPersonStats person location (Just (amount, risk))
  = modifyHydration person (-amount)
