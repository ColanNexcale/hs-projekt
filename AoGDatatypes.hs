module AoGDatatypes (
  GameState,
  Location,
  Person,
  Item,
  Day,
  Daytime,
  Event,
  templateLocation,
  getLocationName
) where

data GameState = Documentation {content :: [String], location :: Location} | Story {content :: [String], location :: Location}
getContent :: GameState -> [String]
getContent gs = content gs

getLocation :: GameState -> String
getLocation gameState = getLocationName $ location gameState

data Location = Location1 {locationName :: String} | Location2 {locationName :: String}

getLocationName :: Location -> String
getLocationName loc = locationName loc

data Person = Agonist {characterName:: String, health :: Int, sunExposure :: Int, hydration :: Int, items :: [Item]}
  | Deuteragonist {characterName :: String, health :: Int, sunExpose :: Int, hydration :: Int, items :: [Item]}

data Item = Tool {itemName :: String} | Weapon {itemName:: String, damage :: Int, range :: Int} | Vessel {itemName :: String, capacity :: Int, fill :: Int}

data Day = Day {dayNum :: Int, dayName :: String, dayTime :: Daytime, events :: [Event] }

data Daytime = Daytime | Nighttime

data Event = EnemySpawn | Sunlight

------mainChar---------- pre build locations ----------------
templateLocation :: Location
templateLocation = Location1 "Raum"
