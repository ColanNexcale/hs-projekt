module GameMap (
  Position,
  Location,
  getLocationName,
  getLocationSunExposure,
  getLocationAt
) where


  type Position = (Int, Int)

  data Location = Location {locationName :: String, sunExposure :: Int}
    deriving (Show)

  -- Accessor function for locations --

  getLocationName :: Location -> String
  getLocationName loc = locationName loc

  getLocationSunExposure :: Location -> Int
  getLocationSunExposure loc = sunExposure loc

  getLocationAt :: Position -> Location
  getLocationAt pos = findInMap pos gameMap
    where findInMap pos [] = deadlyLocation
          findInMap (x,y) (((locX, locY), loc):locations )
            | x == locX && y == locY = loc
            | otherwise = findInMap (x,y) locations

  type GameMap = [(Position, Location)]
  gameMap = [
    ((0,0), ledge),
    ((0,1), ledge),
    ((0,2), forestBorder),
    ((0,3), forestBorder),
    ((0,4), openPlain),
    ((1,0), ledge),
    ((1,1), startLocation),
    ((1,2), denseForest),
    ((1,3), denseForest),
    ((1,4), forestBorder),
    ((2,0), ledge),
    ((2,1), lightForest),
    ((2,2), bigRock),
    ((2,3), forestBorder),
    ((2,4), openPlain),
    ((3,0), forestBorder),
    ((3,1), forestBorder),
    ((3,2), forestBorder),
    ((3,3), forestBorder),
    ((3,4), openPlain),
    ((4,0), openPlain),
    ((4,1), openPlain),
    ((4,2), openPlain),
    ((4,3), openPlain),
    ((4,4), endLocation)
    ]

  startLocation :: Location
  startLocation = Location "Waldhuette" 0

  endLocation :: Location
  endLocation = Location "Sichere Scheune" 0

  deadlyLocation :: Location
  deadlyLocation = Location "Hier wartet der Tod" 10

  denseForest :: Location
  denseForest = Location "dichter Wald" 1

  lightForest :: Location
  lightForest = Location "lichter Wald" 2

  forestBorder :: Location
  forestBorder = Location "Waldrand" 3

  ledge :: Location
  ledge = Location "Felsvorsprung" 5

  openPlain :: Location
  openPlain = Location "offenes Feld" 5

  bigRock :: Location
  bigRock = Location "riesiger Fels" 4
