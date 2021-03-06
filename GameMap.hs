module GameMap (
  Position,
  Location,
  getLocationName,
  getLocationSunExposure,
  getLocationAt,
  getLocationDescription,
  getWaterDiscovery,
  getHintDiscovery,
  getItemDiscovery,
  isFinal,
  startLocation,
  endLocation
) where
  import Items
  import Discovery

  type Position = (Int, Int)

  data Location
    = Location {
      locationName :: String,
      sunExposure :: Int,
      locationDescription :: [String],
      discovery :: Discovery,
      isFinal :: Bool
    }

  -- predefined discoveries
  -- type Water imported from Items
  puddleOfWater :: Maybe Water
  puddleOfWater = Just (10, 50)

  smallStream :: Maybe Water
  smallStream = Just (100, 10)


-- accessor function for locations

  getLocationName :: Location -> String
  getLocationName loc = locationName loc

  getLocationSunExposure :: Location -> Int
  getLocationSunExposure loc = sunExposure loc

  getLocationDescription :: Location -> [String]
  getLocationDescription loc = locationDescription loc

  getDiscovery :: Location -> Discovery
  getDiscovery (Location _ _ _ disc _ ) = disc

  getWaterDiscovery :: Location -> Maybe Water
  getWaterDiscovery loc = getWater $ getDiscovery loc

  getHintDiscovery :: Location -> Bool -> Hint
  getHintDiscovery loc isNewItem
    | isNewItem == True = getHint $ getDiscovery loc
    | otherwise = getHint nothingToDiscover

  getItemDiscovery :: Location -> Maybe Item
  getItemDiscovery loc = getItem $ getDiscovery loc

  -- import function for main game
  -- searches GameMap and returns location in order to access all import
  -- information of the location data type
  getLocationAt :: Position -> Location
  getLocationAt pos = findInMap pos gameMap
    where findInMap pos [] = openPlain
          findInMap (x,y) (((locX, locY), loc):locations )
            | x == locX && y == locY = loc
            | otherwise = findInMap (x,y) locations

  -- mapping from a position to a predefined location
  type GameMap = [(Position, Location)]
  gameMap = [
    (((-2),2), deadlyLocation),
    (((-1),0), deadlyLocation),
    (((-1),1), deadlyLocation),
    (((-1),2), ledge),
    ((0, (-1)), deadlyLocation),
    ((0,0), ledge),
    ((0,1), ledge),
    ((0,2), forestBorder),
    ((0,3), forestBorder),
    ((0,4), openPlain),
    ((1,0), ledgeJammerDiscovery),
    ((1,(-1)), deadlyLocation),
    ((1,1), startLocation),
    ((1,2), denseForest),
    ((1,3), denseForest),
    ((1,4), forestBorder),
    ((2,0), ledge),
    ((2,(-1)), deadlyLocation),
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

  -- predefined locations used in the GameMap
  startLocation :: Location
  startLocation
    = Location {
      locationName        = "Waldhuette",
      sunExposure         = 0,
      locationDescription = startDescription,
      discovery           = waterBottleDiscovery,
      isFinal             = False
    }

  endLocation :: Location
  endLocation
    = Location {
      locationName        = "Sichere Scheune",
      sunExposure         = 0,
      locationDescription = endDescription,
      discovery           = nothingToDiscover,
      isFinal             = True
    }

  deadlyLocation :: Location
  deadlyLocation
    = Location {
      locationName        = "Hier wartet der Tod",
      sunExposure         = 999,
      locationDescription = deadlyDescription,
      discovery           = nothingToDiscover,
      isFinal             = False
    }
  denseForest :: Location
  denseForest
    = Location {
      locationName        = "Dichter Wald",
      sunExposure         = 5,
      locationDescription = denseForestDescription,
      discovery           = smallStreamDiscovery,
      isFinal             = False
    }

  lightForest :: Location
  lightForest
    = Location {
      locationName        = "Lichter Wald",
      sunExposure         = 10,
      locationDescription = lightForestDescription,
      discovery           = puddleOfWaterDiscovery,
      isFinal             = False
    }

  forestBorder :: Location
  forestBorder
    = Location {
      locationName        = "Waldrand",
      sunExposure         = 15,
      locationDescription = forestBorderDescription,
      discovery           = nothingToDiscover,
      isFinal             = False
    }

  ledge :: Location
  ledge
    = Location {
      locationName        = "Felsvorsprung",
      sunExposure         = 20,
      locationDescription = ledgeDescription,
      discovery           = nothingToDiscover,
      isFinal             = False
    }

  ledgeJammerDiscovery :: Location
  ledgeJammerDiscovery
    = Location {
    locationName        = "Felsvorsprung",
    sunExposure         = 20,
    locationDescription = ledgeDescription,
    discovery           = jammerDiscovery,
    isFinal             = False
    }

  openPlain :: Location
  openPlain
    = Location {
      locationName        = "Offenes Feld",
      sunExposure         = 25,
      locationDescription = openPlainDescription,
      discovery           = nothingToDiscover,
      isFinal             = False
    }

  bigRock :: Location
  bigRock
    = Location {
      locationName        = "Riesieger Fels",
      sunExposure         = 20,
      locationDescription = bigRockDescription,
      discovery           = bigRockDiscovery,
      isFinal             = False
    }

  startDescription = [
    "Du befindest dich in einer heruntergekommenen Hütte im Teutoburger Wald.",
    "Obwohl du nicht viel geschlafen hast, weckt dich das unbarmhezige Brennen",
    "der bereits kräftigen Morgensonne. Das knochentrockene Holz knarzt leicht im",
    "Wind und ein warmer Strom zieht durch die undichten Spalten neben deinem",
    "Schlafplatz. Die letzten Tage konntest du dich durch die Wildnis schlagen,",
    "aber deine Wasservorräte werden knapp. Du solltest dich auf den Weg machen.",
    "Richtung Südosten, sagten die anderen Aussätzigen bei deinem letzten Kontakt",
    "mit der Zivilisation. Das Leben abseits der Fänge von Goostlé ist hart...",
    "...aber dir ist es das Wert."
    ]

  endDescription = [
    "Entkräftet erreichst du die alte Scheune.",
    "Hier wird seit Jahrzehnten keine Landwirtschaft mehr betrieben, seit Goostlé",
    "keine Wasserlizenzen mehr für freie Landwirte ausgibt. Den Erntern wäre",
    "die Hitze ja egal, aber ohne Wasser verdorrt jede Saat."
    ]

  openPlainDescription = [
    "Vor dir liegt ein offenes Feld.",
    "Die verdorrten Gräser und Sträucher sind trauriges Zeugnis der Erhitzung",
    "unseres Planeten. Damals wollte das niemand wahrhaben. Du kennst zwar die",
    "Geschichten über eine reichhaltige Vegetation hier in Mitteleuropa, hast",
    "Fotos und Videos gesehen, aber jetzt scheint dir das alles sehr unwirklich.",
    "Deine Wut auf Goostlé ist plötzlich wieder sehr präsent, du weißt warum du",
    "in den Widerstand willst. Wer das Wasser kontrolliert, kontrolliet die",
    "Menschheit. 'Ok Goostlé', bald wird abgerechnet. Aber erst musst du diese",
    "verdammte Scheune erreichen. Am besten bevor du verdurstet bist."
    ]

  lightForestDescription = [
    "Du betrittst einen lichten Bereich des Waldes.",
    "Das Blätterdach ist hier merklich dünner und du spürst ein leichtes",
    "Stechen der Sonne auf den unbedeckten Hautstellen."
    ]

  denseForestDescription = [
    "Dich erwartet ein dichter Bereich des Waldes.",
    "Endlich. Etwas Sonnenschutz. Du atmest entspannt durch und genießt den",
    "Schatten.",
    "Du weißt nicht, ob du heute schon zu viel Sonne abbekommen hast oder ob",
    "du wirklich ein leises Plätschern wahrnimmst... gibt es hier etwa Wasser?"
    ]

  forestBorderDescription = [
    "Du erreichst den Rand des Waldes.",
    "Auch wenn die Bäume hier noch etwas Schatten spenden, spürst du die Unbarm-",
    "herzigkeit der Sonne."
    ]

  ledgeDescription = [
    "Du stehst auf einem Felsvorsprung.",
    "Auch wenn der Untergrund trittsicher scheint und die Aussicht das Risiko",
    "allemal wert ist, zögerst du dich weiter vorzuwagen. Die geschätzten 30 m",
    "freier Fall würden selbst den stabilsten Goostlé Wächter sein unsägliches",
    "KI-'Leben' kosten.",
    "Du schüttelst diese Gedanken ab und dir wird bewusst, dass der Wald hier",
    "keinen Schatten mehr spendet. Die Sonne brennt auf deiner Haut."
    ]

  bigRockDescription = [
    "Du stehst am Fuße eines massiven Felsen im Wald.",
    "20 bis 30 Meter müsste er hoch sein, das wäre genug um über die",
    "umliegenden Baumwipfel zu spähen. Allerdings gibt es auf dem Weg hinauf",
    "keinerlei Schatten, du wärst der Sonne erbarmungslos ausgesetzt. Trotzdem",
    "überlegst du hinaufzuklettern, aber wäre es das Risiko wert?"
    ]

  deadlyDescription = []
