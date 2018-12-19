module GameMap (
  Position,
  Location,
  getLocationName,
  getLocationSunExposure,
  getLocationAt,
  getLocationDescription,
  startLocation,
  endLocation
) where
  import Items

  type Position = (Int, Int)

  data Location
    = Location {
      locationName :: String,
      sunExposure :: Int,
      locationDescription :: [String],
      discovery :: Maybe Water
    }


  puddleOfWater = Just (10, 50)

  smallStream = Just (100, 10)


-- Accessor function for locations --

  getLocationName :: Location -> String
  getLocationName loc = locationName loc

  getLocationSunExposure :: Location -> Int
  getLocationSunExposure loc = sunExposure loc

  getLocationDescription :: Location -> [String]
  getLocationDescription loc = locationDescription loc

  getDiscovery :: Location -> Maybe Water
  getDiscovery (Location _ _ _ (Just water) ) = Just water
  getDiscovery _ = Nothing

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
  startLocation
    = Location {
      locationName = "Waldhuette",
      sunExposure = 0,
      locationDescription = startDescription,
      discovery = Nothing
    }

  endLocation :: Location
  endLocation
    = Location {
      locationName = "Sichere Scheune",
      sunExposure = 0,
      locationDescription = endDescription,
      discovery = Nothing
    }

  deadlyLocation :: Location
  deadlyLocation
    = Location {
      locationName = "Hier wartet der Tod",
      sunExposure = 999,
      locationDescription = deadlyDescription,
      discovery = Nothing
    }
  denseForest :: Location
  denseForest
    = Location {
      locationName = "Dichter Wald",
      sunExposure = 1,
      locationDescription = denseForestDescription,
      discovery = smallStream
    }

  lightForest :: Location
  lightForest
    = Location {
      locationName = "Lichter Wald",
      sunExposure = 2,
      locationDescription = lightForestDescription,
      discovery = puddleOfWater
    }

  forestBorder :: Location
  forestBorder
    = Location {
      locationName = "Waldrand",
      sunExposure = 3,
      locationDescription = forestBorderDescription,
      discovery = Nothing
    }

  ledge :: Location
  ledge
    = Location {
      locationName = "Felsvorsprung",
      sunExposure = 5,
      locationDescription = ledgeDescription,
      discovery = Nothing
    }

  openPlain :: Location
  openPlain
    = Location {
      locationName = "Offenes Feld",
      sunExposure = 5,
      locationDescription = openPlainDescription,
      discovery = Nothing
    }

  bigRock :: Location
  bigRock
    = Location {
      locationName = "Riesieger Fels",
      sunExposure = 4,
      locationDescription = bigRockDescription,
      discovery = Nothing
    }

  -- descriptions should be 9 lines
  startDescription = [
    "Du befindest dich in einer heruntergekommenen Hütte im Teutoburger Wald.",
    "Obwohl du nicht viel geschlafen hast, weckt dich das unbarmhezige Brennen",
    "der bereits kräftigen Morgensonne. Das knochentrockene Holz knarzt leicht im",
    "Wind und ein warmer Strom zieht durch die undichten Spalten neben deinem",
    "Schlafplatz. Die letzten Tage konntest du dich durch die Wildnis schlagen,",
    "aber deine Wasservorräte werden knapp. Du solltest dich auf den Weg machen.",
    "Richtung Südosten sagten die anderen Aussätzigen bei deinem letzten Kontakt",
    "mit der Zivilisation. Das Leben abseits der Fänge von Goostlé ist hart...",
    "...aber dir ist es das Wert."
    ]

  endDescription = []

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
    "Das Blätterdach ist hier merklich dünner und du spürst eon leichtes Stechen",
    "der Sonne auf den unbedeckten Hautstellen. "
    ]

  denseForestDescription = [
    "Dich erwartet ein dichter Bereich des Waldes.",
    "Endlich. Etwas Sonnenschutz. Du atmest entspannt durch und genießt den",
    "Schatten."
    ]

  forestBorderDescription = []

  ledgeDescription = [
    "Du stehst auf einem Felsvorsprung.",
    "Auch wenn der Untergrund trittsicher scheint und die Aussicht das Risiko",
    "allemal wert ist, zögerst du dich weiter vorzuwagen. Die geschätzten 30 m",
    "freier Fall würden selbst den stabilsten Goostlé Wächter sein unsägliches",
    "KI-'Leben' kosten.",
    "Du schüttelst diese Gedanken ab und dir wird bewusst, dass der Wald hier",
    "keinen Schatten mehr spendet. Die Sonne brennt auf deiner Haut."
    ]

  bigRockDescription = []

  deadlyDescription = asciiSkullLarge


  asciiSkull = [
    "      ____",
    "    ,'   Y`.",
    "   /        \\",
    "   \\ ()  () /",
    "    `. /\\ ,'",
    "8====| \"\" |====8",
    "     `LLLU'"
    ]

  asciiSkullLarge = [
      "    @@@@@                                        @@@@@",
      "   @@@@@@@                                      @@@@@@@",
      "   @@@@@@@           @@@@@@@@@@@@@@@            @@@@@@@",
      "    @@@@@@@@       @@@@@@@@@@@@@@@@@@@        @@@@@@@@",
      "        @@@@@     @@@@@@@@@@@@@@@@@@@@@     @@@@@",
      "          @@@@@  @@@@@@@@@@@@@@@@@@@@@@@  @@@@@",
      "            @@  @@@@@@@@@@@@@@@@@@@@@@@@@  @@",
      "               @@@@@@@    @@@@@@    @@@@@@",
      "               @@@@@@      @@@@      @@@@@",
      "               @@@@@@      @@@@      @@@@@",
      "                @@@@@@    @@@@@@    @@@@@",
      "                 @@@@@@@@@@@  @@@@@@@@@@",
      "                  @@@@@@@@@@  @@@@@@@@@",
      "              @@   @@@@@@@@@@@@@@@@@   @@",
      "              @@@@  @@@@ @ @ @ @ @@@@  @@@@",
      "             @@@@@   @@@ @ @ @ @ @@@   @@@@@",
      "           @@@@@      @@@@@@@@@@@@@      @@@@@",
      "         @@@@          @@@@@@@@@@@          @@@@",
      "      @@@@@              @@@@@@@              @@@@@",
      "     @@@@@@@                                 @@@@@@@",
      "      @@@@@                                   @@@@@"
      ]
