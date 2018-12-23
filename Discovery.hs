module Discovery (
  Discovery,
  Hint,
  getWater,
  getHint,
  bigRockDiscovery,
  puddleOfWaterDiscovery,
  smallStreamDiscovery,
  nothingToDiscover
) where
  import Items
  --import GameMap

  type Hint = [String]


  data Discovery = Discovery (Maybe Water) Hint | None

  getWater :: Discovery -> Maybe Water
  getWater (Discovery Nothing _ ) = Nothing
  getWater (Discovery (Just (amount, risk)) _ ) = Just (amount, risk)

  getHint :: Discovery -> Hint
  getHint (Discovery _ hint) = hint

  bigRockDiscovery = Discovery Nothing bigRockHint

  puddleOfWaterDiscovery = Discovery puddleOfWater puddleOfWaterHint
  puddleOfWaterHint = ["> Du findest eine fast ausgetrocknete Pfütze Wasser. Besser als nichts..."]

  smallStreamDiscovery = Discovery smallStream smallStreamHint
  smallStreamHint = ["> Du findest einen Bachlauf. Wie erfrischend!"]

  nothingToDiscover = Discovery Nothing nothingHint

  nothingHint = ["> Hier gibt es nichts zu entdecken"]

  puddleOfWater :: Maybe Water
  puddleOfWater = Just (10, 50)

  smallStream :: Maybe Water
  smallStream = Just (100, 10)

  bigRockHint :: Hint
  bigRockHint = [
    "Du machst dich daran diesen massiven Felsklumpen heraufzuklettern, auch",
    "wenn das aufgehitzte Gestein viel Disziplin beim Aufstieg fordert. Doch du",
    "beißt die Zähne zusammen und erreichst schließlich die Felsspitze. Diese",
    "liegt etwas über den Baumwipfeln und ja! Da! Weiter südöstlich entdeckst",
    "du tatsächlich die alte Scheune! Die Mühe hat sich also gelohnt."
    ]
