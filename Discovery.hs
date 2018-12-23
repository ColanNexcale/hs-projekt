module Discovery (
  Discovery,
  Hint,
  getWater,
  getHint,
  getItem,
  bigRockDiscovery,
  puddleOfWaterDiscovery,
  smallStreamDiscovery,
  jammerDiscovery,
  waterBottleDiscovery,
  nothingToDiscover
) where
  import Items

  type Hint = [String]

  data Discovery = Discovery (Maybe Water) Hint (Maybe Item)| None

  getWater :: Discovery -> Maybe Water
  getWater (Discovery Nothing _ _ ) = Nothing
  getWater (Discovery water _ _ )   = water

  getHint :: Discovery -> Hint
  getHint (Discovery _ hint _) = hint

  getItem :: Discovery -> Maybe Item
  getItem (Discovery _ _ Nothing) = Nothing
  getItem (Discovery _ _ item)    = item

  bigRockDiscovery = Discovery Nothing bigRockHint Nothing

  puddleOfWaterDiscovery = Discovery puddleOfWater puddleOfWaterHint Nothing
  puddleOfWaterHint = [
    "> Du findest eine fast ausgetrocknete Pfütze Wasser.",
    "Besser als nichts..."
    ]

  smallStreamDiscovery = Discovery smallStream smallStreamHint Nothing
  smallStreamHint = ["> Du findest einen Bachlauf. Wie erfrischend!"]

  jammerDiscovery = Discovery Nothing jammerHint $ Just jammer
  jammerHint = [
    "> Du findest einen Elektronik Jammer. Ungefährlich für Menschen, aber",
    "tödlich für Maschinen. Damit kannst du sämtliche Elektronik in einem",
    "Kegel vor dir lahmlegen."
    ]

  waterBottleDiscovery = Discovery Nothing waterBottleHint $ Just  waterBottle
  waterBottleHint = [
    "> Du findest eine alte Wasserflasche. Sie hat zwar schon bessere Tage",
    "gesehen, aber diese Dinger sind gebaut für die Ewigkeit. Du kannst etwas",
    "Wasser darin transportieren und unterwegs trinken."
    ]

  nothingToDiscover = Discovery Nothing nothingHint Nothing
  nothingHint = ["> Hier gibt es nichts zu entdecken."]

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
