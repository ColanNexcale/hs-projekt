module GamteState (
  GameState

) where
  import Items

  lineWidth = 80


  data Event
    = Sunburn {impact :: Int}
    | Dehydration {impact :: Int}

  data Discovery
    = NewItem Item
    | Water {amount :: Int, healthRisk :: Int}

  data GameState
    = Story {description :: [String], header :: [String]}
    | Movement {description :: [String], header :: [String], event :: Maybe Event}
    | Exploration {description :: [String], header :: [String], discovery :: Maybe Discovery}

  getHeader :: GameState -> [String]
  getHeader gameState = header gameState

  getDescription :: GameState -> [String]
  getDescription gameState = description gameState

  getDiscovery :: GameState -> Maybe Discovery
  getDiscovery (Exploration _ _ disc) = Just disc
  getDiscovery _ = Nothing

  getEvent :: GameState -> Maybe Event
  getEvent (Movement _ _ event) = event
  getEvent _ = Nothing

  createHeader :: Person -> [String]
  createHeader p = ("Status # Leben: " ++ (show $ getHealth p)
       ++ " # Wasserhaushalt: " ++ (show $ getHydration p)
        ++ " # " ++ (getItemNames p))
        : (getCharLine '-') : []

  getCharLine :: Char -> [Char]
  getCharLine char = replicate lineWidth char
