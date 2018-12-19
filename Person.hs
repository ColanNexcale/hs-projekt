module Person(
  Person,
  getPersonName,
  getHealth,
  modifyHealth,
  getSunExposure,
  modifySunExposure,
  getHydration,
  modifyHydration,
  getItems,
  addItem,
  removeItem,
  getItemNames,
  startCharacter
) where

  import Items

  data Person = Person {personName:: String, health :: Int, sunExposure :: Int, hydration :: Int, items :: [Item]}

  data PersonAttribute = Health | SunExposure | Hydration
    deriving (Eq)

  -- modifies a person attribute
  -- addition/subtraction is decided upon most probable case in game flow
  modifyAttribute :: Person -> PersonAttribute -> Int -> Person
  modifyAttribute (Person name health sunExp hydration items) attr modifier
    = case  attr of
            Health -> Person name (health - modifier) sunExp hydration items
            SunExposure -> Person name health (sunExp + modifier) hydration items
            Hydration -> Person name health sunExp (hydration - modifier) items
  getPersonName :: Person -> String
  getPersonName p = personName p

  getHealth :: Person -> Int
  getHealth p = health p

  -- subtracts the healthModifier from a persons heath
  -- pass negative int to add health
  modifyHealth :: Person -> Int -> Person
  modifyHealth pers healthModifier = modifyAttribute pers Health healthModifier

  getSunExposure :: Person -> Int
  getSunExposure p = sunExposure p

  -- adds the sunExposureModifier to a persons sunExposure
  -- pass positive int to lower sunExp
  modifySunExposure :: Person -> Int -> Person
  modifySunExposure pers sunExMod = modifyAttribute pers SunExposure sunExMod

  getHydration :: Person -> Int
  getHydration p = hydration p

  -- subtracts the hydrationModifier from a persons hydration
  -- pass negative int to add to hydration
  modifyHydration :: Person -> Int -> Person
  modifyHydration pers hydrModifier = modifyAttribute pers Hydration hydrModifier

  getItems :: Person -> [Item]
  getItems p = items p

  getItemNames :: Person -> String
  getItemNames (Person  _  _  _  _  items) = format (map (getItemName) items) []
    where format [] _ = "Keine Items"
          format [item] [] = item
          format [item] items = items ++ (", ") ++item
          format (i:is) items = (format is items) ++ i


  addItem :: Person -> Item -> Person
  addItem (Person n he s hy items) newItem = Person n he s hy (items ++ [newItem])

  removeItem :: Person -> Item -> Person
  removeItem (Person n he s hy items) rmvItem
    = Person n he s hy (filter (/= rmvItem) items)

  startHealth = 100
  startSunExposure = 0
  startHydration = 100
  startItems = []


  startCharacter = Person {
    personName = "Mosh",
    health = startHealth,
    sunExposure = startSunExposure,
    hydration = startHydration,
    items = startItems
  }
