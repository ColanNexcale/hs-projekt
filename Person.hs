module Person(
  Person,
  getPersonName,
  getHealth,
  modifyHealth,
  getHydration,
  modifyHydration,
  getItems,
  addItem,
  removeItem,
  getItemNames,
  startCharacter
) where

  -- bundles all methods to get and modify a persons attributes

  import Items

  data Person = Person {
    personName:: String,
    health :: Int,
    hydration :: Int,
    items :: [Item]}

  data PersonAttribute = Health | Hydration
    deriving (Eq)

  -- accessor functions for person attributes

  getPersonName :: Person -> String
  getPersonName p = personName p

  getHealth :: Person -> Int
  getHealth p = health p

  getHydration :: Person -> Int
  getHydration p = hydration p

  -- general purpose modification function
  -- default modification operation is subtraction
  modifyAttribute :: Person -> PersonAttribute -> Int -> Person
  modifyAttribute (Person name health hydration items) attr modifier
    = case  attr of
            Health    -> Person name (health - modifier) hydration items
            Hydration -> Person name health (hydration - modifier) items


  -- subtracts the healthModifier from a persons heath
  -- pass negative int to add health
  modifyHealth :: Person -> Int -> Person
  modifyHealth pers healthModifier = modifyAttribute pers Health healthModifier


  -- subtracts the hydrationModifier from a persons hydration
  -- pass negative int to add to hydration
  modifyHydration :: Person -> Int -> Person
  modifyHydration (Person n he hy i) (999) = Person n 0 hy i
  modifyHydration (Person n he hy i) modifier
    | hy <= 0 && modifier > 0
      = modifyAttribute (Person n he hy i) Health modifier
    | hy - modifier <= 0
      = modifyAttribute (Person n he 0 i) Health (modifier - hy)
    | hy - modifier >= 100
      = Person n he 100 i
    | otherwise
      = modifyAttribute (Person n he hy i) Hydration modifier


  -- functions for item management
  -- was planned but not implemented in final version
  getItems :: Person -> [Item]
  getItems p = items p

  getItemNames :: Person -> String
  getItemNames (Person  _  _  _  items) = format (map (getItemName) items) []
    where format [] _         = "Keine Items"
          format [item] []    = item
          format [item] items = items ++ (", ") ++item
          format (i:is) items = (format is items) ++ (", ") ++ i


  addItem :: Maybe Item -> Person -> Person
  addItem Nothing p         = p
  addItem (Just newItem) p  = addConditionally p newItem
    where addConditionally (Person n he hy items) newItem
            | elem newItem items  = Person n he hy items
            | otherwise           = Person n he hy (items ++ [newItem])

  removeItem :: Person -> Item -> Person
  removeItem (Person n he hy items) rmvItem
    = Person n he hy (filter (/= rmvItem) items)

  startHealth = 100
  startHydration = 100
  startItems = []


  startCharacter = Person {
    personName  = "Mosh",
    health      = startHealth,
    hydration   = startHydration,
    items       = startItems
  }
