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

  import Items

  data Person = Person {personName:: String, health :: Int, hydration :: Int, items :: [Item]}

  data PersonAttribute = Health | Hydration
    deriving (Eq)

  -- modifies a person attribute
  -- default modification is subtraction
  modifyAttribute :: Person -> PersonAttribute -> Int -> Person
  modifyAttribute (Person name health hydration items) attr modifier
    = case  attr of
            Health -> Person name (health - modifier) hydration items
            Hydration -> Person name health (hydration - modifier) items

  getPersonName :: Person -> String
  getPersonName p = personName p

  getHealth :: Person -> Int
  getHealth p = health p

  -- subtracts the healthModifier from a persons heath
  -- pass negative int to add health
  modifyHealth :: Person -> Int -> Person
  modifyHealth pers healthModifier = modifyAttribute pers Health healthModifier



  getHydration :: Person -> Int
  getHydration p = hydration p

  -- subtracts the hydrationModifier from a persons hydration
  -- pass negative int to add to hydration
  modifyHydration :: Person -> Int -> Person
  modifyHydration (Person n he hy i) (999) = Person n 0 hy i
  modifyHydration (Person n he hy i) hydrModifier
    | hy <= 0 = modifyAttribute (Person n he hy i) Health hydrModifier
    | hy - hydrModifier <= 0 = Person n he 0 i
    | hy - hydrModifier >= 100 = Person n he 100 i
    | otherwise = modifyAttribute (Person n he hy i) Hydration hydrModifier

  getItems :: Person -> [Item]
  getItems p = items p

  getItemNames :: Person -> String
  getItemNames (Person  _  _  _  items) = format (map (getItemName) items) []
    where format [] _ = "Keine Items"
          format [item] [] = item
          format [item] items = items ++ (", ") ++item
          format (i:is) items = (format is items) ++ i


  addItem :: Person -> Item -> Person
  addItem (Person n he hy items) newItem = Person n he hy (items ++ [newItem])

  removeItem :: Person -> Item -> Person
  removeItem (Person n he hy items) rmvItem
    = Person n he hy (filter (/= rmvItem) items)

  startHealth = 100
  startHydration = 100
  startItems = []


  startCharacter = Person {
    personName = "Mosh",
    health = startHealth,
    hydration = startHydration,
    items = startItems
  }
