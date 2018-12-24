module Items (
  Item,
  Water,
  waterBottle,
  isVessel,
  getItemName,
  getItemInfo,
  getBottleFill,
  getBottleCapacity,
  reduceBottleFill,
  jammer
) where
  -- not fully implemented
  -- can be used in further developement

  data Item = Tool {itemName :: String}
    | Weapon {itemName:: String, damage :: Int}
    | Vessel {itemName :: String, capacity :: Int, fill :: Int}
    deriving (Eq)

  -- (Amount, Healthrisk)
  type Water = (Int, Int)

  waterBottle :: Item
  waterBottle = Vessel {itemName = "Wasserflasche", capacity = 50, fill = 50}

  isVessel :: Item -> Bool
  isVessel (Vessel _ _ _) = True
  isVessel _ = False

  getBottleFill :: Int
  getBottleFill = fill waterBottle

  reduceBottleFill :: Int -> Item -> Item
  reduceBottleFill withdrawValue (Vessel name capac fill)
    = Vessel name capac (fill - withdrawValue)
  reduceBottieFill otherItem = otherItem

  getBottleCapacity :: Int
  getBottleCapacity = capacity waterBottle

  getItemName :: Item -> String
  getItemName item = itemName item

  jammer :: Item
  jammer = Tool "Elektronik Jammer"


  getItemInfo :: Item -> String
  getItemInfo (Vessel n cap fill)
    = n ++ " - Menge: " ++ (show fill) ++ " - Max: " ++(show cap)
  getItemInfo item = itemName item
