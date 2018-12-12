module Items (
  Item,
  waterBottle,
  getItemName,
  getBottleFill,
  getBottleCapacity,
  reduceBottleFill
) where

  data Item = Tool {itemName :: String}
    | Weapon {itemName:: String, damage :: Int, range :: Int}
    | Vessel {itemName :: String, capacity :: Int, fill :: Int}
    deriving (Eq)

  waterBottle :: Item
  waterBottle = Vessel {itemName = "Wasserflasche", capacity = 50, fill = 50}

  getBottleFill :: Int
  getBottleFill = fill waterBottle

  reduceBottleFill :: Int -> Item -> Item
  reduceBottleFill withdrawValue (Vessel name capac fill) = Vessel name capac (fill - withdrawValue)
  reduceBottieFill otherItem = otherItem

  getBottleCapacity :: Int
  getBottleCapacity = capacity waterBottle

  getItemName :: Item -> String
  getItemName item = itemName item
