module GameView (
  printHeader,
  printBody,
  printFooter,
  printDeath,
  printSolved,
  printPrologue

) where
  import Person
  import GameMap
  import Items

  lineWidth       = 80
  mainAreaHeight  = 18
  linesPerScreen  = 24

  -- bundles all methods for displaying the game progress
  -- constants and relative amounts of blank lines ensure integer screen
  -- display on every "frame" of the game

  printHeader :: Person -> IO ()
  printHeader p = printList $ createHeaderArea p

  printBody :: Location -> [String] -> IO ()
  printBody location note = do
                          let description = getLocationDescription location
                          printEmptyLines 1
                          printList description
                          printEmptyLines $ mainAreaHeight - 6
                            - (length description ) - length note
                          printList note
                          printEmptyLines 1


  printFooter :: IO ()
  printFooter = do
                  printList createFooterArea

  printNote :: String -> IO ()
  printNote note = do
                      putStrLn note

  printDeath :: IO ()
  printDeath = do
                  printList asciiSkullLarge
                  printList deathNote
                  putStrLn newGameRequest

  printSolved :: [String] -> IO ()
  printSolved description = do
                              let spacer
                                    = div (linesPerScreen - length description
                                    - length endNote - length endIcon) 4
                              printEmptyLines spacer
                              printList endIcon
                              printEmptyLines spacer
                              printList description
                              printEmptyLines $ spacer -1
                              printList endNote
                              printEmptyLines $ spacer -1
                              putStrLn newGameRequest


  printPrologue :: IO ()
  printPrologue = do
                    printList prologue

  createFooterArea :: [String]
  createFooterArea
    = (getCharLine  '-')
      : ("Was möchtest du tun?")
        : ("Bewegen: w, a, s,d || Die Umgebung untersuchen: u") : []



  createHeaderArea :: Person -> [String]
  createHeaderArea p
    = ("Status # Leben: " ++ (show $ getHealth p)
         ++ " # Wasserhaushalt: " ++ (show $ getHydration p))
          : (formatItems p)
            : (getCharLine '-') : []

  formatItems :: Person -> String
  formatItems person
    = let items = getItems person
          formattedItems = map (getItemInfo) items
      in joinItems formattedItems

  joinItems :: [String] -> String
  joinItems [] = "Keine Items"
  joinItems [i] = i
  joinItems (i:is) = i ++ (", ") ++ joinItems is


  printList :: [String] -> IO ()
  printList []      = return ()
  printList (l:ls)  = do
                        putStrLn l
                        printList ls

  getCharLine :: Char -> [Char]
  getCharLine char = replicate lineWidth char

  printEmptyLines :: Int -> IO ()
  printEmptyLines 0     = return ()
  printEmptyLines count = do
                            putStrLn $ getCharLine ' '
                            printEmptyLines (count-1)


  deathNote :: [String]
  deathNote = [
    "Du bist gestorben."
    ]

  endNote :: [String]
  endNote = [
    "Du hast dein Ziel sicher - und lebendig erreicht, jetzt kannst du dich in",
    "Ruhe darum kümmern in den Widerstand einzutreten."
    ]

  newGameRequest :: String
  newGameRequest = "> Möchtest du ein neues Spiel beginnen? j/n"

  prologue = [
    "           #### #### #### Age of Goostlé #### #### ####",
    "",
    "Wir schreiben das Jahr 2139 und direkt nach dem Menschen selbst hat sich",
    "das Klima zum zweitgrößten Feind der Menschheit erhoben. Die globale",
    "Erwärmung hat im letzten Jahrhundert über 2 Milliarden Menschen das Leben",
    "gekostet - durch Krieg, Hunger- und Durstnöte. Die Fusion von Google und",
    "Nestlé sorgte dafür das der fusionierte Megakonzern Goostlé sowohl das",
    "weltweite Wasser als auch die Maschinen kontrolliert und brachte damit",
    "spätestens seit den Wasserkriegen Ende des 21. Jahrhunderts viel Leid",
    "die Menschheit. Einstige Supermächte zerfielen und wurden abhängig vom",
    "Erfolg ihrer Lobbyarbeit in den Goostlé Gremien.",
    "Natürlich nehmen das nicht alle hin. Immer wieder formierte sich der",
    "Widerstand. Doch es ist mittlerweile nicht mehr möglich einen Computer an-",
    "zuschalten ohne, dass Goostlé davon Wind bekommt. Doch es gibt Gerüchte",
    "über eine neue Bewegung, die eine alternative Kommunikationsform gefunden",
    "haben soll. Du bist fest entschlossen diese Menschen zu finden.",
    "Und Widerstand zu leisten...",
    "",
    "",
    "Möchtest du dich auf dem Weg machen, dem Widerstand beizutreten? j/n"
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

  endIcon = [
    "                     .d88888888b.",
    "                    d88P\"    \"Y88b",
    "                    888        888",
    "                    Y88b      d88P",
    "                      88bo  od88",
    "                    d88888  88888b"
    ]
