module GameView (
  printHeader,
  printBody,
  printFooter,
  printDeath,
  printSolved

) where
  import Person
  import GameMap

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
                          printEmptyLines $ mainAreaHeight - 3
                            - (length description ) - length note - 2
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


  createFooterArea :: [String]
  createFooterArea
    = (getCharLine  '-')
      : ("Was möchtest du tun?")
        : ("Bewegen: w, a, s,d || Die Umgebung untersuchen: u") : []



  createHeaderArea :: Person -> [String]
  createHeaderArea p
    = ("Status # Leben: " ++ (show $ getHealth p)
         ++ " # Wasserhaushalt: " ++ (show $ getHydration p)
          ++ " # " ++ (getItemNames p))
          : (getCharLine '-') : []

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
