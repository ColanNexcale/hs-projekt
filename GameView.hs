module GameView (
  printHeader,
  printBody,
  printFooter,
  printDeath,
  printSolved

) where
  import Person
  import GameMap

  lineWidth = 80
  mainAreaHeight = 18
  linesPerScreen = 24



  printHeader :: Person -> IO ()
  printHeader p = printList $ createHeaderArea p

  printBody :: Location -> String -> IO ()
  printBody location note = do
                          let description = getLocationDescription location
                          printLines 1
                          printList description
                          printLines 1
                          printLines $ mainAreaHeight - 3 - (length description )
                          printNote note


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
                              printLines spacer
                              printList endIcon
                              printLines spacer
                              printList description
                              printLines $ spacer -1
                              printList endNote
                              printLines $ spacer -1 
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
  printList [] = return ()
  printList (l:ls) = do
                        putStrLn l
                        printList ls

  getCharLine :: Char -> [Char]
  getCharLine char = replicate lineWidth char

  printLines :: Int -> IO ()
  printLines 0 = return ()
  printLines count
    = do
        putStrLn $ getCharLine ' '
        printLines (count-1)


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
