module GameView (
  printHeader,
  printBody,
  printFooter,
  printDeath

) where
  import Person
  import GameMap

  lineWidth = 80
  mainAreaHeight = 18
  linesPerScreen = 24



  printHeader :: Person -> IO ()
  printHeader p = printList $ createHeaderArea p

  printBody :: Location -> IO ()
  printBody location = do
                          let description = getLocationDescription location
                          printLines 1
                          printList description
                          printLines $ mainAreaHeight - 1 - (length description )


  printFooter :: IO ()
  printFooter = do
                  printList createFooterArea


  printDeath :: IO ()
  printDeath = do
                  printList asciiSkullLarge
                  printList deathNote

  createFooterArea :: [String]
  createFooterArea
    = (getCharLine  '-')
      : ("Wohin möchtest gehen?")
        : (getCharLine ' ') : []



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
    "Du bist gestorben.",
    "Möchtest du ein neues Spiel beginnen? j/n"
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
