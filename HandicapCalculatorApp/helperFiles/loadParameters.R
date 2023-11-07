# loading required parameters
LIST_OF_CLUBS <- read_csv("data/listOfClubs.csv")$club
LIST_OF_TEES <- c("Weiss", "Gelb", "Blau", "Rot", "Grün")
TEE_INFORMATION <- read_csv("data/teeInformation.csv")
COURSE_INFORMATION <- read_csv("data/courseInformation.csv")
LIST_OF_PLAYERS <- read_csv("data/listOfPlayers.csv")$player
HANDICAP_RESULTS <- read_csv("data/handicapResults.csv")