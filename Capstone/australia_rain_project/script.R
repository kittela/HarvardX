if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

# Downloading the data from my GitHub repository and reading it to memory
# Note the original source may be found here: https://www.kaggle.com/dhruvildave/top-play-store-games?select=android-games.csv
url <- "https://raw.githubusercontent.com/kittela/HarvardX/main/Capstone/android_games_project/android-games.csv"
download.file(url, destfile = basename(url))
games_data <- read.csv(basename(url))


games_data %>% group_by(installs) %>% summarize(n = n())