#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb

#load tidyverse and plays.csv
library(tidyverse)
master_data <- read.csv("plays.csv")

#Select only gameId, playID, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, and pff_manZone
working_data <- master_data |> select(gameId, playId, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, pff_manZone)
summary(working_data)

#remove row with NA values
cleaned_data <- na.omit(working_data)
print(cleaned_data)
