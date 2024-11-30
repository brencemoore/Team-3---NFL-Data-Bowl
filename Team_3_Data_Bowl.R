#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb
library(tidyverse)
library(randomForest)
library(rpart)
master_data <- read.csv("plays.csv")

#Select only gameId, playID, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, and pff_manZone
working_data <- master_data |> select(gameId, playId, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, pff_manZone)
summary(working_data)

#Review Na and Other Values
Na_data <- master_data |> filter(is.na(pff_manZone))
Other_data <- master_data |> filter(pff_manZone == 'Other')
print(Na_data$playDescription)
print(Other_data$playDescription)

#remove row with NA values
cleaned_data <- na.omit(working_data)

#remove other values
cleaned_master <- cleaned_data |> filter(pff_manZone != 'Other')
summary(cleaned_master)

#get counts of each defensive alignment
count_defs <- cleaned_master |> count(pff_passCoverage)
print(count_defs)

#clean Cover-3 of variants and Cover-1 of Cover-1 Double
ready_master <- cleaned_master |> mutate(pff_passCoverage = case_when(
  pff_passCoverage == "Cover-3 Cloud Left" ~ "Cover-3",
  pff_passCoverage == "Cover-3 Cloud Right" ~ "Cover-3",
  pff_passCoverage == "Cover-3 Double Cloud" ~ "Cover-3",
  pff_passCoverage == "Cover-1 Double" ~ "Cover-1",
  TRUE ~ pff_passCoverage))

#get counts of each defensive alignment after cleaning
count_defs <- ready_master |> count(pff_passCoverage)
print(count_defs)

#get list of team Ids
team_Ids <- unique(ready_master$defensiveTeam)
print(team_Ids)

#AFC East
BUF_data <- ready_master  |> filter(defensiveTeam == 'BUF')
MIA_data <- ready_master  |> filter(defensiveTeam == 'MIA')
NJY_data <- ready_master  |> filter(defensiveTeam == 'NYJ')
NE_data <- ready_master  |> filter(defensiveTeam == 'NE')

#AFC West
KC_data <- ready_master  |> filter(defensiveTeam == 'KC')
LAC_data <- ready_master  |> filter(defensiveTeam == 'LAC')
DEN_data <- ready_master  |> filter(defensiveTeam == 'DEN')
LV_data <- ready_master  |> filter(defensiveTeam == 'LV')

#AFC North
PIT_data <- ready_master  |> filter(defensiveTeam == 'PIT')
BAL_data <- ready_master  |> filter(defensiveTeam == 'BAL')
CIN_data <- ready_master  |> filter(defensiveTeam == 'CIN')
CLE_data <- ready_master  |> filter(defensiveTeam == 'CLE')

#AFC South
HOU_data <- ready_master  |> filter(defensiveTeam == 'HOU')
IND_data <- ready_master  |> filter(defensiveTeam == 'IND')
TEN_data <- ready_master  |> filter(defensiveTeam == 'TEN')
JAX_data <- ready_master  |> filter(defensiveTeam == 'JAX')

#NFC East
PHI_data <- ready_master  |> filter(defensiveTeam == 'PHI')
WAS_data <- ready_master  |> filter(defensiveTeam == 'WAS')
DAL_data <- ready_master  |> filter(defensiveTeam == 'DAL')
NYG_data <- ready_master  |> filter(defensiveTeam == 'NYG')

#NFC West
ARI_data <- ready_master  |> filter(defensiveTeam == 'ARI')
SF_data <- ready_master  |> filter(defensiveTeam == 'SF')
LA_data <- ready_master  |> filter(defensiveTeam == 'LA')
STL_data <- ready_master  |> filter(defensiveTeam == 'STL')

#NFC North
DET_data <- ready_master  |> filter(defensiveTeam == 'DET')
MIN_data <- ready_master  |> filter(defensiveTeam == 'MIN')
GB_data <- ready_master  |> filter(defensiveTeam == 'GB')
CHI_data <- ready_master  |> filter(defensiveTeam == 'CHI')

#NFC South
ATL_data <- ready_master  |> filter(defensiveTeam == 'ATL')
TB_data <- ready_master  |> filter(defensiveTeam == 'TB')
NO_data <- ready_master  |> filter(defensiveTeam == 'NO')
CAR_data <- ready_master  |> filter(defensiveTeam == 'CAR')


# Random Forest
#Developed by Logan McDavid
# Source: https://www.youtube.com/watch?v=ZaMlS5hj2Kk


# Convert pff_passCoverage to factors for ready_master
ready_master$pff_passCoverage <- as.factor(ready_master$pff_passCoverage)

# Random forest of pass coverages based on:
# quarter, downm yardsToGo, gameClock, defensiveTeam, pff_manZone
# Number of trees = 500
rf <- randomForest(pff_passCoverage ~ quarter + down + yardsToGo + gameClock + defensiveTeam + pff_manZone, importance = TRUE, data = ready_master, ntree = 500)
rf


# Predict specific instance
new_data <- data.frame(quarter = 2, down = 2, yardsToGo = 7, 
                       gameClock = "7:00", defensiveTeam = "CLE",
                       pff_manZone = "Zone")  # Example
new_data$pff_passCoverage <- predict(rf, newdata = new_data)
print(new_data$pff_passCoverage)


# Print variable importance plot 
varImpPlot(rf)














