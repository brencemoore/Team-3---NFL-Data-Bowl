#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb

#load tidyverse and plays.csv
library(tidyverse)
master_data <- read.csv("plays.csv")

#Select only gameId, playID, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, and pff_manZone
working_data <- master_data |> select(gameId, playId, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, pff_manZone)
summary(working_data)

#remove row with NA values
cleaned_data <- na.omit(working_data)

#remove other values
ready_master <- cleaned_data |> filter(pff_manZone != 'Other')
summary(ready_master)

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

















