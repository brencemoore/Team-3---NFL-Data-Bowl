#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb

#load tidyverse and plays.csv
library(tidyverse)
library(tidymodels)
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

# creates a new data frame where M = playing man and Z = playing zone
new_ready_data <- ready_master |>
  mutate(pff_manZone = ifelse(pff_manZone == "Man",'M','Z'))

new_ready_data$pff_manZone <- as.factor(new_ready_data$pff_manZone)


# splits data into testing and training data to test logistic regression
split <- initial_split(new_ready_data, prop=.7)
train_data <- training(split)
test_data <- testing(split)


# Initialize a logistic regression model in tidymodels using logistic_reg
logisticModel <- logistic_reg(mode="classification", engine="glm")

new_ready_data$gameClock

# Fit a logistic regression model in tidymodels
logisticModel_fit <- logisticModel |>
  fit(pff_manZone ~ quarter + down + yardsToGo + gameClock, data=train_data)

# Create a binary diagnosis feature with 1 if zone and 0 if man
train_data <- train_data |>
  mutate(pff_manZone = ifelse(pff_manZone=="Z", 1, 0))

# Graph logistic regression probabilities
train_data |>
  ggplot(aes(x=yardsToGo, y=pff_manZone)) + 
  geom_point() + 
  geom_smooth(formula ='y ~ x', method = "glm", method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(x="yards to go", y="probability of zone")

test_data <- augment(logisticModel_fit, test_data)

# Predicted probabilities are added "to the right"
head(test_data) #[, 31:35]

test_data |> mn_log_loss(pff_manZone, .pred_M)



