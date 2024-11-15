#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb

#load tidyverse and plays.csv
library(tidyverse)
library(tidymodels)
library(kknn)
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


# We are looking to find how the defense performed based on all unique offensive plays ran 

# Looking at what defensive alignment the defense goes into based on all of the variables in the test
summary(CLE_data)

# KNN is hwat I am going to try lol
# Start with down and yards to go, seems simple enough right?
# So in my head I am thinking x axis is down and y axis is yards until first down
# Lets work with cleveland 

# Getting a k value:
#  We could do the sqrt(n) where n is the number of plays

# Finding unique variables
def_ids <- unique(CLE_data$pff_passCoverage)
print(def_ids)

# START: Split into training and testing 
set.seed(123)
data_split <- initial_split(CLE_data, prop = 0.7, strata = pff_passCoverage)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define the model
CLEknnClass <- nearest_neighbor(mode = "classification", neighbors = 9)

# Define recipe
CLEknnRecipe <- recipe(pff_passCoverage ~ down + yardsToGo, data = train_data) |>
  step_normalize(all_numeric_predictors())

# Assemble workflow
CLEknnWflow <- workflow() |>
  add_recipe(CLEknnRecipe) |>
  add_model(CLEknnClass)

# Fit the model
CLEknnfit <- fit(CLEknnWflow, train_data)

# Make predictions on test set
testPred <- predict(CLEknnfit, test_data)

# Ensure factor levels are the same for confusion matrix calculation
common_levels <- union(levels(test_data$pff_passCoverage), levels(testPred$.pred_class))
testPred <- testPred |>
  mutate(
    pff_passCoverage = factor(test_data$pff_passCoverage, levels = common_levels),
    .pred_class = factor(.pred_class, levels = common_levels)
  )

# Print accuracy
accuracy(testPred, truth = pff_passCoverage, estimate = .pred_class)

# Print confusion matrix
confusionMatrix <- testPred |>
  conf_mat(truth = pff_passCoverage, estimate = .pred_class)
confusionMatrix











