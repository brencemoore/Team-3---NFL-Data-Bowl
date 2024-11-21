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

#_________________________________________________________________
#Decision Tree
# Load libraries
library(rpart)
library(rpart.plot)

# Load the data
plays <- read.csv(file.choose())

# Filter for plays where the Buffalo Bills are on offense
BUF_data <- subset(plays, possessionTeam == "BUF")

# Feature engineering
BUF_data$scoreDiff <- BUF_data$preSnapHomeScore - BUF_data$preSnapVisitorScore  # Score differential
BUF_data$yardsToGoCategory <- cut(BUF_data$yardsToGo, breaks = c(0, 3, 7, 10, 20, Inf),
                                  labels = c("Short", "Medium", "Long", "Very Long", "Extreme"), right = FALSE)

# Select and clean relevant columns
BUF_clean <- na.omit(BUF_data[, c("down", "yardsToGo", "yardsToGoCategory", "quarter", 
                                  "absoluteYardlineNumber", "offenseFormation", 
                                  "pff_passCoverage", "scoreDiff", "pff_runConceptPrimary")])

# Ensure categorical variables are factors
BUF_clean$pff_runConceptPrimary <- as.factor(BUF_clean$pff_runConceptPrimary)
BUF_clean$offenseFormation <- as.factor(BUF_clean$offenseFormation)
BUF_clean$pff_passCoverage <- as.factor(BUF_clean$pff_passCoverage)
BUF_clean$yardsToGoCategory <- as.factor(BUF_clean$yardsToGoCategory)

# Split data into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(BUF_clean), size = 0.8 * nrow(BUF_clean))
train_data <- BUF_clean[train_indices, ]
test_data <- BUF_clean[-train_indices, ]

# Fit the decision tree model
decision_tree <- rpart(pff_runConceptPrimary ~ down + yardsToGoCategory + quarter + 
                         absoluteYardlineNumber + offenseFormation + pff_passCoverage + scoreDiff, 
                       data = train_data, method = "class")

# Visualize the decision tree
rpart.plot(decision_tree)

# Evaluate the model
test_predictions <- predict(decision_tree, newdata = test_data, type = "class")
accuracy <- sum(test_predictions == test_data$pff_runConceptPrimary) / nrow(test_data)
cat("Test Accuracy:", accuracy, "\n")
















