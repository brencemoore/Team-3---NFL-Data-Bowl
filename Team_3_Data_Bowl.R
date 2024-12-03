#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(kknn)
# library(lubridate)

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


# KNN
# Developed by Logan McDavid
# Source 1: https://github.com/SpencerPao/Data_Science/tree/main/KNN
# Source 2: https://www.youtube.com/watch?v=htnZp__02qw

# Additional cleaning done in order to preform knn
# manZone as factor
ready_master <- ready_master |> 
  mutate(pff_manZone = factor(pff_manZone))

ready_master <- ready_master |>
  mutate(gameClock = factor(gameClock))
summary(ready_master)

# Convert to percentage perhaps
ready_master <- ready_master |> 
  mutate(
    gameClockSeconds = as.numeric(sub(".*:", "", gameClock)),
    gameClockMinutes = as.numeric(sub(":.*", "", gameClock)),
    totalSeconds = gameClockMinutes * 60 + gameClockSeconds
  )

# NOTE: 100% = start of quarter 0% - end
ready_master <- ready_master |> 
  mutate(
    quarterPercentage = (totalSeconds / 900) * 100
  )




# Check unique pff_passCoverage values
def_ids <- unique(ready_master$pff_passCoverage)
print(def_ids) 

# Split data into training and testing sets
set.seed(123)
data_split <- initial_split(ready_master, prop = 0.7, strata = pff_passCoverage)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define KNN model
# k = sqrt(pff_passCoverage) 
knnClass <- nearest_neighbor(mode = "classification", neighbors = 3)




# ------- DEFINE RECIPE -------

# RECIPE 1: downs + yardsToGo
knnRecipe <- recipe(pff_passCoverage ~ down + yardsToGo, data = train_data) |> 
  step_normalize(all_numeric_predictors()) # Normalize values

# Assemble workflow
knnWflow <- workflow() |> 
  add_recipe(knnRecipe) |> 
  add_model(knnClass)

# Fit model
knnfit <- fit(knnWflow, train_data)

# Make predictions on test set
testPred <- predict(knnfit, test_data)


# a. CONFUSION MATRIX
# Ensure factor levels are same for confusion matrix calculation
common_levels <- union(levels(test_data$pff_passCoverage), levels(testPred$.pred_class))
confTestPred <- testPred |>
 mutate(
   pff_passCoverage = factor(test_data$pff_passCoverage, levels = common_levels),
  .pred_class = factor(.pred_class, levels = common_levels)
)

# Print accuracy
accuracy(confTestPred, truth = pff_passCoverage, estimate = .pred_class)

# Print confusion matrix
confusionMatrix <- confTestPred |>
 conf_mat(truth = pff_passCoverage, estimate = .pred_class)
confusionMatrix


# b. PREDICTED VS. ACTUAL
# Combine test dataset with predictions
test_results <- bind_cols(test_data, testPred) |>
  rename(predicted = .pred_class)

# Check mismatched rows where predictions and actual values differ
mismatches <- test_results |>
  filter(predicted != pff_passCoverage)

# Print mismatched rows
print(mismatches)

# Predicted Plot
# predicted column for color
plotPredicted <- ggplot(test_results, aes(x = down, y = yardsToGo, color = predicted)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Predicted pff_passCoverage by Down and Yards to Go",
    x = "Down",
    y = "Yards to Go",
    color = "Predicted Coverage"
  ) +
  theme_minimal()

# Actual Plot
# pff_passCoverage for color
plotActual <- ggplot(test_results, aes(x = down, y = yardsToGo, color = pff_passCoverage)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Actual pff_passCoverage by Down and Yards to Go",
    x = "Down",
    y = "Yards to Go",
    color = "Actual Coverage"
  ) +
  theme_minimal()

# Plot side by side
grid.arrange(plotPredicted, plotActual, ncol = 2)



# RECIPE 2: pff_manZone + gameClock
knnRecipe <- recipe(pff_passCoverage ~ pff_manZone + gameClock, data = train_data) |> 
  step_normalize(all_numeric_predictors()) # Normalize values

# Assemble workflow
knnWflow <- workflow() |> 
  add_recipe(knnRecipe) |> 
  add_model(knnClass)

# Fit model
knnfit <- fit(knnWflow, train_data)

# Make predictions on test set
testPred <- predict(knnfit, test_data)


# a. CONFUSION MATRIX
# Ensure factor levels are same for confusion matrix calculation
common_levels <- union(levels(test_data$pff_passCoverage), levels(testPred$.pred_class))
confTestPred <- testPred |>
  mutate(
    pff_passCoverage = factor(test_data$pff_passCoverage, levels = common_levels),
    .pred_class = factor(.pred_class, levels = common_levels)
  )

# Print accuracy
accuracy(confTestPred, truth = pff_passCoverage, estimate = .pred_class)

# Print confusion matrix
confusionMatrix <- confTestPred |>
  conf_mat(truth = pff_passCoverage, estimate = .pred_class)
confusionMatrix


# b. PREDICTED VS. ACTUAL
# Combine test dataset with predictions
test_results <- bind_cols(test_data, testPred) |>
  rename(predicted = .pred_class)

# Check mismatched rows where predictions and actual values differ
mismatches <- test_results |>
  filter(predicted != pff_passCoverage)

# Print mismatched rows
print(mismatches)

# Predicted Plot
# predicted column for color
plotPredicted <- ggplot(test_results, aes(x = pff_manZone, y = gameClock, color = predicted)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Predicted pff_passCoverage by Down and Yards to Go",
    x = "pff_manZone",
    y = "gameClock",
    color = "Predicted Coverage"
  ) +
  theme_minimal()

# Actual Plot
# pff_passCoverage for color
plotActual <- ggplot(test_results, aes(x = pff_manZone, y = gameClock, color = pff_passCoverage)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Actual pff_passCoverage by Down and Yards to Go",
    x = "pff_manZone",
    y = "gameClock",
    color = "Actual Coverage"
  ) +
  theme_minimal()

# Plot side by side
grid.arrange(plotPredicted, plotActual, ncol = 2)


# Debugging lines
str(train_data)
str(test_data)

# Check unique values and summaries
unique(train_data$pff_manZone)
unique(test_data$pff_manZone)

summary(train_data$gameClock)
summary(test_data$gameClock)

class(ready_master$pff_manZone)
# So we need to make pff_manZone a factor

class(ready_master$gameClock)
# Also split into a factor
