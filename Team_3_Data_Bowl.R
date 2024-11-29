#Team 3:Samuel Hartmann, Logan McDavid, Brence Moore, Nathan Lamb
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(kknn)

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
# Check for unique pff_passCoverage values
def_ids <- unique(CLE_data$pff_passCoverage)
print(def_ids)  # Ensure there is variability in the target variable

# Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(ready_master, prop = 0.7, strata = pff_passCoverage)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define the KNN model
CLEknnClass <- nearest_neighbor(mode = "classification", neighbors = 4)

# Define the recipe with normalization
CLEknnRecipe <- recipe(pff_passCoverage ~ down + yardsToGo, data = train_data) |> 
  step_normalize(all_numeric_predictors())

# Assemble the workflow
CLEknnWflow <- workflow() |> 
  add_recipe(CLEknnRecipe) |> 
  add_model(CLEknnClass)

# Fit the model
CLEknnfit <- fit(CLEknnWflow, train_data)

# Make predictions on the test set
testPred <- predict(CLEknnfit, test_data)

# I could be crazy but I think when I was running this code with my new code it was overtraining my data.
# I will look into it in a little bit

# Confusion Matrix Mark out
# Ensure factor levels are the same for confusion matrix calculation

# common_levels <- union(levels(test_data$pff_passCoverage), levels(testPred$.pred_class))
# testPred <- testPred |>
#  mutate(
#    pff_passCoverage = factor(test_data$pff_passCoverage, levels = common_levels),
#   .pred_class = factor(.pred_class, levels = common_levels)
# )

# Print accuracy
# accuracy(testPred, truth = pff_passCoverage, estimate = .pred_class)

# Print confusion matrix
# confusionMatrix <- testPred |>
#  conf_mat(truth = pff_passCoverage, estimate = .pred_class)
# confusionMatrix





# # Working to plot the data
# # Combine test dataset with predictions
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








