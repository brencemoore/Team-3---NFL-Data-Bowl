# Team 3 Data Bowl
# Logan McDavid, Samual Hartmann, Nathan Lamb, Brence Moore
# main.r
# Using different models to predict defensive scheme


# Code for reading in and sorting data - Samuel
# load tidyverse and plays.csv
library(tidyverse)
master_data <- read.csv("plays.csv")

# Select only gameId, playID, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, and pff_manZone
working_data <- master_data |> select(gameId, playId, quarter, down, yardsToGo, defensiveTeam, gameClock, pff_passCoverage, pff_manZone)
summary(working_data)

# Plot the working data - Logan
ggplot(working_data, aes(x = pff_passCoverage)) +
  geom_bar(fill = "purple", color = "black") +
  labs(
    title = "Total Count of Defensive Schemes",
    x = "Defensive Scheme",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Review Na and Other Values
Na_data <- master_data |> filter(is.na(pff_manZone))
Other_data <- master_data |> filter(pff_manZone == 'Other')
print(Na_data$playDescription)
print(Other_data$playDescription)

# remove row with NA values
cleaned_data <- na.omit

# remove other values
cleaned_master <- cleaned_data |> filter(pff_manZone != 'Other')
summary(cleaned_master)

# Plot the cleaned data - Logan
ggplot(cleaned_master, aes(x = pff_passCoverage)) +
  geom_bar(fill = "purple", color = "black") +
  labs(
    title = "Total Count of Defensive Schemes",
    x = "Defensive Scheme",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# get counts of each defensive alignment
count_defs <- cleaned_master |> count(pff_passCoverage)
print(count_defs)

# clean Cover-3 of variants and Cover-1 of Cover-1 Double
ready_master <- cleaned_master |> mutate(pff_passCoverage = case_when(
  pff_passCoverage == "Cover-3 Cloud Left" ~ "Cover-3",
  pff_passCoverage == "Cover-3 Cloud Right" ~ "Cover-3",
  pff_passCoverage == "Cover-3 Double Cloud" ~ "Cover-3",
  pff_passCoverage == "Cover-1 Double" ~ "Cover-1",
  TRUE ~ pff_passCoverage))

# Plot the master data - Logan
ggplot(ready_master, aes(x = pff_passCoverage)) +
  geom_bar(fill = "purple", color = "black") +
  labs(
    title = "Total Count of Defensive Schemes",
    x = "Defensive Scheme",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# get counts of each defensive alignment after cleaning
count_defs <- ready_master |> count(pff_passCoverage)
print(count_defs)

# get list of team Ids
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
#end code generated by Samuel

# ------------------------------------
# METHOD: K-NEAREST NEIGHBOR
# Developed by Logan McDavid
# Source 1: https://github.com/SpencerPao/Data_Science/tree/main/KNN
# Source 2: https://www.youtube.com/watch?v=htnZp__02qw
# ------------------------------------

# Packages included
library(tidymodels)
library(gridExtra)
library(kknn)

# Additional cleaning done in order to preform knn
# manZone as factor
ready_master <- ready_master |> 
  mutate(pff_manZone = factor(pff_manZone))

# Convert to seconds
ready_master <- ready_master |>
  mutate(gameClockSec = sapply(strsplit(gameClock, ":"), function(x) {
    minutes <- as.numeric(x[1])
    seconds <- as.numeric(x[2])
    minutes * 60 + seconds
  }))

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
# I have found this one to not be very helpful
knnRecipe <- recipe(pff_passCoverage ~ pff_manZone + gameClockSec, data = train_data) |> 
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
plotPredicted <- ggplot(test_results, aes(x = pff_manZone, y = gameClockSec, color = predicted)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Predicted pff_passCoverage by pff_manZone and gameClock",
    x = "pff_manZone",
    y = "gameClock",
    color = "Predicted Coverage"
  ) +
  scale_y_continuous(
    breaks = seq(min(test_results$gameClockSec, na.rm = TRUE), 
                 max(test_results$gameClockSec, na.rm = TRUE), 
                 by = 60) # Adjust the interval if necessary
  ) +
  theme_minimal()

# Actual Plot
# pff_passCoverage for color
plotActual <- ggplot(test_results, aes(x = pff_manZone, y = gameClockSec, color = pff_passCoverage)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Actual pff_passCoverage by pff_manZone and gameClock",
    x = "pff_manZone",
    y = "gameClock",
    color = "Actual Coverage"
  ) +
  scale_y_continuous(
    breaks = seq(min(test_results$gameClockSec, na.rm = TRUE), 
                 max(test_results$gameClockSec, na.rm = TRUE), 
                 by = 60) # Adjust the interval if necessary
  ) +
  theme_minimal()

# Plot side by side
grid.arrange(plotPredicted, plotActual, ncol = 2)



# RECIPE 3: pff_manZone + yardsToGo
knnRecipe <- recipe(pff_passCoverage ~ pff_manZone + yardsToGo, data = train_data) |> 
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
plotPredicted <- ggplot(test_results, aes(x = pff_manZone, y = yardsToGo, color = predicted)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Predicted pff_passCoverage by Down and Yards to Go",
    x = "pff_manZone",
    y = "Yards to Go",
    color = "Predicted Coverage"
  ) +
  theme_minimal()

# Actual Plot
# pff_passCoverage for color
plotActual <- ggplot(test_results, aes(x = pff_manZone, y = yardsToGo, color = pff_passCoverage)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Actual pff_passCoverage by Down and Yards to Go",
    x = "pff_manZone",
    y = "Yards to Go",
    color = "Actual Coverage"
  ) +
  theme_minimal()

# Plot side by side
grid.arrange(plotPredicted, plotActual, ncol = 2)



# ------------------------------------
# METHOD: RANDOM FOREST
# Developed by Logan McDavid
# Source: https://www.youtube.com/watch?v=ZaMlS5hj2Kk
# ------------------------------------

# Packages included
library(tidyverse)
library(randomForest)

# Convert pff_passCoverage to factors for ready_master
ready_master$pff_passCoverage <- as.factor(ready_master$pff_passCoverage)

# Convert time to seconds
ready_master <- ready_master |>
  mutate(gameClockSec = sapply(strsplit(gameClock, ":"), function(x) {
    minutes <- as.numeric(x[1])
    seconds <- as.numeric(x[2])
    minutes * 60 + seconds
  }))

# Random forest of pass coverages based on:
# quarter, downm yardsToGo, gameClock, defensiveTeam, pff_manZone
# Number of trees = 500
rf <- randomForest(pff_passCoverage ~ quarter + down + yardsToGo + gameClockSec + defensiveTeam + pff_manZone, importance = TRUE, data = ready_master, ntree = 500)
rf


# Predict specific instance
new_data <- data.frame(quarter = 2, down = 2, yardsToGo = 7, 
                       gameClockSec = "7:00", defensiveTeam = "CLE",
                       pff_manZone = "Zone")  # Example
new_data$pff_passCoverage <- predict(rf, newdata = new_data)
print(new_data$pff_passCoverage)


# Print variable importance plot 
varImpPlot(rf)



# ------------------------------------
# METHOD: Naive Bayes
# Developed by Samuel Hartmann
# Source(s) - zybooks for model and recipe
# blackbox.ai for GGplot of model results
# ------------------------------------


#split quarter, down, yards to go, gameclock, and pass_coverage off into new df
pass_coverage <- ready_master %>%
  select(quarter, down, yardsToGo, gameClock, pff_passCoverage)

#set seed
set.seed(110)

# Convert time to seconds
pass_coverage <- pass_coverage |>
  mutate(gameClockSec = sapply(strsplit(gameClock, ":"), function(x) {
    minutes <- as.numeric(x[1])
    seconds <- as.numeric(x[2])
    minutes * 60 + seconds
  }))

# Split the data into training and testing sets stratafied on pff_passCoverage
data_split <- initial_split(pass_coverage, prop = 0.70, strata = pff_passCoverage)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create a recipe with quarter, down, yardsToGo, and gameClockSec
coverage_recipe <- recipe(pff_passCoverage ~quarter + down + yardsToGo + gameClockSec, data = train_data)

# Prepare the recipe
prepared_recipe <- prep(coverage_recipe)

# Bake the training and test data based on recipe
train_baked <- prepared_recipe |> bake(new_data = NULL)
test_baked <- prepared_recipe |> bake(new_data = test_data)

#marticize the train data
x <- train_baked |> select(-pff_passCoverage) |> as.matrix()

#extract the test data
y <- train_baked$pff_passCoverage

# Fit the Multinomial Naive Bayes model using naivebayes::multinomial_naive_bayes
mnb <- naivebayes::multinomial_naive_bayes(x, y, laplace = 1)

# Add the predictions from the model onto the dataset
train_predict <- train_data |>  mutate(.pred_class = predict(mnb, x))

# Create a confusion matrix for the training data using the predictions stored in train_predict
train_confusion_matrix <- table(Predicted = train_predict$.pred_class, Actual = train_predict$pff_passCoverage)
print("Training Confusion Matrix:")
print(train_confusion_matrix)

# Predict from test data
test_matrix <- test_baked |> select(-pff_passCoverage) |> as.matrix()
test_predict <- test_data |> mutate(.pred_class = predict(mnb, test_matrix))

# Create a confusion matrix for the test data
test_confusion_matrix <- table(Predicted = test_predict$.pred_class, Actual = test_data$pff_passCoverage)
print("Test Confusion Matrix:")
print(test_confusion_matrix)

# Calculate accuracy for the test data
test_accuracy <- sum(test_predict$.pred_class == test_data$pff_passCoverage) / nrow(test_data)
print(paste("Test Accuracy:", round(test_accuracy, 2)))

# Convert the confusion matrix to a data frame
confusion_long <- as.data.frame(test_confusion_matrix)

# Check the structure of the confusion_long data frame
str(confusion_long)

# Rename columns to make them clearer
colnames(confusion_long) <- c("Predicted", "Actual", "Count")

# Create a new column for Correct/Incorrect prediction
confusion_long <- confusion_long %>%
  mutate(Prediction = ifelse(Actual == Predicted, "Correct", "Incorrect"))

# Create the ggplot visualization
ggplot(confusion_long, aes(x = Actual, y = Count, fill = Prediction)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confusion Matrix Visualization",
       x = "Actual Class",
       y = "Count",
       fill = "Prediction") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------
# METHOD: Logistic Regression
# Developed by Brence Moore
# Source(s) - zyBooks
# ------------------------------------

library(tidyverse)
library(tidymodels)

master_data <- read.csv("plays.csv")

#Select only gameId, playID, quarter, down, yardsToGo, defensiveTeam, gameClock>
working_data <- master_data |> select(gameId, playId, quarter, down, yardsToGo,>
summary(working_data)

#remove row with NA values
cleaned_data <- na.omit(working_data)

#remove other values
ready_master <- cleaned_data |> filter(pff_manZone != 'Other')

# Creates a new data frame where M = playing man and Z = playing zone
ready_master <- ready_master |>
  mutate(pff_manZone = ifelse(pff_manZone == "Man",'M','Z'))

# Converts gameClock from a character to an integer that represents seconds
ready_master <- ready_master |>
  mutate(gameClockSec = sapply(strsplit(gameClock, ":"), function(x) {
    minutes <- as.numeric(x[1])
    seconds <- as.numeric(x[2])
    minutes * 60 + seconds
  }))

ready_master$pff_manZone <- as.factor(ready_master$pff_manZone)
summary(ready_master)

# Splits the data into training and testing data
split <- initial_split(ready_master, prop=.7)
train_data <- training(split)
test_data <- testing(split)


# Initialize a logistic regression model in tidymodels using logistic_reg
logisticModel <- logistic_reg(mode="classification", engine="glm")

# Fit a logistic regression model in tidymodels
logisticModel_fit <- logisticModel |>
  fit(pff_manZone ~ yardsToGo, data=train_data)

# Create a binary diagnosis feature with 1 if zone and 0 if man
train_data <- train_data |>
  mutate(pff_manZone = ifelse(pff_manZone=="Z", 1, 0))

# Graph logistic regression probabilities with variable yardsToGo
train_data |>
  ggplot(aes(x=yardsToGo, y=pff_manZone)) +
  geom_point() +
  geom_smooth(formula ='y ~ x', method = "glm", method.args = list(family = "binomial"),
              se = FALSE) +
  labs(x="yards to go", y="probability of zone")

test_data <- augment(logisticModel_fit, test_data)

# Predicted probabilities are added "to the right"

# Prints error rate for predicting man and zone
test_data |> mn_log_loss(pff_manZone, .pred_M)
test_data |> mn_log_loss(pff_manZone, .pred_Z)



# ------------------------------------
# METHOD: 
# Developed by 
# Source(s) - 
# ------------------------------------



# ------------------------------------
# METHOD: 
# Developed by 
# Source(s) - 
# ------------------------------------
