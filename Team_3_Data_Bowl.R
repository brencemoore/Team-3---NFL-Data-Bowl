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

print(team_Ids)

#"ATL" "DAL" "TEN" "TB"  "SEA" "NE"  "DET" "WAS" "LA"  "MIN" "BAL" "LV"  "IND" "BUF"
#"NYJ" "HOU" "JAX" "NYG" "LAC" "DEN" "CAR" "PIT" "ARI" "CIN" "GB"  "PHI" "KC"  "MIA"
#"SF"  "CHI" "CLE" "NO" 

# splits data into testing and training data to test logistic regression
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

# Graph logistic regression probabilities
train_data |>
  ggplot(aes(x=yardsToGo, y=pff_manZone)) + 
  geom_point() + 
  geom_smooth(formula ='y ~ x', method = "glm", method.args = list(family = "binomial"), 
              se = FALSE) +
  labs(x="yards to go", y="probability of zone")

test_data <- augment(logisticModel_fit, test_data)

# Predicted probabilities are added "to the right"
#head(test_data) #[, 31:35]
#head(train_data)

#test_data$.pred_class
#test_data$pff_manZone

test_data |> mn_log_loss(pff_manZone, .pred_M)
test_data |> mn_log_loss(pff_manZone, .pred_Z)

ready_master |>
  ggplot(aes(x = yardsToGo, y = down, color = pff_manZone)) + 
  geom_point() + 
  labs(x = "yards to go", y = "down") + 
  scale_color_manual(values = c("blue", "red"), labels = c("Zone", "Man"))

ggplot(ready_master, aes(x=pff_passCoverage, y=down, fill=pff_passCoverage)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

datasets_list <- list(BUF_data, MIA_data, NJY_data, NE_data)#, 
                      KC_data, LAC_data, DEN_data, LV_data, 
                      PIT_data, BAL_data, CIN_data, CLE_data, 
                      HOU_data, IND_data, TEN_data, JAX_data, 
                      PHI_data, WAS_data, DAL_data, NYG_data, 
                      ARI_data, SF_data, LA_data, STL_data, 
                      DET_data, MIN_data, GB_data, CHI_data, 
                      ATL_data, TB_data, NO_data, CAR_data)

# Add an identifier for each dataset and combine them into one data frame
combined_data <- bind_rows(
  lapply(seq_along(datasets_list), function(i) {
    datasets_list[[i]] %>%
      mutate(dataset_id = paste("Dataset", i))  # Add an identifier column
  })
)

# Plot combined data with dataset_id as fill
ggplot(combined_data, aes(x = pff_passCoverage, y = yardsToGo, fill = dataset_id)) + 
  geom_boxplot(alpha = 0.3) +
  theme(legend.position = "right") +
  labs(x = "PFF Pass Coverage", y = "Down", fill = "Dataset")

ggplot(combined_data, aes(x = dataset_id, y = yardsToGo, fill = pff_passCoverage)) + 
  geom_boxplot(alpha = 0.3) +
  theme(legend.position = "none") +
  labs(x = "Dataset", y = "yards to go")

ggplot(combined_data, aes(x = dataset_id, y = yardsToGo, fill = pff_passCoverage)) + 
  geom_violin(alpha = 0.3, position = position_dodge(width = 0.75)) +  # Adds density-based violin plot
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), alpha = 0.5) +  # Adds boxplot within the violin plot
  theme(legend.position = "right") +
  labs(x = "Dataset", y = "Yards to Go", fill = "Pass Coverage")
