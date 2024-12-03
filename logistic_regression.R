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
  geom_smooth(formula ='y ~ x', method = "glm", method.args = list(family = "bi>
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

