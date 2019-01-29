decay <- 0.995

#Libraries
library(data.table)
library(RCurl)
library(tidyr)
library(dplyr)
library(stringr)
library(glmnet)

#Download data
URL <- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"
matchdata <- as.data.table(read.csv(URL, fileEncoding = "UTF-8", stringsAsFactors
                                    = FALSE))[is.na(score1) == FALSE]

#Manipulate data, need 2 copies of each match so each team is the attack and defence
matchdata2 <- select(matchdata, date, team1, team2, score1, score2)
matchdata2a <- mutate(matchdata2, team1a = team2, team2a = team1, score1a = score2, score2a = score1) %>%
  select(date, team1 = team1a, team2 = team2a, score1 = score1a, score2 = score2a)
matchdata3 <- bind_rows(matchdata2, matchdata2a) %>%
  arrange(date)

#Final dataset in nice format
matchdata_final <- data.table(date = as.Date(matchdata3$date),
                              attack = as.factor(matchdata3$team1),
                              defence = as.factor(matchdata3$team2),
                              goals = matchdata3$score1,
                              weight = decay ** (as.numeric(Sys.Date() - as.Date(matchdata3$date))))

#Create the model all before train/test split so factors are consistent
model_data <- model.matrix(~ attack + defence - 1, data = matchdata_final,
                  contrasts.arg=list(attack=diag(nlevels(matchdata_final$attack)), 
                                     defence=diag(nlevels(matchdata_final$defence))))

#Train/test split
train_model <- model_data[matchdata_final$date <= max(matchdata_final$date) - 28,]
test_model <- model_data[matchdata_final$date > max(matchdata_final$date) - 28,]
train_weights <- matchdata_final$weight[matchdata_final$date
                                        <= max(matchdata_final$date) - 28]
train_goals <- matchdata_final$goals[matchdata_final$date
                                     <= max(matchdata_final$date) - 28]
test_goals <- matchdata_final$goals[matchdata_final$date
                                    > max(matchdata_final$date) - 28]

#Fit RR model
base_rr_model <- glmnet(x = train_model, y = train_goals,
                        weights = train_weights, alpha = 0)

#Predict on test set
predictions <- predict(base_rr_model, test_model)
