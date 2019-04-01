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
matchdata2 <- select(matchdata, date, team1, team2, score1, score2) %>%
  filter(as.Date(date) <= Sys.Date())
matchdata2a <- mutate(matchdata2, team1a = team2, team2a = team1, score1a = score2, score2a = score1) %>%
  select(date, team1 = team1a, team2 = team2a, score1 = score1a, score2 = score2a) %>%
  filter(as.Date(date) <= Sys.Date())
matchdata3 <- bind_rows(matchdata2, matchdata2a) %>%
  arrange(date)

#Final dataset in nice format
matchdata_final <- data.table(date = as.Date(matchdata3$date),
                              attack = as.factor(matchdata3$team1),
                              defence = as.factor(matchdata3$team2),
                              goals = matchdata3$score1)

#Create the model all before train/test split so factors are consistent
model_data <- model.matrix(~ attack + defence - 1, data = matchdata_final,
                  contrasts.arg=lapply(matchdata_final[,2:3], contrasts, contrasts = FALSE))

#Train/test split
train_model <- model_data[matchdata_final$date <= max(matchdata_final$date) - 14,]
test_model <- model_data[matchdata_final$date > max(matchdata_final$date) - 14,]
train_goals <- matchdata_final$goals[matchdata_final$date
                                     <= max(matchdata_final$date) - 14]
test_goals <- matchdata_final$goals[matchdata_final$date
                                    > max(matchdata_final$date) - 14]

#Setup table
parameter_table <- data.table(decay = rep(0.0, 20),
                              llambda = rep(0.0, 20),
                              mse = rep(99999.9, 20))

for(i in 0:20){
  decay <- 0.99 + i * 0.0005
  train_weights <- decay **
    (as.numeric(Sys.Date() - as.Date(matchdata3$date)))[matchdata_final$date <= max(matchdata_final$date) - 14]
  
  #Fit RR model
  base_rr_model <- glmnet(x = train_model, y = train_goals,
                          weights = train_weights, alpha = 0)
  
  #Predict on test set
  predictions <- predict(base_rr_model, test_model)
  
  #Need to make this mean by column!!
  mse <- apply((predictions - test_goals) ^ 2, 2, mean)
  parameter_table$decay[i] = decay
  llambda_mse <- as.numeric(final_rr_model$a0[which.min(mse)])
  parameter_table$llambda[i] <- llambda_mse
  parameter_table$mse[i] = min(mse)
}
plot(parameter_table$decay[1:20], parameter_table$mse[1:20])

decay <- 0.997
fit_weights <- decay ** (as.numeric(Sys.Date() - as.Date(matchdata3$date)))

#Fit RR model
final_rr_model <- glmnet(x = model_data, y = matchdata_final$goals,
                        weights = fit_weights, alpha = 0)

ridge_values <- coef(final_rr_model,s = 1.333779)[,1] # this lambda was found through CV
# These coefficients contain all the estimates for all variables so we need to find the player ones we want
attack <- data_frame(attack_team =
                              str_replace(names(ridge_values)[str_detect(names(ridge_values),
                                                                                     "attack")], "attack", ""),
                            attack_rating = unname(ridge_values[str_detect(names(ridge_values), "attack")]))
defence <- data_frame(attack_team =
                       str_replace(names(ridge_values)[str_detect(names(ridge_values),
                                                                  "defence")], "defence", ""),
                     attack_rating = unname(ridge_values[str_detect(names(ridge_values), "defence")]))
