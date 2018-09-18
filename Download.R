library(data.table)
library(RCurl)
library(tidyr)
library(stringr)
URL <- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"
matchdata <- as.data.table(read.csv(URL, fileEncoding = "UTF-8", stringsAsFactors
                                    = FALSE))[is.na(score1) == FALSE]
teams1 <- data.table(unique(matchdata$team1))
teams2 <- data.table(unique(matchdata$team2))
teamlist <- data.table(unique(rbind(teams1, teams2)))[order(V1)]
comps <- data.table(unique(matchdata$league))[order(V1)]
runningtotals <- data.table(team = teamlist$V1,
                            datelast = as.Date("1999-12-31"),
                            numerator = 0,
                            denominator = 0)

goalratio <- 0.5
xgratio <- 0.5
nsxgratio <- 0.5
noxggoalratio <- 0.5

goalweight <- 1/3
xgweight <- 1/3
nsxgweight <- 1 - goalweight - xgweight
noxggoalweight <- 1

attritionrate <- 0.995

matchdata2 <- matchdata[is.na(score1) == FALSE, c("date", "team1", "team2", "score1", "score2", "xg1", "xg2", "nsxg1", "nsxg2")]
matchdata2$date <- as.Date(matchdata2$date)
matchdata2$perfscore1 <- ifelse(is.na(matchdata2$xg1) == FALSE,
                                goalweight * ifelse(matchdata2$score1 > matchdata2$score2, 1 -
                                               (1 - goalratio) ^ (matchdata2$score1 - matchdata2$score2),
                                             (1 - goalratio) ^ (matchdata2$score2 - matchdata2$score1)
                                             - 1) +
                         xgweight * ifelse(matchdata2$xg1 > matchdata2$xg2, 1 - 
                                             (1 - xgratio) ^ (matchdata2$xg1 - matchdata2$xg2),
                                           (1 - xgratio) ^ (matchdata2$xg2 - matchdata2$xg1) - 1) +
                         nsxgweight * ifelse(matchdata2$nsxg1 > matchdata2$nsxg2, 1 - 
                                               (1 - nsxgratio) ^ (matchdata2$nsxg1 - matchdata2$nsxg2),
                                             (1 - nsxgratio) ^ (matchdata2$nsxg2 - matchdata2$nsxg1) - 1),
                         noxggoalweight * ifelse(matchdata2$score1 > matchdata2$score2, 1 -
                                               (1 - noxggoalratio) ^ (matchdata2$score1 - matchdata2$score2),
                                             (1 - noxggoalratio) ^ (matchdata2$score2 - matchdata2$score1)
                                             - 1))
matchdata2$perfscore2 <- - matchdata2$perfscore1
matchdata2$rating.after1 <- 0
matchdata2$rating.after2 <- 0

calcs <- data.table(rating1 = as.numeric(0),
                    rating2 = as.numeric(0),
                    numerator1 = as.numeric(0),
                    numerator2 = as.numeric(0),
                    denominator1 = as.numeric(0),
                    denominator2 = as.numeric(0))

for (i in 1:nrow(matchdata2)){
  rating1 <- ifelse(runningtotals$denominator[runningtotals$team == matchdata2$team1[i]] == 0,
                    0,
                    runningtotals$numerator[runningtotals$team == matchdata2$team1[i]]/
                      runningtotals$denominator[runningtotals$team == matchdata2$team1[i]])
  rating2 <- ifelse(runningtotals$denominator[runningtotals$team == matchdata2$team2[i]] == 0,
                    0,
                    runningtotals$numerator[runningtotals$team == matchdata2$team2[i]]/
                      runningtotals$denominator[runningtotals$team == matchdata2$team2[i]])
  numerator1 <- runningtotals$numerator[runningtotals$team == matchdata2$team1[i]] *
    attritionrate ** as.integer(difftime(matchdata2$date[i],
                                         runningtotals$datelast[runningtotals$team == matchdata2$team1[i]],
                                         "days")) + matchdata2$perfscore1[i] + rating2
  numerator2 <- runningtotals$numerator[runningtotals$team == matchdata2$team2[i]] *
    attritionrate ** as.integer(difftime(matchdata2$date[i],
                                         runningtotals$datelast[runningtotals$team == matchdata2$team2[i]],
                                         "days")) + matchdata2$perfscore2[i] + rating1
  denominator1 <- runningtotals$denominator[runningtotals$team == matchdata2$team1[i]] *
    attritionrate ** as.integer(difftime(matchdata2$date[i],
                                         runningtotals$datelast[runningtotals$team == matchdata2$team1[i]],
                                         "days")) + 1
  denominator2 <- runningtotals$denominator[runningtotals$team == matchdata2$team2[i]] *
    attritionrate ** as.integer(difftime(matchdata2$date[i],
                                         runningtotals$datelast[runningtotals$team == matchdata2$team2[i]],
                                         "days")) + 1
  matchdata2$rating.after1[i] <- numerator1 / denominator1
  matchdata2$rating.after2[i] <- numerator2 / denominator2
  
  runningtotals$numerator[runningtotals$team == matchdata2$team1[i]] <- numerator1
  runningtotals$numerator[runningtotals$team == matchdata2$team2[i]] <- numerator2
  runningtotals$denominator[runningtotals$team == matchdata2$team1[i]] <- denominator1
  runningtotals$denominator[runningtotals$team == matchdata2$team2[i]] <- denominator2
  runningtotals$datelast[runningtotals$team == matchdata2$team1[i]] <- matchdata2$date[i]
  runningtotals$datelast[runningtotals$team == matchdata2$team2[i]] <- matchdata2$date[i]
}
ratingtable <- data.table(team = runningtotals$team, rating = runningtotals$numerator /
                            runningtotals$denominator)[order(-rating)]