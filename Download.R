library(data.table)
library(RCurl)
library(tidyr)
library(stringr)
URL <- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"
matchdata <- as.data.table(read.csv(URL, fileEncoding = "UTF-8", stringsAsFactors = FALSE))
teams1 <- data.table(unique(matchdata$team1))
teams2 <- data.table(unique(matchdata$team2))
teamlist <- data.table(unique(rbind(teams1, teams2)))[order(V1)]
str(teamlist)
comps <- data.table(unique(matchdata$league))[order(V1)]
runningtotals <- data.table(team = teamlist$V1,
                            datelast = as.Date("1999-12-31"),
                            numerator = 0,
                            denominator = 0)

goalratio <- 0.5
xgratio <- 0.5
nsxgratio <- 0.5
noxggoalratio <- 0.5

goalweight <- 1
xgweight <- 1
nsxgweight <- 1
noxggoalweight <- 1

matchdata2 <- matchdata[, c("date", "team1", "team2", "score1", "score2", "xg1", "xg2", "nsxg1", "nsxg2")]
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