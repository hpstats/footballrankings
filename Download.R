library(data.table)
library(RCurl)
library(tidyr)
library(stringr)
URL <- "https://raw.githubusercontent.com/openfootball/eng-england/master/2018-19/1-premierleague.txt"
x <- as.data.table(strsplit(getURL(URL, ssl.verifypeer = FALSE), '\n')[[1]])
z <- x[substr(x$V1, 1, 1) != '#' & x$V1 != '' & substr(x$V1, 1, 8) != 'Matchday']
z$date <- as.Date(ifelse(substr(z$V1, 1, 1) == '[', substr(z$V1, 6, str_locate(z$V1, ']') - 1), NA),
                  format = "%b/%d")
z <- fill(z, date)
zz <- z[substr(z$V1, 1, 1) != '[']
