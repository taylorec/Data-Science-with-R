library(rvest)
library(stringr)
library(tidyr)
url = 'http://espn.go.com/nfl/superbowl/history/winners'
webpage =read_html(url)
sb_table = html_nodes(webpage, 'table')
sb = html_table(sb_table)[[1]] ##acces the first table on the page
head(sb)
sb = sb[-(1:2), ]#row,column
names(sb) = c("number", "date", "site", "result")
head(sb)
sb = separate(sb, result, c('winner', 'loser'), sep=', ', remove=TRUE)
head(sb)
pattern =" \\d+$"
sb$winnerScore = as.numeric(str_extract(sb$winner, pattern))
sb$loserScore =as.numeric(str_extract(sb$loser, pattern))
sb$winner = gsub(pattern, "", sb$winner)
sb$loser =gsub(pattern, "", sb$loser)
head(sb)

plot(sb$winnerScore, col='green', type='l')
lines(sb$loserScore, col='red')

deficit <- (sb$winnerScore - sb$loserScore)
year <- as.numeric(str_sub(sb$date, -4, -1))
plot(year, deficit, type='l')

library(dplyr)
as.list(distinct(select(sb, winner)))
winners <- sort((table(select(sb,winner))), decreasing = TRUE)
winners
barplot(winners, las=2, col='blue')
