library(ggplot2)
library(dplyr)
library(plotly)

# getting information from data set 
batting <- read.csv('Batting.csv')

# adding Batting average for each player
batting$BA <- batting$H / batting$AB

# adding On Base Percentage for each player
batting$OBP <- (batting$H + batting$BB + batting$HBP) /
  (batting$AB + batting$BB + batting$HBP + batting$SF)

# calculating 1B figure to use for slugging percentage.
batting$X1B <-  batting$H - batting$X2B - batting$X3B - batting$HR

# adding Slugging Percentage for each player
batting$SLG <- (batting$X1B) +(2 * batting$X2B) + (3 * batting$X3B) + 
  (4 * batting$HR) / batting$AB

# loading in salary data
salaries <-  read.csv('Salaries.csv')

# creating batting subset to match salaries time frame
batting <- subset(batting, yearID >= 1985)

# merging batting and salaries
combo <- merge(batting, salaries, by = c('playerID','yearID'))

# analyzing three key players to replace
lost_players <- subset(combo, playerID %in% c('giambja01','damonjo01','saenzol01'))

# since the players left in 2001 we only want to analyze that off season
combo <- subset(combo, yearID == 2001)

# focus on their main statistics
lost_players <- subset(lost_players, select = c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB'))

# visual to see outlines and trend to set filter with
ggplot(combo, aes(x=OBP,y=salary)) + geom_point()

# filtering player replacement
combo <- subset(combo, salary < 8000000 & OBP >0)
combo <- subset(combo, AB > mean(lost_players$AB))

replacement <- combo %>% arrange(desc(OBP))
replacement <- subset(replacement, select = c('playerID','AB','salary','OBP'))

scatter_plot <- ggplot(combo, aes(x=OBP,y=salary, text=playerID)) + geom_point()

print(ggplotly(scatter_plot))
print(head(replacement))