setwd("C://Users/jwolo/OneDrive/Sport-Analytics/02-Support/")
# Part 1
box.score <- read.csv("./01_manhattan.csv")
box.score$PTS <- box.score$X2P*2 + box.score$X3P*3 + box.score$FT
box.score


box.score$FG.PCT <- round(box.score$FG / box.score$FGA,2)
box.score

starting.five <- box.score[1:5,]
barplot(starting.five$PTS~starting.five$Player)

barplot(starting.five$PTS~as.character(starting.five$Player))

# Part 2
file.paths <- list.files(pattern="0*_*.csv")

all.games <- data.frame()

for(path in file.paths){
  all.games <- rbind(all.games, read.csv(path))  
}



apply(all.games[,2:ncol(all.games)], 2, sum)
tapply(all.games$FG, all.games$Player, sum)
aggregate(all.games[,3]~all.games$Player, FUN=sum, simplify = T, drop = T)
