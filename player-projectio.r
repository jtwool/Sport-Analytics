setwd("C:/Users/jwolo/Downloads/Sport-Analytics/Sport-Analytics/08-Support/")

Rush.2013 <- read.csv(file="./NFL-rushers-2013.csv", row.names=1) 
Rush.2014 <- read.csv(file="./NFL-rushers-2014.csv", row.names=1) 
Rush.2015 <- read.csv(file="./NFL-rushers-2015.csv", row.names=1) 
Rush.2016 <- read.csv(file="./NFL-rushers-2016.csv", row.names=1)

all.players <- rbind(Rush.2013, Rush.2014, Rush.2015, Rush.2016)

z.scale <- function(x){(x-mean(x)) / sd(x)}

neighbors.matrix <- function(x){
  df <- apply(x, MARGIN=2, FUN=z.scale)
  N <- nrow(df)
  df2 <- matrix(0, nrow = N, ncol = N)
  for(i in 1:N){
    for(j in 1:N){
      rmse <- sqrt(sum((df[i,]-df[j,])^2))
      df2[i,j] <- -1*rmse 
    }
  }
  return(round(df2,2))
}

past <- rbind(Rush.2013,Rush.2014)
past[is.na(past)] <- 0

past.similarity <- neighbors.matrix(past)
row.names(past.similarity) <- row.names(past)
colnames(past.similarity) <- row.names(past)

similar.w.stats <- function(player,k=10){
  similar.players <- sort(past.similarity[player,],decreasing = T)[2:nrow(past)]
  similar.player.names <- row.names(data.frame(similar.players))
  df <- past[player,]
  for (name in similar.player.names){df <- rbind(df, past[name, ])}
  df$similarity <- c(NA, z.scale(similar.players))
  df$similarity <- df$similarity / max(df$similarity, na.rm=T)
  return(head(df,k+1))
}

name.to.name <- function(x){
  paste0(strsplit(x, " ")[[1]][c(1,2)], collapse = " ")
}

name.to.year <- function(x){
  tokens <- strsplit(x, " ")[[1]]
  tail(tokens,1)
}

lookup.and.average <- function(player.names, weights){
  seasons <- all.players[player.names, ]
  seasons$weight <- weights
  dne <- which(is.na(seasons[,1]))
  seasons.exist <- seasons[-dne,]
  season.avg  <- apply(seasons.exist[,2:21], 2, 
                       function(x){weighted.mean(x,w=seasons.exist$weight)}) 
  return(season.avg)
}


project.rb <- function(player){
  top.matches <- similar.w.stats(player,k=20)[2:20,]
  match.names <- row.names(top.matches)
  wgts  = top.matches$similarity
  matched.players <- data.frame(player = sapply(match.names, name.to.name),
                                year = sapply(match.names, name.to.year))
  year.one <- c()
  year.two <- c()
  for (i in 1:nrow(matched.players)){
    player <- matched.players[i,]$player
    yr     <- as.integer(matched.players[i,]$year)
    year.one <- append(year.one, paste(player, yr+1))
    year.two <- append(year.two, paste(player, yr+2))
  }
  
  year.one.avg <- lookup.and.average(year.one, wgts)
  year.two.avg <- lookup.and.average(year.two, wgts)
  
  
  results <- rbind(year.one.avg, year.two.avg)
  return(round(results[,2:ncol(results)],1))
}


rbind(project.rb("Le'Veon Bell 2013"),
      all.players[c("Le'Veon Bell 2013", 
                    "Le'Veon Bell 2014", 
                    "Le'Veon Bell 2015"),3:21])[,c(1,3,4,12,13,14)]

projections <- c()
actuals     <- c()
for (player in c(row.names(Rush.2013), row.names(Rush.2014))){
  name <- name.to.name(player)
  yr   <- as.integer(name.to.year(player))
  projections <- append(projections, project.rb(player)[,12])
  actuals     <- append(actuals, all.players[c(
    paste(name, yr+1),
    paste(name, yr+2)),14])
}

rushing.lm <- lm(actuals ~ projections)
plot(y=actuals, x=projections, yaxt = "n") 
axis(2, at=seq(200,2100,200))
abline(rushing.lm, col='red', lwd=2)

summary(rushing.lm$residuals)
