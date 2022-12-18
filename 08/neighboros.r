setwd("C:/Users/jwolo/Downloads/Sport-Analytics/Sport-Analytics/08-Support/")

Batting.2011 <- read.csv(file="./MLB-batting-top-700-2011.csv", row.names=1) 
mcols <- c(1,3:17)

neighbors.matrix <- function(x){
  df <- apply(x, MARGIN=2, FUN=function(x){(x-mean(x)) / sd(x)})
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

neighbors.2011 <- neighbors.matrix(as.matrix(Batting.2011[,mcols]))
row.names(neighbors.2011) <- row.names(Batting.2011)
colnames(neighbors.2011) <- row.names(Batting.2011)

similar.w.stats <- function(player,K=10){
  similar.players <- sort(neighbors.2011[player,],decreasing = T)[2:(K+1)]
  similar.player.names <- row.names(data.frame(similar.players))
  df <- Batting.2011[player,]
  for (name in similar.player.names){df <- rbind(df, Batting.2011[name, ])}
  return(df)
}

similar.w.stats("Dustin Pedroia",K=5)
similar.w.stats("Joey Votto",K=5)

