pageRank <- function(V, 
                     ratings=array(1, dim=nrow(V)), 
                     iterations=1, 
                     boost=T){
  
  weights <- rowSums(V)^-1
  
  if (boost==T) {
    I <- nrow(V)
    for(w in 1:length(weights)){
      if (weights[w] == Inf) {
        V[w,] <- array(1, dim=I)
      }}
    weights <- rowSums(V)^-1
  }
  
  for (i in 1:iterations){
    new_ratings <-  colSums(c(weights*ratings) * V, na.rm=T)
    #error <- sqrt(mean((ratings-new_ratings)^2))
    ratings <- new_ratings
  }
  
  return(ratings)
}

# Example from text
votes.matrix = matrix(c(0,0,0, 1,0,0, 1,1,0), 
                      ncol=3, nrow=3, byrow=T)

pageRank(V=votes.matrix, 
         ratings=array(100, dim=3), 
         iterations = 1)


# EPL Goals
EPL.goals    <- read.csv("EPL.goals.csv", row.names=1)
EPL.wins     <- read.csv("EPL.wins.csv", row.names=1)
EPL.pts    <- read.csv("EPL.pts.csv", row.names=1)
rtg.by.goals <- pageRank(EPL.goals, iterations=10)
rtg.by.wins  <- pageRank(EPL.wins, iterations=10)
rtg.by.pts  <- pageRank(EPL.pts, iterations=10)

EPL.df <- data.frame(Wins=round(65*rtg.by.wins,0), 
                     Pts=round(65*rtg.by.pts, 0),
                     Goals=round(65*rtg.by.goals,0))

EPL.df[order(-EPL.df$Pts),]
