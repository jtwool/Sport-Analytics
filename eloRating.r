
elo.prb   <- function(A, B){
  rd = (B-A)/400
  1 / (1 + 10^rd)
}

elo.match <- function(winner, loser, k=15){
    w.pct <- elo.prb(winner, loser)
    chng  <- k*(1-w.pct)
    winner <- winner + chng
    loser  <- loser  - chng
    return(c(winner, loser))
}

# Data prep

ATP <- read.csv("ATP.matches.csv")

ATP.players <- factor(c(ATP$winner,ATP$loser))

ATP$winner <- factor(ATP$winner, levels=levels(ATP.players))
ATP$loser  <- factor(ATP$loser,  levels=levels(ATP.players))

ratings <- data.frame(match.elo = array(1500,dim=length(levels(ATP.players))),
                      set.elo   = array(1500,dim=length(levels(ATP.players))),
                      game.elo  = array(1500,dim=length(levels(ATP.players))),
                      row.names=levels(ATP.players))

# Calculate ratings

for (match.num in 1:nrow(ATP)){
  winner <- ATP[match.num,5]
  loser  <- ATP[match.num,6]
  
  #Matches
  winner.m.rtg <- ratings[winner,1] 
  loser.m.rtg <- ratings[loser,1]
  
  results <- elo.match(winner.m.rtg, loser.m.rtg, k=20)
  
  ratings$match.elo[as.integer(winner)] <- results[1]
  ratings$match.elo[as.integer(loser)] <- results[2]
  
  #Sets
  for(i in 1:ATP[match.num, 17]){
    winner.s.rtg <- ratings[winner,2] 
    loser.s.rtg <- ratings[loser,2]
    results <- elo.match(winner.s.rtg, loser.s.rtg, k=15)
    ratings$set.elo[as.integer(winner)] <- results[1]
    ratings$set.elo[as.integer(loser)] <- results[2]
  }
  if(ATP[match.num, 18] >0){
    for(i in 1:ATP[match.num, 18]){
        winner.s.rtg <- ratings[winner,2] 
        loser.s.rtg <- ratings[loser,2]
        results <- elo.match(loser.s.rtg, winner.s.rtg, k=15)
        ratings$set.elo[as.integer(loser)] <- results[1]
        ratings$set.elo[as.integer(winner)] <- results[2]
      }
  }
  
  #Games
  for(i in 1:ATP[match.num, 19]){
    winner.g.rtg <- ratings[winner,3] 
    loser.g.rtg <- ratings[loser,3]
    results <- elo.match(winner.g.rtg, loser.g.rtg, k=10)
    ratings$game.elo[as.integer(winner)] <- results[1]
    ratings$game.elo[as.integer(loser)] <- results[2]
  }
  if(ATP[match.num, 20] > 0){
    for(i in 1:ATP[match.num, 20]){
        winner.g.rtg <- ratings[winner,3] 
        loser.g.rtg <- ratings[loser,3]
        results <- elo.match(loser.g.rtg, winner.g.rtg, k=10)
        ratings$game.elo[as.integer(loser)] <- results[1]
        ratings$game.elo[as.integer(winner)] <- results[2]
    }
  }
  
}


head(ratings[order(-ratings$match.elo), ], 15)
# 