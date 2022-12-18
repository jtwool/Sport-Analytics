setwd("/Users/jwolo/Downloads/Sport-Analytics/Sport-Analytics/05-Support/")

#install.packages("rjson")
library("rjson")
pme <- fromJSON(file="plus-minus-events.json")

event_to_matrix <- function(event){
  event.matrix <- matrix(data=0, nrow=2, ncol=99)
  e <- event[[1]]
  ifelse(e$scoring_team=="Anaheim",
         {Anaheim_value=1; Detroit_value=-1},
         {Anaheim_value=-1; Detroit_value=1})
        for (player in e$Anaheim){
          event.matrix[1, player] <- Anaheim_value;
        }
        for (player in e$Detroit){
          event.matrix[2, player] <- -Detroit_value;
        }
        return(event.matrix)
      }

events.by.player <- apply(FUN=event_to_matrix, 
                          X=as.array(pme$events), 
                          MARGIN=1)

matrix(apply(X=events.by.player, 
             MARGIN=1, 
             FUN=sum), 
       nrow=2)

