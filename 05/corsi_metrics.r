#install.packages("rjson")
library("rjson")

corsi.events <- readLines("corsi.events.jsonl")

corsi.addition <- function(event, offensive=T){
  event.data <- fromJSON(event)
  if (offensive) {t = "Syracuse"} else {t = "Albany"}; 
  corsi.array <- matrix(array(20, data=0),nrow=2)
  if (event.data$event_team == t)
        {for (player in event.data$on.ice.syr){
          corsi.array[1, player] <- 1;}
  } else {
        for (player in event.data$on.ice.alb){
          corsi.array[2, player] <- 1;
        }
  }
  return(corsi.array)}

offensive.events <- lapply(FUN=corsi.addition, X=corsi.events)
defensive.events <- lapply(FUN=corsi.addition, X=corsi.events, offensive=F)


offensive.corsi <- Reduce(x = offensive.events, f= `+`)
defensive.corsi <- Reduce(x = defensive.events, f= `+`)

round(offensive.corsi/(offensive.corsi+defensive.corsi),2)
