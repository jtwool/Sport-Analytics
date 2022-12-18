setwd("/Users/jwolo/Desktop/")
epl <- read.csv("EPL_2016.csv", header = F)

EPL <- matrix(0, nrow=20, ncol=20)

parse.score <- function(x){
  if(x == "-"){
    return(F)
  }
  s = strsplit(x,"-")
  return(as.numeric(s[[1]]))
}

for (i in 1:nrow(epl)) {
  for (j in 1:ncol(epl)) {
    score = parse.score(epl[i,j])
    print(score)
    if (typeof(score) != typeof(F)){
      
      if(score[1] > score[2]){
        EPL[j, i] <- EPL[j, i] + 3
      }
      if(score[2] > score[1]){
        EPL[i, j] <- EPL[i, j] + 3
      }
      if(score[2] == score[1]){
        EPL[i, j] <- EPL[i, j] + 1
        EPL[j, i] <- EPL[j, i] + 1
      }
    }
  }
}

EPL

write.csv(EPL,file = "EPL.3wins.csv",row.names = F,col.names = F)
