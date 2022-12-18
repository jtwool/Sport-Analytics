make.Marcel <- function(yr1, yr2, yr3){
  wtd.pa  <- (mean(yr1$PA)*5+mean(yr2$PA)*4+mean(yr3$PA*3))/12
  lg.avgs <- (colMeans(yr1)*5+colMeans(yr2)*4+colMeans(yr3)*3)/12
  wtd.lg.avg <- lg.avgs * (1200/wtd.pa)
  
  project.Marcel <- function(P){
    player.totals <- (P[1,]*5+P[2,]*4+P[3,]*3)/4
    wtd.totals    <- player.totals + wtd.lg.avg
    wtd.rates     <- wtd.totals/wtd.totals$PA
    age.adj       <- 1+(29-P$Age[1])*0.006
    exp.PA        <- (P$PA[1]*.5+P$PA[2]*.1+200)*age.adj
    return(wtd.rates*exp.PA*age.adj)
  }
  return(project.Marcel)
}

#setwd("C:/Users/jwolo/Downloads/Sport-Analytics/Sport-Analytics/08-Support/")

Batting.2011 <- read.csv(file="./MLB-batting-top-700-2011.csv", row.names=1) 
Batting.2010 <- read.csv(file="./MLB-batting-top-700-2010.csv", row.names=1) 
Batting.2009 <- read.csv(file="./MLB-batting-top-700-2009.csv", row.names=1) 
Batting.2008 <- read.csv(file="./MLB-batting-top-700-2008.csv", row.names=1) 
mcols <- c(1,3:17)

fn.Marcel <- make.Marcel(Batting.2010[,mcols], 
                         Batting.2009[,mcols], 
                         Batting.2008[,mcols])

Marcel.predict <- function(player.name){
  player <- rbind(Batting.2010[player.name,][,mcols], 
                  Batting.2009[player.name,][,mcols],
                  Batting.2008[player.name,][,mcols])
  
  projection <- round(fn.Marcel(player),0)
  actual     <- Batting.2011[player.name,][,mcols]
  df <- rbind(projection,actual, actual-projection)
  rownames(df) <- c("Projected", "Actual", "Difference")
  return(df)
}

Marcel.predict("Albert Pujols")
Marcel.predict("Derek Jeter")

