setwd("C:/Users/jwolo/OneDrive/Sport-Analytics/03-Support/")
library(ggplot2)
pythagorean.wins <- function(r, ra, k=2, n=1){r^k / (r^k + ra^k) * n}

# ############### #
# NFL 2001 Season #
# ############### #

NFL.2001 <- read.csv("2001-NFL.csv")

NFL.2001$py.rat <- apply(as.matrix(NFL.2001[,c(5,6)]), 1, function(x){pythagorean.wins(x[1],x[2],k=2.37)})
NFL.2001$py.exp <- apply(as.matrix(NFL.2001[,c(5,6)]), 1, function(x){pythagorean.wins(x[1],x[2], n=16, k=2.37)})

NFL.2001$luck <- NFL.2001$W - NFL.2001$py.exp

head(NFL.2001[order(NFL.2001$luck),],5)
head(NFL.2001[order(-NFL.2001$luck),],5)

# ############### #
# NBA 2001 Season #
# ############### #

NBA.2001 <- read.csv("NBA-2001.csv")

NBA.2001$py.rat <- apply(as.matrix(NBA.2001[,c(4,5)]), 1, function(x){pythagorean.wins(x[1],x[2],k=13.91)})
NBA.2001$py.exp <- apply(as.matrix(NBA.2001[,c(4,5)]), 1, function(x){pythagorean.wins(x[1],x[2], n=82, k=13.91)})

NBA.2001$luck <- NBA.2001$W - NBA.2001$py.exp

head(NBA.2001[order(NBA.2001$luck),],5)
head(NBA.2001[order(-NBA.2001$luck),],5)

# ############### #
# NHL 2001 Season #
# ############### #

NHL.2001 <- read.csv("NHL-2001.csv")

NHL.2001$py.rat <- apply(as.matrix(NHL.2001[,c(7,8)]), 1, function(x){pythagorean.wins(x[1],x[2], k=1.93)})
NHL.2001$py.exp <- apply(as.matrix(NHL.2001[,c(7,8)]), 1, function(x){pythagorean.wins(x[1],x[2], n=82, k=1.93)})

NHL.2001$luck <- NHL.2001$W - NHL.2001$py.exp

head(NHL.2001[order(NHL.2001$luck),],5)
head(NHL.2001[order(-NHL.2001$luck),],5)

# ############### #
# EPL 2001 Season #
# ############### #

EPL.2001 <- read.csv("EPL-2001.csv")
EPL.2001

EPL.2001$py.rat <- apply(as.matrix(EPL.2001[,c(5,6)]), 1, function(x){pythagorean.wins(x[1],x[2], k=1.7)})
EPL.2001$py.exp <- apply(as.matrix(EPL.2001[,c(5,6)]), 1, function(x){pythagorean.wins(x[1],x[2], n=38, k=1.7)})

EPL.2001$luck <- EPL.2001$W - EPL.2001$py.exp

a <- head(EPL.2001[order(EPL.2001$luck),],5)
b <- head(EPL.2001[order(-EPL.2001$luck),],5)
x <- rbind(b,a)


# #################### #
# Weibull distribution #
# #################### #
par(mfrow=c(1,1))

weibull.pdf <- function(x,e,b,y){
  if(x<b){return(0)}
  else{ (y/e)*((x-b)/e)^(y-1)*exp(1)^(-1*((x-b)/e)^y) }}

rf <- sapply(1:20, function(x){weibull.pdf(x, 5.3, .5, 1.83)})
ra <- sapply(1:20, function(x){weibull.pdf(x, 4.6, .5, 1.83)})

df <- data.frame(runs=rep(0:20,2), prob=c(rf,ra), Side=rep(c("for","against"), each=21))
ggplot(df, aes(x=runs, y=prob)) +
  geom_line(aes(color=Side), size=2) +
  theme_bw() + 
  ylab("P(Runs)") +
  xlab("Runs")


# Monte Carlo solution
for.cdf <- cumsum(rf)
ag.cdf <- cumsum(ra)

rand.var <- function(x,ds){
  u <- runif(1)
  for (i in 1:length(ds)){ if(u < ds[i]){return(i-1)}}}

for.fn <- function(x){rand.var(x,ds=for.cdf)}
ag.fn <- function(x){rand.var(x,ds=ag.cdf)}

set.seed(2001)
N <- 100000
runs.for= sapply(array(0,N), for.fn)
runs.ag = sapply(array(0,N), ag.fn)
winner = table(sign(runs.for - runs.ag))


(winner[3]+.5*winner[2])/(winner[2]+winner[1]+winner[3])
pythagorean.wins(5.3,4.6,k=1.83)

# Simulated seasons

set.seed(2001)
seasons <- 10
games <- 162
wins <- array(0, seasons)
for (season in 1:seasons){
  runs.for= sapply(array(0,games), for.fn)
  runs.ag = sapply(array(0,games), ag.fn)
  winner = table(sign(runs.for-runs.ag))
  wins[season] <- floor(winner[2]*.5+winner[3])
}
table(wins)
