setwd("/Users/jwolo/Downloads/Sport-Analytics/Sport-Analytics/04-Support/")
al.1992 <- read.csv("AL.1992.batting.csv")

make.OBP <- function(h,bb,hbp,ab,sf){
  (h+bb+hbp) / (ab+bb+hbp+sf)
}

make.singles <- function(h,b2,b3,hr){
  h-b2-b3-hr
}

make.SLG <- function(h,b2,b3,hr,ab){
  b1 <- make.singles(h,b2,b3,hr)
  (b1+2*b2+3*b3+4*hr)/ab
}

make.linearRC <- function(h,b2,b3,hr,bb,hbp){
  b1 <- make.singles(h,b2,b3,hr)
  (.72*bb+.75*hbp+.9*b1+1.24*b2+1.56*b3+1.95*hr)
}

make.simpleRC <- function(OBP, SLG, AB){
  OBP*SLG*AB
}

make.outs <- function(PA, H, BB, HBP){
  PA-H-BB-HBP
}

al.1992$OBP <- apply(al.1992[,3:13],   
                     1,         
                     function(x){make.OBP(x[4],x[8],x[10],x[2],x[11])}
)

al.1992$SLG <- apply(al.1992[,3:13],   
                     1,                
                     function(x){make.SLG(x[4],x[5],x[6],x[7],x[2])}
)

al.1992$RC.L <- apply(al.1992[,3:13],
                     1,              
                     function(x){make.linearRC(x[4],x[5],x[6],x[7],x[8],x[10])}
)

al.1992$RC.S <- apply(al.1992[,3:15],   
                     1,                
                     function(x){make.simpleRC(x[12],x[13],x[1])}
)

al.1992$outs <- apply(al.1992[,3:15],   
                      1,                
                      function(x){make.outs(x[1],x[4],x[8],x[10])}
)

al.1992$OPS <- with(al.1992, OBP+SLG)


#
# AL TOTALS 1992
# make.linearRC(h=20006,b2=3596,b3=386,hr=1776,bb=7704,hbp=585)
# [1] 27333.23
# make.simpleRC(OBP=.328,SLG=.385,AB=77147)
# [1] 9742.123
# make.outs(PA=86887,H=20006,BB=7704,HBP=585)
# [1] 58592
#
al.1992$RpO.L <- with(al.1992, RC.L/outs)
al.1992$RpO.S <- with(al.1992, RC.S/outs)
al.1992$VORP.L <- with(al.1992, (RpO.L-(27333/58592)*.8)*outs)
al.1992$VORP.S <- with(al.1992, (RpO.S-(9742/58592)*.8)*outs)


par(mfrow=c(1,3))
plot(al.1992$VORP.L, al.1992$VORP.S)
plot(al.1992$OPS, al.1992$VORP.S)
line(abline(lm(VORP.S~OPS, data=al.1992)))
hist(al.1992$VORP.S)

al.1992[order(-al.1992$VORP.S),c(1,2,21:22)]

#
# Pitchers
#

pitchers <- read.csv("AL.1992.pitching.csv")

make.RA <- function(R, IP){
  9*(R/IP)
}

make.VORP.p <- function(IP, RA, Repl){
  IP * (Repl-RA)/9
}

AL.Replacement <- make.RA(R=9802, IP=20329) * 1.35 - 0.66


pitchers$RA <- apply(pitchers[,3:14],
                     MARGIN=1,
                     FUN=function(x){make.RA(x[8],x[6])})


pitchers$VORP <- apply(pitchers[,3:15],
                       MARGIN=1,
                       FUN=function(x){make.VORP.p(x[6],x[13],AL.Replacement)})

head(pitchers[order(-pitchers$VORP),c(1:5,8,16)],15)
head(pitchers[order(pitchers$VORP),],15)

hist(pitchers$VORP,xlab = "VORP", ylab = "Count", main="Histogram of VORP")
plot(pitchers$ERA, pitchers$VORP, xlab="ERA", ylab="VORP", main="ERA + VORP")
plot(pitchers$W, pitchers$VORP, xlab="Wins", ylab="VORP", main="Wins + VORP")

#
# Simple WAR
#

al.1992$WAR <- al.1992$VORP.S/10
pitchers$WAR <- pitchers$VORP/10
head(al.1992[order(-al.1992$VORP.S),c(1,2,24)],10)
head(pitchers[order(-pitchers$VORP),c(1:2,17)],10)

