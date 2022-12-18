setwd("C:/Users/jwolo/OneDrive/Sport-Analytics/04-Support/")
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

make.wOBA <- function(h,b2,b3,hr,pa,bb,hbp){
  b1 <- make.singles(h,b2,b3,hr)
  (.72*bb+.75*hbp+.9*b1+1.24*b2+1.56*b3+1.95*hr)/pa
}

al.1992$OBP <- apply(al.1992[,3:13],   # 1992 AL dataset - numbers only
                     1,                # row-wise application
                     function(x){make.OBP(x[4],x[8],x[10],x[2],x[11])}
)

al.1992$SLG <- apply(al.1992[,3:13],   
                     1,                
                     function(x){make.SLG(x[4],x[5],x[6],x[7],x[2])}
)

al.1992$OPS <- with(al.1992, OBP+SLG)

al.1992$wOBA <- apply(al.1992[,3:13],  
                     1,               
                     function(x){make.wOBA(x[4],x[5],x[6],x[7],x[1],x[8],x[10])}
)


top.wOBA <- al.1992[order(-al.1992$wOBA),][1:10,c(1,14,15,16,17)]

cor.mat <- cor(al.1992[,c(11,14:17)])

par(mfrow=c(2,2))
hist(al.1992$OBP, col="gray10")
hist(al.1992$OPS, col="gray30")
hist(al.1992$SLG, col="gray50")
hist(al.1992$wOBA, col="gray70")
