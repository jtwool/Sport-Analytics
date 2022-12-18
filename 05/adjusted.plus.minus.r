
generate.event <- function(){
  
  team.1 <- sample(1:10, 5)
  team.2 <- sample(1:10, 5)
  rtg <- sum(players.1[team.1])/(sum(players.1[team.1])+sum(players.2[team.2]))
  u <- runif(1)
  if ( u > rtg+.4 ){ outcome <- 1 }
  else if(u < rtg-.4 ){ outcome<- -1} else {outcome <- 0} 
  print(c(u,rtg,outcome))
  mat <- matrix(0, nrow=2, ncol=21)
  
  mat[2,21] <- -1    ;    mat[1,21] <- -1
  mat[1,1+team.1] <- 1  ;   mat[1,10+team.2] <- -1;
  mat[2,1+team.1] <- -1 ;   mat[2,10+team.2] <- 1;
  
  if (outcome != 0) {
    if (outcome == 1) {mat[1,1] <- 1} 
    else {mat[2,1] <- 1}
  }
  return(mat)
}

players.1 <- rnorm(10, sd=2)
players.2 <- rnorm(10, sd=2)
APM.mat <- c()
for (e in 1:1000){
  APM.mat <- rbind(APM.mat, generate.event())
}

apm.mat <- data.frame(APM.mat)
colnames(apm.mat) <- c("Outcome",
                       "Katie A.",
                       "Sammie B.",
                       "Annie C.",
                       "Brianna D.",
                       "Tiffany E.",
                       "Yasmin G.",
                       "Michelle H.",
                       "Daisy I.",
                       "Eva J.",
                       "Rachel K.",
                       "Nancy L.",
                       "Hazel M.",
                       "Melissa N.",
                       "Isobel O.",
                       "Eliza P.",
                       "Alish Q.",
                       "Maryam R.",
                       "Isla S.",
                       "Elizabeth T.",
                       "Sofia U."
)

write.csv(x = apm.mat, file = "adjusted.plus.minus.data.csv", row.names = F)
players <- glm(Outcome ~ . + 0, data=apm.mat, family="binomial")
data.frame(players$coefficients)

library(MASS)
hockey.apm <- read.csv(file = "adjusted.plus.minus.data.csv")
head(hockey.apm)
players <- MASS::lm.ridge(Outcome ~ . + 0, data=hockey.apm)
data.frame(players$coef)
