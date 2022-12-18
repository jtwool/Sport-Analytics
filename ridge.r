# setwd("/Users/jwolo/Downloads/Sport-Analytics/Sport-Analytics/05-Support/")
#install.packages("ridge")
library(ridge)

apm.matrix <- head(read.csv("adjusted.plus.minus.data.csv"),1000)
players <- glm(Outcome ~ . + 0, data=apm.matrix, family="binomial")
data.frame(players$coefficients)

players <- glm(Outcome ~ . + 0, data=apm.matrix, family="binomial")


ridge.players <- logisticRidge(Outcome ~ . + 0, data=apm.matrix)
round(data.frame(log=players$coefficients, ridg=ridge.players$coef[,1]),2)
