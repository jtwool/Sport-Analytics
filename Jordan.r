library(ggplot2)

# Distributions

js <- sapply(1:20, function(x){dbinom(x,20,.83)})
bs <- sapply(1:20, function(x){dbinom(x,20,.88)})
df <- data.frame(makes=1:20, jordan=round(js,3), bird=round(bs,3))
p <- ggplot(df, aes(x=makes)) +
      geom_line(aes(y=bird), color="green", size=2) + 
      geom_line(aes(y=jordan), color="red", size=2) +
      theme_bw() + 
      ylab("P(Makes)") +
      xlab("Makes")

# Monte Carlo solution
jordan <- .83
bird   <- .88
N      <- 10000

j_games <- rbinom(N,20,jordan)
b_games <- rbinom(N,20,bird)
j_games - b_games
table(sign(j_games-b_games))/N
