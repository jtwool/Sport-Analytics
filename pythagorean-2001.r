library("ggplot2")

pythagorean.wins <- function(r, ra, k=2, n=1){
  r^k / (r^k + ra^k) * n
}

MLB.2001 <- read.csv("2001-MLB.csv")

MLB.2001$py.rat <-pythagorean.wins(MLB.2001$R, MLB.2001$RA)
MLB.2001$py.exp <- pythagorean.wins(MLB.2001$R, MLB.2001$RA, n=162)

MLB.2001$luck <- MLB.2001$W - MLB.2001$py.exp

head(MLB.2001[order(MLB.2001$luck),], n=5)
head(MLB.2001[order(-MLB.2001$luck),], n=5)

ggplot(MLB.2001, aes(x=py.exp, y=W, label=Tm)) + 
  geom_smooth(method="lm", level=.95, alpha=.25) +
  geom_point(aes(color=Lg),size=2) +
  geom_text(alpha=1, nudge_y = 2.5) +
  xlab("Pythagorean Expected Wins") +
  ylab("Observed 2001 Wins") +
  theme_bw()