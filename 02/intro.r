# Total bases
total.bases <- 0
hits <- 2
walks <- 1
total.bases <- hits + walks
total.bases

# Receiving yards
receiving.yards <- 44
catches <- 3
rush.yards <- 70
rush.attempts <- 15

tot.yards <- receiving.yards + rush.yards
touches <- catches + rush.attempts
yards.per.touch <- tot.yards / touches
yards.per.touch

# Starting five
twos <- c(3,2,14,8, 4)
threes <- c(1,3,0,3,1)
fts <- c(1,2,4,3,1)
totals <- twos*2+threes*3+fts*1
totals
sum(totals)