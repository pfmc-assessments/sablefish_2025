library(FSA)
library(FSAdata)
library(plotrix)
# https://derekogle.com/fishR/examples/oldFishRVignettes/AgeLengthKey.pdf


data(RockBassLO2)
head(RockBassLO2)

# 1. Create a data frame with aged fish
# 1. Create a data frame with the unaged fish that has an age column with NAs

rb.age <- dplyr::filter(RockBassLO2, !is.na(age)) # get fish without NAs in age variable
str(rb.age)

rb.len <- dplyr::filter(RockBassLO2,is.na(age)) # get fish with NAs in age variable
str(rb.len)

Summarize(~tl,data = rb.age, digits=1)
# startcat is the beginning of the length category
# w is the width of the length categories
rb.age1 <- lencat(~tl, data = rb.age, startcat=110, w=10)
# the binned lengths are in a column called LCat

# matrix of counts of length bin (column) and age (rows)
rb.raw <- with(rb.age1, table(LCat, age))
rb.key <- prop.table(rb.raw, margin = 1)
round(rb.key, 2) 

rb.len1 <- alkIndivAge(rb.key, age ~ tl, data = rb.len)
head(rb.len1)

rb.comb <- rbind(rb.age,rb.len1)
head(rb.comb)

rb.sum <- Summarize(tl~age,data=rb.comb,digits=2) 


hist(~age,data=rb.comb,breaks=3:11,xlim=c(2,12),xlab="Age (yrs)",main="")

plot(tl~age, data=rb.comb, ylab="Total Length (mm)",xlab="Age (jittered)")
lines(mean~age,data=rb.sum,col="blue",lwd=2)

histStack(tl~age,data=rb.comb,breaks=seq(110,280,10),xlab="Total Length (mm)")


