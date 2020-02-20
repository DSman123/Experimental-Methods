# Joel Cabrera
# Experimental Methods
# Professor McCabe
# January 28, 2020

# R Exercises from QSS
# Exercise 1.3

# 1.3.1
5 + 3
5 - 3
5 / 3
5^3
5*(10-3)
sqrt(4)
# 1.3.2
result <- 5+3
result
print(result)
result <- 5-3
result
Result
kosuke <- "instructor"
kosuke
kosuke <- "instructor and author"
kosuke
Result <- "5"
Result / 3
sqrt(Result)
result
class(result)
Result
class(Result)
class(sqrt)
# 1.3.3
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)
world.pop
pop.first <- c(2525779, 3026003, 3691173) 
pop.second <- c(4449049, 5320817, 6127700, 6916183) 
pop.all <- c(pop.first, pop.second)
pop.all
world.pop[2]
world.pop[c(2, 4)]
world.pop[c(4, 2)]
world.pop[-3] ## (-) drops 3rd element
pop.million <- world.pop / 1000 # divides each element by 1000
pop.million
pop.rate <- world.pop / world.pop[1] 
pop.rate
pop.increase <- world.pop[-1] - world.pop[-7] 
percent.increase <- (pop.increase / world.pop[-7]) * 100
percent.increase
percent.increase[c(1, 2)] <- c(20, 22) # replaces indices
percent.increase
#1.3.4
length(world.pop)
min(world.pop)
max(world.pop)
range(world.pop)
mean(world.pop)
sum(world.pop) # total number
sum(world.pop) / length(world.pop)
year <- seq(from = 1950, to = 2010, by = 10) 
year
seq(to = 2010, by = 10, from = 1950)
seq(from = 2010, to = 1950, by = -10)
2008:2012
2012:2008
names(world.pop)
names(world.pop) <- year 
names(world.pop)
world.pop
myfunction <- function(input1, input2, ..., inputN) { ## Example of defining function
  DEFINE "output" USING INPUTS
  return(output)
}
my.summary <- function(x){
  s.out <- sum(x)
  l.out <- length(x) 
  m.out <- s.out / l.out 
  out <- c(s.out, l.out, m.out)
  names(out) <- c("sum", "length", "mean") 
  return(out)
} 
z <- 1:10 
my.summary(z)
my.summary(world.pop)
# 1.3.5
getwd()
# setwd() can also use this to set working directory
# data=read.csv(file.choose(), header=TRUE) CAN ALSO use this to obtain data
UNpop <- read.csv("UNpop.csv") 
class(UNpop)
load("UNpop.RData") # if the same dataset is saved as an object in an RData ???le
names(UNpop)
nrow(UNpop)
ncol(UNpop)
dim(UNpop)
summary(UNpop)
UNpop$world.pop # access world.pop variable
UNpop[, "world.pop"] # e x t r a c t t h e c o l u m n c a l l e d " w o r l d . p o p "
UNpop[c(1, 2, 3),] # e x t r a c t t h e f i r s t t h r e e r o w s ( a n d a l l c o l u m n s 
UNpop[1:3, "year"] # e x t r a c t t h e f i r s t t h r e e r o w s o f t h e " y e a r " c o l u m n
UNpop$world.pop[seq(from = 1, to = nrow(UNpop), by = 2)]
world.pop <- c(UNpop$world.pop, NA) 
world.pop
mean(world.pop)
mean(world.pop, na.rm = TRUE)
# 1.3.6
save.image("qss/INTRO/Chapter1.RData") # saves workspace via command; file location needed
save(UNpop, file = "Chapter1.RData") 
save(world.pop, year, file = "qss/INTRO/Chapter1.RData")
write.csv(UNpop, file = "UNpop.csv")
load("Chapter1.RData")
# 1.3.7
install.packages("foreign")
library("foreign") # package used to import files from SPSS and Stata
read.dta("UNpop.dta")
read.spss("UNpop.sav")
write.dta(UNpop, file = "UNpop.dta") # will save as Stata file
# 1.3.8
source("UNpop.R")
library(foreign) 
UNpop <- read.csv("UNpop.csv") 
UNpop$world.pop <- UNpop$world.pop / 1000
write.dta(UNpop, file = "UNpop.dta")
install.packages("lintr")
library(lintr)# checks for incorrect syntax
lint("UNpop.R")
install.packages("rmarkdown")
library(rmarkdown) # use R as writing documents like in Microsoft Word

# Exercise 1.5.1

# 1.
data=read.csv(file.choose(), header=TRUE)
dim(data)
summary(data)
names(data)
r = nrow(data)
c = ncol(data)
obs = r*c
obs # number of observations
datay = data$year
datay
names(datay) # returns NULL; better to examine csv
range(datay)

# 2.
data$overtotal = data$overseas + data$total # adding overseas voters to total voters
data$VAPvt = data$overtotal/data$VAP *100 # VAP = voting age population
data$VAPvt
data$VEPvt = data$overtotal/data$VEP *100 # Q: Assuming still including overseas voters?
data$VEPvt

# 3.
# data$VAPANES <- data$VAPvt - data$ANES results in (-) turnout rates... don't think this is correct
data$VAPANES1 = data$ANES - data$VAPvt # yields (+) turnout rates
data$VAPANES1
round(mean(data$VAPANES1))
round(range(data$VAPANES1))

data$VAPANES2 = data$ANES - data$VEPvt # yields (+) turnout rates
data$VAPANES2
round(mean(data$VAPANES2))
round(range(data$VAPANES2))

# 4.
data$year[c(1, 3, 5, 7, 9, 11, 13, 14)] # reports presidential election years except 2002
round(data$VEPvt[c(1, 3, 5, 7, 9, 11, 13, 14)]) # rounded for simplicity's sake
data$ANES[c(1, 3, 5, 7, 9, 11, 13, 14)]

round(data$VEPvt) # reports midterm elections, but 2002 is not included
data$ANES

# 5.
turnout1 = data[c(1, 2, 3, 4, 5, 6, 7),]
turnout2 = data[c(8, 9, 10, 11, 12, 13, 14),]
turnout1[, "VEP"]
turnout1[, "total"]
turnout1[, "ANES"]
turnout1[, "total"]/turnout1[, "VEP"]
turnout1VEPvt = (turnout1[, "total"]/turnout1[, "VEP"])-(turnout1[, "ANES"])
turnout1VEPvt

turnout2 = data[c(8, 9, 10, 11, 12, 13, 14),]
turnout2[, "VEP"]
turnout2[, "total"]
turnout2[, "ANES"]
turnout2[, "total"]/turnout2[, "VEP"]
turnout2VEPvt = (turnout2[, "total"]/turnout2[, "VEP"])-(turnout2[, "ANES"])
turnout2VEPvt

# Comparing
turnout1VEPvt
turnout2VEPvt

# 6.
data$felons # could also do: (data[, "felons"])
data$noncit
data$feloncit = data$felons + data$noncit
data$adjVAP = data$VAP - data$feloncit
data$newtotal = data$total - data$overseas
data$adjVAPvt = data$newtotal/data$adjVAP *100 # total does not consider overseas voters
data$adjVAPvt
# Comparing
data$adjVAPvt
data$VAPvt
data$VEPvt
data$ANES
