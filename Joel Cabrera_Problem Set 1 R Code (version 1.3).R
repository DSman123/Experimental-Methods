#############################################
## Problem Set 01 ###########################
## Name: Joel Cabrera  #############
## People you worked with:    N/A    ###########
#############################################

# Loading directory and libraries
# library(installr)
# updateR()
setwd("~/Rutgers University - Undergraduate/Spring 2020 Classes/Advanced Topics in Multivariate Statistics (Topic??? Experimental Methods) (16???790???676)/Problem Set (PS) Assignments/PS #1")
library(foreign)
library(ri)
library(ggplot2)

###########################################
# Problem 2a
###########################################
# (See 2a in Word document for answer)

###########################################
# Problem 2b
###########################################
data = read.dta("lgbt2.dta")
summary(data)
dim(data) # 2516 subjects, 12 variables
dp1 = data[data$privatetreatment == 1, ] #rows & columns, data frame
dp2 = data[data$privatetreatment == 0, ]
#dpuf = data$privatetreatmen[data$privatetreatment == 1] # column only, no data frame
#dpuf
#length(dpuf)
dp2 = data[data$privatetreatment == 0, ]
dp1
dp2
nrow(dp1) # tells number of subjects in veiled condition
nrow(dp2) # tells number of subjects in direct condition

###########################################
# Problem 2c
###########################################
# Calculating proportion (or mean)
propa = mean(data$s5_direct == "0", na.rm = TRUE) # 0 = no, 1 = yes
prop = propa * 100
prop  # proportion of subjects who said "no" for direct question

###########################################
# Problem 2d
###########################################
# checking variables
table(data$s_private_sum_5) # CV5i
table(data$s5_direct) # d5i
table(data$in5) # CD5i
# creating new variable
# If in veiled condition, then take on values of CV5i in the veiled condition
data$combined[data$privatetreatment == 1] <- data$s_private_sum_5[data$privatetreatment == 1]
# if in direct condition, then take on values of both d5i & CD5i in the direct condition
data$combined[data$privatetreatment == 0] <- data$s5_direct[data$privatetreatment == 0] + data$in5[data$privatetreatment == 0]
table(data$combined) # range 0-5

###########################################
# Problem 2e
###########################################
#1. calculating average observed outcome for each condition
co1 = mean(data$combined[data$privatetreatment == 1], na.rm = TRUE) # mean of new variable when subject was in Veiled condition
co2 = mean(data$combined[data$privatetreatment == 0], na.rm = TRUE) # mean of new variable when subject was in Direct condition
co1 # mean of new variable when subject was in Veiled condition (x)
co2 # mean of new variable when subject was in Direct condition (y)

#2. conducting t-test
# Note: t.test(.) does differencing for you
combined.t <- t.test(data$combined[data$privatetreatment == 1], data$combined[data$privatetreatment == 0])
combined.t # t = -3.13, p-value = 0.001768
t = -3.13

#3. calculating difference in means & standard error
diff.combined = co1-co2
diff.combined # difference in means
se.combined = diff.combined/t
se.combined # standard error

#4. right-tailed alternative t-test
diff.combined.alt = t.test(data$combined[data$privatetreatment == 1], data$combined[data$privatetreatment == 0], alternative = "greater")
diff.combined.alt # right t-test

###########################################
# Problem 2f
###########################################
# 1st estimate
propa = mean(data$s5_direct == "0", na.rm = TRUE) # 0 = no, 1 = yes
prop = propa * 100
prop  # proportion of subjects who said "no" for direct question (Item/Question 5: Would you be happy to have an openly LGBt manager?)
# 2nd estimate
100*abs(diff.combined) # mean of new variable when subject was in Veiled condition (x) &  mean of new variable when subject was in Direct condition (y)
# combining estimates
truepop = prop + 100*abs(diff.combined) # true proportion of people who would not be happy with an LGBt manager
truepop # about 27 in table 2 of paper
# abs value = must do due to "prop"

###########################################
# Problem 2g
###########################################
# 2-tailed test
#1. calculating average observed outcome for each condition
# (see output from "t" codes below)                                   
#2. conducting t-test
# t-test for southern respondents
south = data[data$south == 1, ] # extracting southern respondents
south.t = t.test(south$combined[south$privatetreatment == 1],          
                 south$combined[south$privatetreatment == 0])
south.t
t1 = -1.4062    ## t-stat
# t-test for non-southern respondents
nonsouth = data[data$south == 0, ] # extracting non-southern respondents                                     
nonsouth.t = t.test(nonsouth$combined[nonsouth$privatetreatment == 1],
                    nonsouth$combined[nonsouth$privatetreatment == 0])
nonsouth.t
t2 = -2.8876    ## t-stat
#3. calculating difference in means & standard error
# difference in means & standard error among southern respondents
# 1st way  
## diff.south <- mean(data$combined[data$privatement == 1 &        
##                        data$south == 1]) - 
##                        mean(data$combined[data$privatetreatment == 0 & 
##                       data$south == 1])
##diff.south
##se.south = diff.south/t1 # standard error
##se.south
# 2nd way
diff.south = south.t$estimate[1]- south.t$estimate[2] # look at south.t in global environment
diff.south

# difference in means & standard error among non-southern respondents
# 1st way
## diff.nonsouth <- mean(data$combined[data$privatement == 1 & 
##                        data$south == 0]) - 
##                        mean(data$combined[data$privatetreatment == 0 & 
##                       data$south == 0])
##se.nonsouth = diff.nonsouth/t2 # standard error
##se.nonsout
# 2nd way
diff.nonsouth = nonsouth.t$estimate[1]- nonsouth.t$estimate[2] # look at nonsouth.t in global environment
diff.nonsouth

# Plot for change in reporting for full sample, southerners & non-southerners
# test of proportions
# prop test for southerners

# plotting
par(mar = c(4, 6, 4, 2)) 
plot(c(1, 2, 3), c(diff.combined, diff.south, diff.nonsouth), 
     xlim = c(.5, 3.5), 
     ylim = c(-0.5, 0.5), 
     main = "Treatment Effects", 
     xlab = "Proportional effect", 
     ylab = "Change (%) in Reporting", 
     cex.lab = .8, cex.main = .8, 
     pch = c(15, 1), 
     xaxt="n", 
     yaxt="n") 
abline(h=seq(-.4, .4, .2), col = "gray", lty = 20) 
abline(h=0) 
axis(1, c(1,2,3), c("All Respondents", "Southern \n Respondents", "Non-Southern \n Respondents"), cex.axis = .8) 
axis(2, seq(-.4, .4, .2), seq(-40, 40, 20), las = 3) 
lines(c(1,1), combined.t$conf.int, lwd=2) # should come from = t-test of data$combined
lines(c(2,2), south.t$conf.int, lwd=2) # displays confidence intervals for south and non-south respondents
lines(c(3,3), nonsouth.t$conf.int, lwd=2)

###########################################
# Problem 2h
###########################################
herreg <- lm(combined ~ south + privatetreatment + south*privatetreatment, data = data)
herreg
summary(herreg) # gives all descriptive and inferential statistics

###########################################
# Problem 2i
###########################################
# (See 2i in Word document for answer)
