# Joel Cabrera
# Experimental Methods
# Professor McCabe
# May 11, 2020

### 1. Subsetting and loading data preliminaries
library(ggplot2)
data = read.csv("Experimental+Omnibus+2020_May+3,+2020_07.19 (updated).csv")
data = subset(data, consent == "I Agree")

### 2. Checking variables
# Dependent (outcome) variable
table(data$jc_q8outcome_1) # question 8, "TrumpFeelings" variable (continuous)
# Independent (treatment) variable
table(data$jctreat) # vignette given before shown question 8, "vignette" variable (binary)
# Covariates (CVs)
table(data$jc_q1) # question 1, "vote" variable (binary)
table(data$jc_q2) # question 2 "satisfication" variable (ordinal)
table(data$jc_q3) # question 3 "control" variable (ordinal)
table(data$jc_q4) # question 4 "diplomacy" variable (binary)
table(data$jc_q5) # question 5 "trade" variable (binary)
table(data$jc_q6) # question 6 "rights" variable (binary)
table(data$jc_q7) # question 7 "liberties" variable (binary)

### 3. Average Treatment Effect (ATE)                               
# Creating new variable
data$new[data$jctreat == 1] <- data$jc_q8outcome_1[data$jctreat == 1]
data$new[data$jctreat == 0] <- data$jc_q8outcome_1[data$jctreat == 0]
# Checking new variable
table(data$new[data$jctreat == 1])
table(data$new[data$jctreat == 0])
# Means of groups
treat_group = mean(data$new[data$jctreat == 1], na.rm = TRUE) # mean of scores of respondents who received vignette
control_group = mean(data$new[data$jctreat == 0], na.rm = TRUE) # mean of scores of respondents who did not receive vignette
treat_group
control_group
# Difference in means
diff.data = treat_group - control_group
diff.data
# Student's t-test
data.t = t.test(data$new[data$jctreat == 1], data$new[data$jctreat == 0])
data.t # p-value = 0.8848
t = 0.14515
# Standard error
se.data = diff.data/t
se.data
# 95% confidence interval
data.t$conf.int
# Box plot                               
par(mar = c(4, 6, 4, 2)) 
plot(c(1), c(diff.data),              
     xlim = c(.5, 1.5), 
     ylim = c(-15, 15), 
     main = "Figure 1: Treatment Effect", 
     xlab = "Proportion of Receiving the Vignette Vs. Not Receiving the Vignette", 
     ylab = "95% Confidence Interval Range", 
     cex.lab = .8, cex.main = .8, 
     pch = c(15, 1), 
     xaxt="n", 
     yaxt="n") 
abline(h=seq(-40, 40, 10), col = "gray", lty = 20) 
abline(h=0) 
axis(1, c(1), c("All Respondents"), cex.axis = .8) 
axis(2, seq(-40, 40, 10), seq(-40, 40, 10), las = 3) 
lines(c(1,1), data.t$conf.int, lwd=2) # should come from t-test of data
  
### 4. Linear regression models
# Univariate
reg1 = lm(jc_q8outcome_1 ~ jctreat, data = data) # regressing outcome on treatment condition
summary(reg1)
# Fitted line plot
plot(x = data$jctreat, #x-values
     y = data$jc_q8outcome_1, # y-values
     main = "Figure 2: Relationship Between Being Informed of Trump's Behavior and Perception of U.S. Due to Trump", # label for main title
     ylab = "Perception of U.S. Democracy Due to Trump", #y-axis label
     xlab = "Being Informed of Trump's Behavior", #x-axis label
     pch = 1) # point type
abline(reg1, col = "red") #adds reg. line
# Residual plot
reg1.res = resid(reg1)
plot(data$jctreat, reg1.res, 
       ylab = "Residuals", xlab = "Being Informed of Trump's Behavior", 
       main = "Figure 3: Perception of U.S. Due to Trump") 
abline(0, 0) # adds line at 0

# Multivariate (for robustness checks)
# 1 CV
reg2 = lm(jc_q8outcome_1 ~ jctreat + jc_q1, data = data)
summary(reg2)
# 2 CVs
reg3 = lm(jc_q8outcome_1 ~ jctreat + jc_q1 + jc_q2, data = data)
summary(reg3)
# 3 CVs
reg4 = lm(jc_q8outcome_1 ~ jctreat + jc_q1 + jc_q2 + jc_q3, data = data)
summary(reg4)
# 4 CVs
reg5 = lm(jc_q8outcome_1 ~ jctreat + jc_q1 + jc_q2 + jc_q3 + jc_q4, data = data)
summary(reg5)
# 5 CVs
reg6 = lm(jc_q8outcome_1 ~ jctreat + jc_q1 + jc_q2 + jc_q3 + jc_q4 + jc_q5, data = data)
summary(reg6)
# 6 CVs
reg7 = lm(jc_q8outcome_1 ~ jctreat + jc_q1 + jc_q2 + jc_q3 + jc_q4 + jc_q5 + jc_q6, data = data)
summary(reg7)
# 7 CVs
reg8 = lm(jc_q8outcome_1 ~ jctreat + jc_q1 + jc_q2 + jc_q3 + jc_q4 + jc_q5 + jc_q6 + jc_q7, data = data)
summary(reg8)