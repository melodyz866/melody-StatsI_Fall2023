#question 1
install.packages("car")
Yes
library(car)
data(Prestige)
help(Prestige)
head(Prestige, 10)

# a: Create a new variable 'professional' by exist variable 'type'
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
table(Prestige$professional)
# b: regression
model1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model1)
regression_summary <- capture.output(summary(model1))
writeLines(regression_summary, "regression_summary.txt")

# C: predictive equation: 
#Prestige=21.1423+0.0031709×Income+37.7813×Professional−0.0023257×(Income×Professional)

