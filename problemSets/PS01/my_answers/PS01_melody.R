#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
# (1) 90%  confidence interval 

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

confidence_level <- 0.90

y_size <- length(y)
y_mean <- mean(y)
y_sd <- sd(y)

degrees_of_freedom <- length(y) - 1
t_critical <- qt((1 - confidence_level) / 2, df = degrees_of_freedom)
lower_Confidence <- y_mean - (t_critical * (y_sd / sqrt(y_size)) )
upper_Confidence <- y_mean + (t_critical * (y_sd / sqrt(y_size)) )

cat("Confidence Interval : [", lower_Confidence, ", " , upper_Confidence, "]\n")

# (2) hypothesis
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
alpha <- 0.05
mu<-100
t_statistic <- (mean(y) - mu) / (sd(y) / sqrt(length(y)))
p_value <- 2 * pt(abs(t_statistic), degrees_of_freedom, lower.tail = FALSE)
t_test_IQ <- t.test(y, mu = 100)  


# p_value: 0.72, which is  greater than 𝓪= 0.05; so no enough evidence to reject the null hypothesis; In another word no sufficient evident to prove the IQ score of randon 25 students in the school higher than the average IQ score among all the school in the country.  .

#####################
# Problem 2
#####################

install.packages("tidyverse") 
library(tidyverse) 
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
relationship_expenditure <- expenditure[, c("Y", "X1", "X2", "X3")]
par(mfrow = c(1, 3))  

plot(relationship_expenditure$X1, relationship_expenditure$Y, main = "Y vs. X1", xlab = "X1", ylab = "Y", col = "yellow")
lm_model1 <- lm(Y ~ X1, data = relationship_expenditure)
abline(lm_model, col = "blue")
install.packages("stargazer")

library(stargazer)
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
output_stargazer("regression_output1.tex", lm_model1)

plot(relationship_expenditure$X2, relationship_expenditure$Y, main = "Y vs. X2", xlab = "X2", ylab = "Y", col = "pink")
lm_model<- lm(Y~X2, data = relationship_expenditure)
abline(lm_model,col="black")

plot(relationship_expenditure$X3, relationship_expenditure$Y, main = "Y vs. X3", xlab = "X3", ylab = "Y", col = "blue")
lm_model<- lm(Y~X3, data = relationship_expenditure)
abline(lm_model,col="black")

plot(relationship_expenditure$X1, relationship_expenditure$X2, main = "X1 vs. X2", xlab = "X1", ylab = "X2", col = "blue")
lm_model<- lm(X1~X2, data = relationship_expenditure)
abline(lm_model,col="black")

plot(relationship_expenditure$X1, relationship_expenditure$X3, main = "X1 vs. X3", xlab = "X1", ylab = "X3", col = "green")
lm_model<- lm(X1~X3, data = relationship_expenditure)
abline(lm_model,col="black")

plot(relationship_expenditure$X2, relationship_expenditure$X3, main = "X2 vs. X3", xlab = "X2", ylab = "X3", col = "orange")
lm_model<- lm(X2~X3, data = relationship_expenditure)
abline(lm_model,col="black")
summary(lm_model)

pairs(relationship_expenditure[, c("Y", "X1", "X2", "X3")], col = c("blue", "red", "green", "purple"))

ggplot(expenditure, aes(x = X1)) +
  geom_line(aes(y = Y, color = "Y vs. X1")) +
  geom_line(aes(x = X2, y = Y, color = "Y vs. X2")) +
  geom_line(aes(x = X3, y = Y, color = "Y vs. X3")) +
  labs(title = "plot of Y, X1, X2, and X3", x = "X1_X2_X3", y = "Y_expenditures") +
  theme_minimal()

detach("package:ggplot2", unload = TRUE)
install.packages("ggplot2")

install.packages("ggplot2")
library("ggplot2")
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
expenditure$Region <- as.factor(expenditure$Region)
library("ggplot2")
ggplot(expenditure, aes(x=Y, fill=Region))+ geom_histogram(binwidth = 6, position = "dodge")+labs(title = "histogram Y by region", x="expenditures on shelter", y="regions") + theme_minimal()
ggplot(expenditure,aes(x=factor(Region), y=Y, fill=factor(Region)))+ geom_bar(stat = "identity")+labs(title = "bar chart Y by region", X="region", y="expenditures")+ theme_minimal()
ggplot(expenditure, aes(x=X1, y=Y))+ geom_line()+labs(title = "line Y by X1", x="person income", y="expenditures on shelter")+theme_minimal()

ggplot(expenditure,aes(x=X1, y=Y, color= Region))+geom_line()+geom_point(aes(shape= Region), size=6) + labs(title = "line Y by X1&Region", x="person income", y="expenditures on shelter") + theme_minimal()+scale_shape_manual(values = c("1"=0, "2"=1,"3"=2,"4"=5))

