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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# run regression  outcome variable : voteshare / the explanatory variable : difflog

model1 <- lm(voteshare ~ difflog, data = inc.sub)

# scatter plot with regression line

plot(inc.sub$difflog, inc.sub$voteshare, main = "Scatter Plot 1  with Regression Line", 
     xlab = "difflog", ylab = "voteshare")
abline(lm(voteshare ~ difflog, data = inc.sub), col = "pink")

# save the residuals as a new object , and summary residuals of the model as txt file
summary(model1)
residuals1 <- residuals(model1)

residuals_summary <- capture.output(summary(model1$residuals))
writeLines(residuals_summary, "residuals_summary.txt")

regression1_summary <- capture.output(summary(model1))
writeLines(regression1_summary, "regression1_summary.txt")


# voteshare= 0.579031 (intercept) +0.041666 × difflog

#question2
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)
plot(inc.sub$difflog, inc.sub$presvot, main = "Scatter Plot 2  with Regression Line", 
     xlab = "difflog", ylab = "presvot")
abline(lm(presvote ~ difflog, data = inc.sub), col = "blue")

residuals2 <- residuals(model2)
regression2_summary <- capture.output(summary(model2))
writeLines(regression2_summary, "regression2_summary.txt")
# presvote = 0.507583 + 0.023837 × difflog

#question3 

model3 <- lm(voteshare ~ presvote, data = inc.sub)

summary(model3)

plot(inc.sub$presvote, inc.sub$voteshare, main = "Scatter Plot 3  with Regression Line", 
     xlab = "presvote", ylab = "voteshare")
abline(lm(voteshare ~ presvote, data = inc.sub), col = "yellow")

regression3_summary <- capture.output(summary(model3))
writeLines(regression3_summary, "regression3_summary.txt")

# voteshare =  0.441330  +   0.388018 × presvote

#question4 
model4 <- lm(residuals1 ~ residuals2)

summary(model4)

plot(residuals2, residuals1, main = "Scatter Plot 4  with Regression Line", 
     xlab = "residuals2", ylab = "residuals1")

abline(lm(residuals1 ~ residuals2), col = "red")

regression4_summary <- capture.output(summary(model4))
writeLines(regression4_summary, "regression4_summary.txt")


# residuals1 = -4.860e-18  + 2.569e-01 × residuals2

#question5 

model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model5 )


regression5_summary <- capture.output(summary(model5))
writeLines(regression5_summary, "regression5_summary.txt")

# voteshare = 0.4486442+ 0.0355431× difflog + 0.2568770 × presvote



