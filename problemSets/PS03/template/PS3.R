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

model <- lm(voteshare ~ difflog, data = inc.sub)

# scatter plot with regression line

plot(inc.sub$difflog, inc.sub$voteshare, main = "Scatter Plot 1  with Regression Line", 
     xlab = "difflog", ylab = "voteshare")
abline(lm(voteshare ~ difflog, data = inc.sub), col = "pink")

# save the residuals of the model
summary(model)
residuals <- residuals(model)

residuals_summary <- capture.output(summary(model$residuals))
writeLines(residuals_summary, "residuals_summary.txt")
getwd()

