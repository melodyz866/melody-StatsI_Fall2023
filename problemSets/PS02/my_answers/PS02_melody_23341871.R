#question 1 (a)
table <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, ncol = 3, byrow = TRUE)
rownames(table) <- c("upper class", "lower class")
colnames(table) <- c("not stopped", "bribe request", "stopped/warning")

row_frequen <- rowSums(table)

col_frequen <- colSums(table)

grand_total <- sum(table)

expected_frequencies <- outer(row_frequen, col_frequen) / grand_total

chi_statis <- sum((table - expected_frequencies)^2 / expected_frequencies)
#chi statistic = 3.79

#(b)
df <- (nrow(table) - 1) * (ncol(table)- 1)

p_val<-pchisq(chi_statis, df, lower.tail = FALSE)
result <- chisq.test(table)
print(result)
#alpha=0.1, p value=0.15
#(c)
standardized_residuals <- (table - expected_frequencies) / sqrt(expected_frequencies)
print(standardized_residuals)
sink("standardized_residuals_output.txt")
sink()
df_standardized_residuals <- as.data.frame(standardized_residuals)
df_standardized_residuals$class <- rownames(df_standardized_residuals)

install.packages("ggplot2")
library(ggplot2)
ggplot(df_standardized_residuals, aes(x = class, y = "bribe request")) +
  geom_point() +
  labs(x = "class", y = "Standardized Residual (bribe request)") +
  theme_minimal()

#question 2
install.packages("tidyverse") 
library(tidyverse) 
data<-read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summarise(data)
bivariate_model <- lm(water ~ reserved, data = data)
summary(bivariate_model)

ggplot(data, aes(x = reserved, y = water)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Reserved policy", y = "Water facility") +
  ggtitle("Scatterplot of Water facilityvs. Reserved policy") +
  theme_minimal()
