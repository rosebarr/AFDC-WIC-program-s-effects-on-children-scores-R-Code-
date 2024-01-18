#Setting the working directory
setwd()
getwd()
data <- read.csv("good.csv")
df <- na.omit(data)


#PART 1: OVERVIEW Descriptive statistics (with NAs removed from Dataset)
head(df)
names(df)


#PART 1.1: Mean, Median, range
summary(df[, c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht")])

#PART 1.2: Standard deviations
sds <- sapply(df[, c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht")], sd)
sds

#Part 1.3: frequencies, correlations)
variables_to_check_freq <- c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht")
frequencies <- lapply(df[variables_to_check_freq], table)
print(frequencies)

#Part 1.4: observed ranges, and correlations)
ranges <- sapply(df[, c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht")], range)
print(ranges)

#Part 1.5: correlation Matrix
library(corrplot)
cor_matrix <- cor(df[, c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht")])
corrplot(cor_matrix, method = "number")
#The correlation plot reveals a strong correlation between 'mathraw97' and 'AGE97'.

install.packages("car")
library(car)
table(df$mathraw97)
scatterplotMatrix(df[,c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")]) 



#Part 1.6: HOME97 Relevance

correlation_HOME97 <- cor(df$HOME97, df$mathraw97)
print(correlation_HOME97)
#The cor function computes the Pearson correlation coefficient, which measures the strength and direction of a linear relationship between two continuous variables. The result is a single number between -1 and 1. 
#A positive value indicates a positive linear relationship, a negative value indicates a negative linear relationship, and 0 indicates no linear relationship.

model <- lm(mathraw97 ~ HOME97, data = df)
summary (model)
plot(df$HOME97, df$mathraw97, main="Scatterplot of HOME97 vs. mathraw97", 
     xlab="HOME97", ylab="mathraw97", pch=19)
abline(model, col="red")
#linear regression model to understand the relationship between the variables and provides additional statistical information about that relationship. 

model_without_HOME97 <- lm(mathraw97 ~ AGE97 + faminc97 + bthwht + WICpreg, data = df)
model_with_HOME97 <- lm(mathraw97 ~ AGE97 + faminc97 + bthwht + WICpreg + HOME97, data = df)

#install.packages("stargazer")
library(stargazer)
stargazer(model_without_HOME97, model_with_HOME97, type = "text")



#2.2.	MULTIPLE REGRESSION MODEL EVALUATION: BEFORE CORRECTIONS

library(lmtest)
model_before_diagnostics <- lm(mathraw97 ~ AGE97 + faminc97 + bthwht + WICpreg + HOME97, data = df)
#Ramsey RESET test used to test the linearity of regression model

#2.1. Residual vs. Fitted Plot
plot(model_before_diagnostics, which = 1)
reset_test <- resettest(model_before_diagnostics, power = 2)
reset_test


#2.2 Evaluate Homoscedasticity
plot(model_before_diagnostics, which = 1)
title("Residuals vs. Fitted Values")
bp_test <- bptest(model_before_diagnostics)
bp_test


# 2.3. Evaluate Normality of Residuals
qqnorm(resid(model_before_diagnostics))
qqline(resid(model_before_diagnostics))
title("Q-Q Plot Standardized Residuals")
shapiro_test <- shapiro.test(resid(model_before_diagnostics))
shapiro_test


#2.4 Outliers
plot(model_with_HOME97, which = 4)
title("Cook's Distance Plot")

# Calculate Cook's distance
cook_dist <- cooks.distance(model_before_diagnostics)

# Find observations with high Cook's distance
influential_observations <- which(cook_dist > 4 / length(cook_dist))

# View the influential observations
Outliers_Index <- as.vector(influential_observations)
length(Outliers_Index)
cat("Outliers Observations index",Outliers_Index )


#3 MULTIPLE REGRESSION MODEL EVALUATION: AFTER CORRECTIONS

#3.1. Making the corrections
# Install and load necessary libraries
#install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)
library(car)

Raw_data <- df[, c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht","HOME97")]
# Predictors "AGE97", "faminc97" and "HOME97" are continou in nature so we take the log

Cont_var_log_transf <- log(Raw_data[, c("AGE97","faminc97","HOME97")])
Des_var <- df[, c("WICpreg", "mathraw97", "bthwht")]
log_Transformed_data <- data.frame(Cont_var_log_transf,Des_var)

#multicollinearity 
Transformed_data_model <- lm(mathraw97~., data =log_Transformed_data)
vif_values <- vif(Transformed_data_model)
vif_values


plot(Transformed_data_model, which = 4)
title("Cook's Distance Plot")

# Calculate Cook's distance
cook_dist <- cooks.distance(Transformed_data_model)

# Find observations with high Cook's distance
influential_observations <- which(cook_dist > 4 / length(cook_dist))

# View the influential observations
Outliers_Index <- as.vector(influential_observations)
Outliers_Index

#Clean data from the outliers
Transformed_clean_data <- log_Transformed_data[-influential_observations, ]
Outliers_Index


# Making comparisons between the previous model and the transformed model 

Final_transformed_data_model <- lm(mathraw97~., data =Transformed_clean_data) 
summary(Final_transformed_data_model)

Final_Orignal_model <- lm(mathraw97~., data = Raw_data) 
summary(Final_Orignal_model)

stargazer(Final_Orignal_model, Final_transformed_data_model, type = "text")


#3.2 Evaluate Linearity for the Log Transformed Model
plot(Final_transformed_data_model, which = 1)
reset_test <- resettest(Final_transformed_data_model, power = 2)
reset_test

#3.3. Evaluate Homoscedasticity Log Transformed Model
plot(Final_transformed_data_model, which = 1)
title("Residuals vs. Fitted Values")

bp_test <- bptest(Final_transformed_data_model)
bp_test


#3.4 Evaluate Normality of Residuals Log Transformed Model
qqnorm(resid(Final_transformed_data_model))
qqline(resid(Final_transformed_data_model))
shapiro_test <- shapiro.test(resid(Final_transformed_data_model))
shapiro_test
