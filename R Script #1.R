#Setting the working directory
setwd()
getwd()
Complete_Dataset <- read.csv("good.csv")

#Exploring the dataset
head(Complete_Dataset)
names(Complete_Dataset)


#Display information about the Dataset's structure,
str(Complete_Dataset)


#check null variales in the dataset
Null_Values <- sum(is.na(Complete_Dataset))
print(Null_Values)

#The Dataset has a total of 4778 null values, so I need to remove them by cleaning the datsset
Processed_Data<- na.omit(Complete_Dataset)


#3.1. DESCRIPTIVE ANALYTICS

vars_to_summarize <- c("faminc97", "readss97", "AGE97", "HOME97", "WICpreg", "AFDCpreg", "bthwht")
summary_stats <- sapply(Processed_Data[vars_to_summarize], summary)
print(summary_stats)

sd(Processed_Data$faminc97)
sd(Processed_Data$AGE97)



#3.2 correlation matrix

library(dplyr)

cor_vars <- Processed_Data %>%
  select(readss97, WICpreg, AFDCpreg, AGE97, faminc97, HOME97, bthwht)
correlation_matrix <- cor(cor_vars, use = "pairwise.complete.obs")
print(correlation_matrix)



# Linear regression model

Regression_Model <- lm(readss97 ~ WICpreg, data=Processed_Data)
summary(Regression_Model)
plot(Processed_Data$WICpreg, Processed_Data$readss97,
     main="Women, Infant and Children Nutrition Program participation during pregnancy and Reading Scores",
     xlab="AWomen, Infant and Children Nutrition Program participation during pregnancy", ylab="Reading Scores")
abline(Regression_Model, col="blue")


Regression_Model <- lm(readss97 ~ HOME97, data=Processed_Data)
summary(Regression_Model)
plot(Processed_Data$HOME97, Processed_Data$readss97,
     main="Emotional and cognitive stimulation at home and Reading Scores",
     xlab="emotional and cognitive stimulation", ylab="Reading Scores")
abline(Regression_Model, col="blue")





#multiple regression models & nested models

install.packages('stargazer')
library(stargazer)


#model control
modelcontrol <- lm(readss97 ~ AGE97 + faminc97 + bthwht + HOME97, data = Processed_Data)
summary(modelcontrol)


#Nested models
#modelWIC
modelWIC1 <- lm(readss97 ~ WICpreg, data = Processed_Data)
summary(modelWIC1)
modelWIC2 <- lm(readss97 ~ WICpreg + AGE97 + faminc97, data = Processed_Data)
summary(modelWIC2)
modelWIC3 <- lm(readss97 ~ WICpreg + AGE97 + faminc97 + bthwht + HOME97, data = Processed_Data)
summary(modelWIC3)

stargazer(modelWIC1,modelWIC2,modelWIC3, header = F, type="text")



#AFDCpreg
modelAFDC1 <- lm(readss97 ~ AFDCpreg, data = Processed_Data)
summary(modelAFDC1)
modelAFDC2 <- lm(readss97 ~ AFDCpreg + AGE97 + faminc97, data = Processed_Data)
summary(modelAFDC2)
modelAFDCpreg3 <- lm(readss97 ~ AFDCpreg + AGE97 + faminc97 + bthwht + HOME97, data = Processed_Data)
summary(modelAFDCpreg3)

stargazer(modelAFDC1,modelAFDC2,modelAFDCpreg3, header = F, type="text")





