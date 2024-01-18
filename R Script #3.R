setwd()
getwd()

data <- read.csv("good.csv")
df <- na.omit(data)

#PART 1: OVERVIEW Descriptive statistics (with NAs removed from Dataset)
head(df)
names(df)

df$RACE <- ifelse(df$CHRACE == 9, NA,
                       ifelse(df$CHRACE == 1, 0,
                              ifelse(df$CHRACE == 2, 1, NA)))


#PART 1.1: Mean, Median, range
summary(df[, c("WICpreg", "RACE", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")])

#PART 1.2: Standard deviations
sds <- sapply(df[, c("WICpreg", "RACE", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")], sd)
sds

#Part 1.3: frequencies, correlations)
variables_to_check_freq <- c("WICpreg", "RACE", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")
frequencies <- lapply(df[variables_to_check_freq], table)
print(frequencies)

#Part 1.4: observed ranges, and correlations)
ranges <- sapply(df[, c("WICpreg", "RACE", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")], range)
print(ranges)

#Part 1.5: correlation Matrix
library(corrplot)
cor_matrix <- cor(df[, c("WICpreg", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")])
corrplot(cor_matrix, method = "number")
#The correlation plot reveals a strong correlation between 'mathraw97' and 'AGE97'.

scatterplotMatrix(df[,c("WICpreg","RACE", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")])


#2. Linear regression model without transformations/ interactions effects
lm1<-lm(mathraw97~WICpreg + faminc97 + AGE97 + RACE + HOME97 + bthwht, data=df)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)


#3. Summary of the transformations

library(ggplot2)
library(stats)
library(dplyr)
install.packages("car")
library(car)
table(df$mathraw97)
scatterplotMatrix(df[,c("WICpreg","RACE", "mathraw97", "AGE97", "faminc97", "bthwht", "HOME97")]) 


#3.1.Log transform family income
df$log_faminc97 <- log(df$faminc97)
lm2 <- lm(mathraw97 ~ WICpreg + log_faminc97 + AGE97 + RACE + HOME97 + bthwht, data = df)
summary(lm2)

scatterplotMatrix(df[,c("WICpreg","RACE", "mathraw97", "AGE97", "log_faminc97", "bthwht", "HOME97")])


#3.2. Remove outliers
#identify outliers
cd <- cooks.distance(lm2)
plot(cooks.distance(lm2), pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 0.00586, col="red")  # add cutoff line
text(x=1:length(cd), y=cd, labels=ifelse(cd > 0.0058,names(cd),""), col="red", pos = 4) 

#eliminate outliers
df2 <- subset(df, cd < 0.00586) # n = 1848 ->  1831
summary(df2)



#4. Interaction effects 
#Mean centering
df2$AGE97c <- df2$AGE97 -mean(df2$AGE97)
df2$HOME97c <- df2$HOME97 - mean(df2$HOME97)
df2$faminc97c <- df2$log_faminc97 - mean(df2$log_faminc97)

mean(df$AGE97)
mean(df2$AGE97c)

mean(df$HOME97)
mean(df2$HOME97c)

mean(df$faminc97)
mean(df2$faminc97c)

library(ggplot2)


# 4.1. Interaction effect WICpreg * famin97c

summary(lmA<-lm(mathraw97 ~ WICpreg*faminc97c + 
                  RACE + AGE97c +  bthwht + HOME97c, data=df2))

#4.1.1.
ggplot(df2, aes(x = faminc97c, y = mathraw97, color = factor(WICpreg))) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, aes(group = factor(WICpreg))) +
  labs(x = "Centered Family Income (faminc97c)", y = "Math Scores (mathraw97)", color = "WICpreg") +
  ggtitle("Interaction Effect of WICpreg and faminc97c on Math Scores")


install.packages("interplot")
library(interplot)
plotlmA <- interplot(m = lmA, var1 = "faminc97c", var2 = "WICpreg")
plotlmA


# 4.2. Interaction effect WICpreg * RACE
summary(lmB<-lm(mathraw97 ~ WICpreg*RACE + 
                  faminc97c + AGE97c +  bthwht + HOME97c, data=df2))

#4.2.1.
ggplot(df2, aes(x = RACE, y = mathraw97, color = factor(WICpreg))) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, aes(group = factor(WICpreg))) +
  labs(x = "RACE (RACE)", y = "Math Scores (mathraw97)", color = "WICpreg") +
  ggtitle("Interaction Effect of WICpreg and RACE on Math Scores")


install.packages("sjPlot")
library(sjPlot)
interaction.plot(df2$RACE, df2$WICpreg, df2$mathraw97)



# 4.3. Interaction effect WICpreg * AGE
summary(lmC<-lm(mathraw97 ~ WICpreg*AGE97c + 
                  faminc97c + RACE +  bthwht + HOME97c, data=df2))

#4.3.1.
ggplot(df2, aes(x = AGE97c, y = mathraw97, color = factor(WICpreg))) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, aes(group = factor(WICpreg))) +
  labs(x = "AGE of children (AGE97c)", y = "Math Scores (mathraw97)", color = "WICpreg") +
  ggtitle("Interaction Effect of WICpreg and AGE97c on Math Scores")


install.packages("sjPlot")
library(sjPlot)
interaction.plot(df2$AGE97c, df2$WICpreg, df2$mathraw97)

