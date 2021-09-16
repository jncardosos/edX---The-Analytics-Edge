library(tidyverse)
library(fs)

USDA <- read_csv("unit_01/recitation/USDA.csv")
str(USDA)

summary(USDA)
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]
HighSodium <- subset(USDA, Sodium > 100)
HighSodium$Description
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)

# Plots
plot(USDA$Protein, USDA$TotalFat, xlab= "Protein", ylab = "Fat", main = "Protein vs Fat")

# Histogram
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels")
hist(USDA$VitaminC, 
     xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0, 100), breaks = 1000)

# Boxplot
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab = "Sugar (g)")   


# Adding columns with Above and below average (Transforming boolean in to numeric)
USDA$Sodium[1] > mean(USDA$Sodium, na.rm=TRUE)
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))

USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
str(USDA)


# Transforming the vector in to a matrix
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)


# Applying the mean of Iron in the 2 groups of HighProtein: Above and below averege
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)



# Analysing what are the amount of vitamin C on Above and below the mean Carbs in foods
tapply(USDA$VitaminC, USDA$HighCarbs, mean, na.rm = TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm = TRUE)
