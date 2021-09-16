library(tidyverse)

wine <- read_csv("unit_02/Data/lectures/wine.csv") 

wine %>% 
    summary()

model1 = lm(Price ~ AGST, data = wine)
summary(model1) # multiple R squared will increase if more independent variables are added to the model, however, Ajusted R squared will decrease if the added variable doesn't help the model

SSE <- model1$residuals %>% 
    sum(model1$residuals^2)

model12 <- lm(Price ~ AGST + HarvestRain, data = wine)

model12 %>% 
    summary()

SSE_2 <- model12$residuals %>% 
    sum(model12$residuals^2)

model13 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

model13 %>% 
    summary()

SSE_3 <- model13$residuals %>% 
    sum(model13$residuals^2)

model_quiz <- lm(Price ~ HarvestRain + WinterRain, data = wine)

model_quiz %>% 
    summary()

model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)

cor(wine$WinterRain, wine$Price)

cor(wine$Age, wine$FrancePop)

cor(wine)

model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
model5 %>% 
    summary()

wineTest <- read_csv("unit_02/Data/lectures/wine_test.csv")

str(wineTest)
predictTest <- predict(model4, newdata = wineTest)
predictTest

SSE_Predicted <- sum((wineTest$Price - predictTest)^2)
SST_Predicted <- sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE_Predicted/SST_Predicted
