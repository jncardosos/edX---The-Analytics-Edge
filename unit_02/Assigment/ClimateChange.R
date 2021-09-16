library(tidyverse)

climate <- read_csv("unit_02/Data/assigment_unit_02/climate_change.csv")

climate %>% 
    glimpse()

climate_training <- subset(climate, Year <= 2006)

climate_test <- climate %>% 
    filter(Year > 2006)

climate_training %>% 
    arrange(-Year)

ClimateReg <- lm(Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11`+ `CFC-12` + TSI + Aerosols, data = climate_training)
summary(ClimateReg)

?subset

subset(climate, Year <= 2006)

SSE <- sum(ClimateReg$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(climate_training))
RMSE

cor(climate_training)

ClimateReg2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_training)
summary(ClimateReg2)

ClimateRegStep <- step(ClimateReg)
summary(ClimateRegStep)

TempPredictions <- predict(ClimateRegStep, newdata = climate_test)
SSE_Pred <- sum((TempPredictions - climate_test$Temp)^2)
SST_Pred <- sum((mean(climate_training$Temp) - climate_test$Temp)^2)
R2 <- 1 - SSE_Pred/SST_Pred
R2
