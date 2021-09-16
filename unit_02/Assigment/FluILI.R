
library(tidyverse)
library(lubridate)
install.packages("zoo")
library(zoo)

FluTrain <- read.csv("unit_02/Data/assigment_unit_02/FluTrain.csv")

FluTest <- read.csv("unit_02/Data/assigment_unit_02/FluTest.csv")

FluTest %>% 
    glimpse()

FluTrain %>% 
    
    ggplot(aes(ILI)) +
    geom_histogram(bins = 50)

# When handling a skewed dependent variable, it is often useful to predict the logarithm of the dependent variable instead of the dependent variable itself -- this prevents the small number of unusually large or small observations from having an undue influence on the sum of squared errors of predictive models.

plot(log(FluTrain$ILI), FluTrain$Queries)

FluTrain %>% 
    arrange(-Queries)
    
FluReg <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluReg)

# Normally, we would obtain test-set predictions from the model FluTrend1 using the code PredTest1 = predict(FluTrend1, newdata=FluTest)

# However, the dependent variable in our model is log(ILI), so PredTest1 would contain predictions of the log(ILI) value.

# We are instead interested in obtaining predictions of the ILI value. We can convert from predictions of log(ILI) to predictions of ILI via exponentiation, or the exp() function.

PredTest1 <-  exp(predict(FluReg, newdata = FluTest))
PredTest1

FluTest$Week

SSE_Pred <- sum((PredTest1 - log(FluTest$ILI))^2)
SST_Pred <- sum(mean(log(FluTrain$ILI)) - FluTest$ILI)

1 - SSE_Pred/SST_Pred

FluTrain <- FluTrain %>% 
    mutate(ILILag2 = lag(ILI, 2, na.pad = TRUE))

FluReg2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluReg2)

plot(log(FluTrain$ILILag2), FluTrain$ILI)

summary(FluReg)
summary(FluReg2)

FluTest <- FluTest %>% 
    mutate(ILILag2 = lag(ILI, 2, na.pad = TRUE))

FluTest$ILILag2[x] = FluTrain$ILI[y]

FluTrain$ILI[416]
FluTrain$ILI[417]

nrow(FluTrain)

