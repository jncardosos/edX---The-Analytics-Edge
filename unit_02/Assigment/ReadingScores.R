
library(tidyverse)

pisa2009_train <- read_csv("unit_02/Data/assigment_unit_02/pisa2009train.csv")
pisa2009_test  <- read_csv("unit_02/Data/assigment_unit_02/pisa2009test.csv")

pisa2009_test <- pisa2009_test %>% 
    mutate(raceeth = as.factor(raceeth))

pisa2009_train <- pisa2009_train %>% 
    mutate(raceeth = as.factor(raceeth))

pisa2009_train %>% glimpse()


pisa2009_train %>% 
    filter(male == 1) %>% 
    pull(readingScore) %>% 
    mean()

pisa2009_train %>% 
    filter(male == 0) %>% 
    pull(readingScore) %>% 
    mean()


tapply(pisa2009_train$readingScore, pisa2009_train$male, mean)

pisa2009_train %>% 
    summarise_all(~sum(is.na(.))) %>% view()

summary(pisa2009_train)

mean(pisa2009_train$minutesPerWeekEnglish, na.rm = TRUE)

pisa2009_train %>% 
    mutate(minutesPerWeekEnglish = 
               replace(minutesPerWeekEnglish, 
                       is.na(minutesPerWeekEnglish), 
                       min(minutesPerWeekEnglish, na.rm = TRUE))) %>% 
    pull(minutesPerWeekEnglish) %>% 
    mean()

pisa2009_train %>% 
    mutate(minutesPerWeekEnglish = 
               replace(minutesPerWeekEnglish, 
                       is.na(minutesPerWeekEnglish), 
                       max(minutesPerWeekEnglish, na.rm = TRUE))) %>% 
    pull(minutesPerWeekEnglish) %>% 
    mean()

pisa2009_train <- na.omit(pisa2009_train) 

pisa2009_test  <- na.omit(pisa2009_test)


pisa2009_train$raceeth = relevel(pisa2009_train$raceeth, "White")
pisa2009_test$raceeth = relevel(pisa2009_test$raceeth, "White")

ReadingScoreReg <- lm(readingScore ~ ., data = pisa2009_train)
summary(ReadingScoreReg)

predTest <- predict(ReadingScoreReg, newdata = pisa2009_test)
summary(predTest)
           