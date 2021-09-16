library(tidyverse)
library(lubridate)

# 1. ----

parole <- read.csv("unit_03/00_data/parole.csv")

parole %>% 
    group_by(violator) %>% 
    tally()

parole %>% glimpse()

# 2. ----

parole <- parole %>% 
    mutate(
        male = as.factor(male),
        race = as.factor(race),
        state = as.factor(state),
        crime = as.factor(crime),
        violator = as.factor(violator)
    )

summary(parole)
str(parole)
parole %>% glimpse()


# 3. ----

set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

# 4. ----

ParoleLog1 <-  glm(violator ~ ., data=train, family=binomial)

ParoleLog1  %>% summary()