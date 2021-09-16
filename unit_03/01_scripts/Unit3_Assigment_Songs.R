

# Assigment 1 Popularity of Music Records ----

library(tidyverse)
library(lubridate)

# 1. ----

songs <- read.csv("unit_03/00_data/songs.csv")
songs %>% 
    glimpse()

songs <- songs %>% 
    mutate(timesignature = as.factor(timesignature))

songs %>% 
    filter(artistname == "Michael Jackson" & Top10 == 1) %>% 
    view()

songs %>% 
    group_by(timesignature) %>% 
    tally()

songs %>% 
    select(songtitle, tempo) %>% 
    arrange(-tempo)

# 2. ----

SongsTest <- songs %>% 
    filter(year > 2009) %>% 
    select(-year, -songtitle, -artistname, -songID, -artistID) 
    

SongsTrain <- songs %>% 
    filter(year <= 2009) %>% 
    select(-year, -songtitle, -artistname, -songID, -artistID) 

SongsLog1 <-  glm(Top10 ~ ., data=SongsTrain, family=binomial)

SongsLog1 %>% summary()

# The coefficient estimate for loudness is positive, meaning that mainstream listeners prefer louder songs, which are those with heavier instrumentation. However, the coefficient estimate for energy is negative, meaning that mainstream listeners prefer songs that are less energetic, which are those with light instrumentation. These coefficients lead us to different conclusions!

# 3. ----

cor(SongsTrain) %>% view()

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

SongsLog2 %>% summary()

SongsLog3 <-  glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

SongsLog3 %>% summary()
