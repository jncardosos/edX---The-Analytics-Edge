library(tidyverse)

# 1.1 e 1.2----

CPS <- read_csv("unit_01/Data/assigment_unit_1/employment_demographics/CPSData.csv")

CCODE <- read_csv("unit_01/Data/assigment_unit_1/employment_demographics/CountryCodes.csv")

CPS %>% 
    group_by(Industry) %>% 
    tally() %>% 
    arrange(-n)

# 1.3

CPS %>% 
    group_by(State) %>% 
    tally() %>% 
    arrange(-n)

# 1.4

CCODE

CPS %>% 
    group_by(Citizenship) %>% 
    tally()

# 1.5 ----

CPS %>% 
    filter(Hispanic == 1) %>% 
    group_by(Race) %>% 
    tally()

# 2.1 ----

CPS %>% 
    summarise_all(~sum(is.na(.))) %>% 
    view()

# 2.2 ----

table(CPS$Region, is.na(CPS$Married))

table(CPS$Sex, is.na(CPS$Married))

table(CPS$Age, is.na(CPS$Married))

table(CPS$Citizenship, is.na(CPS$Married))

# 2.3 ----

CPS %>% 
    group_by(State) %>% 
    summarise(non_metro = sum(is.na(MetroAreaCode)),
              metro = sum(!is.na(MetroAreaCode))) %>% 
    arrange(metro)

# 2.4 ----

CPS %>% 
    group_by(Region) %>% 
    summarise(non_metro = sum(is.na(MetroAreaCode)),
              metro = sum(!is.na(MetroAreaCode))) %>% 
    arrange(metro)

# 2.5 ----

CPS %>% 
    group_by(State) %>% 
    summarise(non_metro = sum(is.na(MetroAreaCode)),
              metro = sum(!is.na(MetroAreaCode))) %>% 
    arrange(metro) %>% 
    mutate(prop = non_metro/(metro+non_metro)) %>%
    arrange(-prop) %>% view()

# 3.1 ----

MetroAreaCode <- read_csv("unit_01/Data/assigment_unit_1/employment_demographics/MetroAreaCodes.csv")

# 3.2 ----

CPS = merge(CPS, MetroAreaCode, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

CPS <- as_tibble(CPS)

CPS %>% 
    summarise_all(~sum(is.na(.))) %>% 
    view()

# 3.3 ----

CPS %>% 
    group_by(MetroArea) %>% 
    tally() %>% 
    arrange(-n)

# 3.4 ----

CPS %>%
    group_by(MetroArea) %>% 
    summarize(hispanic_1 = mean(Hispanic)) %>% 
    arrange(-hispanic_1)

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing = TRUE)

# 3.5 ----

CPS %>% 
    group_by(MetroArea, Race) %>% 
    tally() %>% 
    arrange(-n)

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean), decreasing = TRUE)

# 3.6 ----

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))

# 4.1 ----

CPS = merge(CPS, CCODE, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

CPS %>% 
    glimpse()

# 4.2 ----

CPS %>% 
    summarise_all(~sum(is.na(.))) %>% 
    view()

CPS %>% 
    group_by(Country) %>% 
    tally() %>% 
    arrange(-n)

# 4.3 ----

CPS %>% 
    group_by(MetroArea) %>%
    filter(Country != "United States") %>% 
    filter(MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA") %>% 
    tally(na.rm = TRUE)
?tally

# 4.4 ----

CPS %>% 
    group_by(MetroArea) %>% 
    filter(Country == "Somalia") %>% 
    tally(sort = TRUE)
