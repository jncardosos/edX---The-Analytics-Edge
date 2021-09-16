
library(tidyverse)

# Reading files

# 1.1 to 1.2
crimes_USA <- read_csv("unit_01/Data/mvtWeek1.csv")
str(crimes_USA)

# 1.3
max(crimes_USA$ID)

# 1.4
min(crimes_USA$Beat)

# 1.5
table(crimes_USA$Arrest)

# 1.6
crimes_USA %>% 
    filter(grepl("ALLEY", crimes_USA$LocationDescription))

crimes_USA %>% 
    filter(LocationDescription == "ALLEY") %>% 
    count()

# 2.1

crimes_USA$Date

# 2.2

DateConvert = as.Date(strptime(crimes_USA$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

# 2.3

crimes_USA$Month = months(DateConvert)
crimes_USA$WeekDay = weekdays(DateConvert)
crimes_USA$Date = DateConvert

table(crimes_USA$Month)

crimes_USA %>% 
    group_by(Month) %>% 
    count() 
    
# 2.4

which.max(table(crimes_USA$WeekDay))

crimes_USA %>% 
    group_by(WeekDay) %>% 
    count() 

# 2.5

crimes_USA %>% 
    filter(Arrest == TRUE) %>% 
    group_by(Month) %>% 
    count()
   
# 3.1 

hist(crimes_USA$Date, breaks=100)

# 3.2

boxplot(crimes_USA$Date~crimes_USA$Arrest)

# 3.3

table(crimes_USA$Arrest, crimes_USA$Year)

crimes_USA %>% 
    filter(Year == 2001) %>% 
    group_by(Arrest) %>% 
    count()

2152/(18517+2152)

# 3.4

table(crimes_USA$Arrest, crimes_USA$Year)

crimes_USA %>% 
    filter(Year == 2007) %>% 
    group_by(Arrest) %>% 
    count()

1212/(1212+13068)

# 3.5

table(crimes_USA$Arrest, crimes_USA$Year)

crimes_USA %>% 
    filter(Year == 2012) %>% 
    group_by(Arrest) %>% 
    count()
550/(550+13542)

# 4.1

sort(table(crimes_USA$LocationDescription), decreasing = TRUE)

# 4.2

?subset
crimes_USA_top5 <- subset(crimes_USA, crimes_USA$LocationDescription == "STREET" | 
                          crimes_USA$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|
                          crimes_USA$LocationDescription == "ALLEY"|
                          crimes_USA$LocationDescription == "GAS STATION"|
                          crimes_USA$LocationDescription == "DRIVEWAY - RESIDENTIAL")

# 4.3 

crimes_USA_top5$LocationDescription = factor(crimes_USA_top5$LocationDescription)
crimes_USA_top5 %>% 
    filter(Arrest == TRUE) %>% 
    group_by(LocationDescription) %>% 
    count()

table(crimes_USA_top5$Arrest, crimes_USA_top5$LocationDescription)

# 4.4

crimes_USA_top5 %>% 
    filter(LocationDescription == "GAS STATION") %>% 
    group_by(WeekDay) %>% 
    count()

# 4.5

crimes_USA_top5 %>% 
    filter(LocationDescription == "DRIVEWAY - RESIDENTIAL") %>% 
    group_by(WeekDay) %>% 
    count()



