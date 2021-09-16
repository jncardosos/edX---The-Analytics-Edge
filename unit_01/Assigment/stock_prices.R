
library(tidyverse)
library(fs)

IBM <- read_csv("unit_01/Data/assigment_unit_1/stock_dynamics/IBMStock.csv")

GE <- read_csv("unit_01/Data/assigment_unit_1/stock_dynamics/GEStock.csv")

ProcterGamble <- read_csv("unit_01/Data/assigment_unit_1/stock_dynamics/ProcterGambleStock.csv")

CocaCola <- read_csv("unit_01/Data/assigment_unit_1/stock_dynamics/CocaColaStock.csv")

Boeing <- read_csv("unit_01/Data/assigment_unit_1/stock_dynamics/BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

# 1.1 e 1.2 ----

IBM %>% 
    arrange(StockPrice)

summary(IBM)

# 1.3 a 1.8 ----

summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)       
sd(ProcterGamble$StockPrice)

# 2.1 to 2.3 ----

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("1983-01-01")), lwd=2)

ggplot() + 
    geom_line(data = CocaCola, aes(x = Date, y = StockPrice, color = "red"))+
    geom_line(data = ProcterGamble, aes(x = Date, y = StockPrice), color = "blue")

# 3.1 to 3.3----

plot(CocaCola$Date[301:432], 
     CocaCola$StockPrice[301:432], 
     type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
lines(ProcterGamble$Date, GE$StockPrice, col="green")
lines(ProcterGamble$Date, IBM$StockPrice, col="orange")
lines(ProcterGamble$Date, Boeing$StockPrice, col="black")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

# 3.4 ----

plot(CocaCola$Date[301:432], 
     CocaCola$StockPrice[301:432], 
     type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
lines(ProcterGamble$Date, GE$StockPrice, col="green")
lines(ProcterGamble$Date, IBM$StockPrice, col="orange")
lines(ProcterGamble$Date, Boeing$StockPrice, col="black")
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-01-01")), lwd=2)

# 4.1 ----

IBM %>% 
    group_by(months(Date)) %>% 
    mutate(mean_overall = mean(StockPrice)) %>% 
    ungroup() %>% 
    summarize(mean_price = mean(StockPrice))

mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

# 4.2 ---- 
    
mean(GE$StockPrice)
tapply(GE$StockPrice, months(IBM$Date), mean)

mean(CocaCola$StockPrice)
which.max(tapply(CocaCola$StockPrice, months(IBM$Date), mean))













