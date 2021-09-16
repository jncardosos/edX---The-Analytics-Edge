

WHO <- read_csv("unit_01/Data/WHO.csv")
str(WHO)
head(WHO)


mean(WHO$Over60, na.rm=TRUE)
sd(WHO$Over60, na.rm=TRUE)
summary(WHO)
which.min(WHO$Over60)
WHO[183,]
WHO$Country[183]

which.max(as.numeric(WHO$LiteracyRate))
WHO[44,]

tapply(WHO$ChildMortality, WHO$Region, mean)
