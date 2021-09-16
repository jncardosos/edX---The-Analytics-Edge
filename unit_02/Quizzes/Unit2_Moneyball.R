# VIDEO 2

# Read in data
baseball = read.csv("unit_02/Data/baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

# W = 0.881375 + 0.105766 (RD)
# W >= 95
# 0.881375 + 0.105766 (RD) >= 95
# RD >= 95
# = 80.8814/0.1058 = 133.4 ~ 135

713-614

80.8814 + 0.105766*99 

# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

RunsReg2 = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsReg2)

# -804.62 + 2737.77 (OBP) + 1584.91 (SLG)
-804.62 + (2737.77*0.338) + (1584.91*0.540)
-804.62 + (2737.77*0.391) + (1584.91*0.450)
-804.62 + (2737.77*0.369) + (1584.91*0.374)
-804.62 + (2737.77*0.313) + (1584.91*0.447)
-804.62 + (2737.77*0.361) + (1584.91*0.500)

-837.38 + (2913.60*0.297) + (1514.29*0.370)


# We predicted: RS = 805, RA = 622
# Our regression equation to predict wins was: Wins = 80.8814 + 0.1058 (RS-RA)
# So our prediction for wins is: Wins = 80.8814 + 0.1058 (805-622) = 100



teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(x = teamRank, y = wins2012)

cor(x = teamRank, y = wins2013)
