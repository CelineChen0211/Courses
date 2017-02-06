# STA 141 HW 1 
# Problem 5

# -----------------------
# AirPassengers
# Figure 1-1
j = 0
Y = matrix( rep(0,12*12), 12, 12)
for (i in 1948:1960) {
        Y[j,] = AirPassengers[ time(AirPassengers) >= i & time(AirPassengers) < (i+1)]
        j = j + 1
}
plot(Y[1,], xlab = "Month", ylab = "Monthly airline passenger numbers",
     type = "b", pch = 1,col= 1, ylim = c(100,750))
for (i in 2:12) {
        points(Y[i,], type = "b", pch = i, col = i)
}
legend("topleft", legend = c(1949:1960), lty=rep(1,12), col=c(1:12), bty = "n", cex = 0.4)

# Figure 1-2
plot(AirPassengers)

# Parameters
class(AirPassengers) # "ts"
typeof(AirPassengers) # "double"
length(AirPassengers) # 144

# -----------------------
# EuStockMarkets
?EuStockMarkets
# Figure 2-1
DAX = EuStockMarkets[,1]
SMI = EuStockMarkets[,2]
CAC = EuStockMarkets[,3]
FTSE = EuStockMarkets[,4]
plot(DAX, xlab = "Year", ylab = "Daily Closing Prices of European Stock",
     type="l",col="black", ylim = c(1000,10000))
points(SMI, type = "l", col = "Red")
points(CAC, type = "l", col = "Orange")
points(FTSE, type = "l", col = "Blue")
legend("topleft", legend = c("DAX", "SMI", "CAC", "FTSE"), lty=c(1,1,1,1),
       col=c("black", "red", "orange", "blue"), bty='n')
# Figure 2-2
hist(EuStockMarkets)
# Parameters
class(EuStockMarkets) # "mts"    "ts"     "matrix"
head(EuStockMarkets)

# -----------------------
# PlantGrowth
?PlantGrowth
# Figure 3-1
boxplot(weight ~ group, data = PlantGrowth, main = "PlantGrowth data") 

# Figure 3-2
Data = split(PlantGrowth$weight, PlantGrowth$group)
plot(Data$ctrl, xlab = "Trials", ylab = "Weight",
     type="l",col="black", ylim = c(3.5,8))
points(Data$trt1, type = "l", col = "Red")
points(Data$trt2, type = "l", col = "Green")
legend("topleft", legend = c("Ctrl", "Trt1", "Trt2"), 
       lty=c(1,1,1), col=c("black", "red", "green"), bty='n')
# Parameters
class(PlantGrowth) # "data.frame"

# -----------------------
# trees
# Figure 4-1
plot(trees) 
# Figure 4-2
par(mfrow = c(1,1))
plot(Girth^2*Height ~ Volume, data = trees, xlab = "Volume (ft^3)", ylab = "V (calulated)") 
Model = lm(Girth^2*Height ~ Volume, data = trees)
abline(Model)
# Parameters
class(trees)
dim(trees)


# -----------------------
# AirQuality
# Figure 5
par(mfrow = c(2, 2))
boxplot(Ozone ~ Month, data = airquality, main = "Ozone")
boxplot(Solar.R ~ Month, data = airquality, main = "Solar Radiation")
boxplot(Wind ~ Month, data = airquality, main = "Wind")
boxplot(Temp ~ Month, data = airquality, main = "Max Daily Temperature")
# Parameters
class(airquality) # "data.frame"
dim(airquality) # 153   6
head(airquality)
#   Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
# 3    12     149 12.6   74     5   3
# 4    18     313 11.5   62     5   4
# 5    NA      NA 14.3   56     5   5
# 6    28      NA 14.9   66     5   6







