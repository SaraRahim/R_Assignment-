SARA ANGELINA RAHIM
R00211761

------------------------------------------------------------------------------------------------------- 
# read the dataset into a variable called "cars"
cars <- read.csv("cars.csv")

# seperating the csv into columns
cars <- read.table("C://Users//Sara//OneDrive//Documents//cars.csv", header = T, sep = "")

# display the seperated csv
cars

# max, min, range
max(cars$Price) = 61.9
min(cars$Price) = 7.4
range(cars$Price) = 7.4 61.9

min(cars$EngineSize) = 1
max(cars$EngineSize) = 5.7
range(cars$EngineSize) = 1.0 5.7 

min(cars$Fuel.tank.capacity) = 9.2
max(cars$Fuel.tank.capacity) = 27
range(cars$Fuel.tank.capacity) = 9.2 27.0

min(cars$RPM) = 3800
max(cars$RPM) = 6500
range(cars$RPM) = 3800 6500
 
min(cars$Weight) =  1695
max(cars$Weight) = 4105
range(cars$Weight) = 1695 4105

max(cars$MPG.city) = 80
min(cars$MPG.city) = 7.9
range(cars$MPG.city) = 7.9 80.0

min(cars$Cylinders) = 3
max(cars$Cylinders) = 8
range(cars$Cylinders) = 3 8

min(cars$Horsepower) = 55
max(cars$Horsepower) = 300
range(cars$Horsepower) = 55 300

min(cars$Passengers) = 2
max(cars$Passengers) = 8
range(cars$Passengers) = 2 8

# find the mean 
mean(cars$Price) = 19.36848
mean(cars$EngineSize)= 2.886957
mean(cars$Fuel.tank.capacity)= 16.62826
mean(cars$RPM)= 5267.391
mean(cars$Weight)= 3074.837
mean(cars$MPG.city)= 21.7837
mean(cars$Cylinders) = 4.967391
mean(cars$Horsepower) = 142.6196
mean(cars$Passengers) = 5.119565

# finding the summary stats of each variable
summary(cars$Price)
summary(cars$EngineSize)
summary(cars$Fuel.tank.capacity)
summary(cars$MPG.city)
summary(cars$Cylinders)
summary(cars$Passengers)
summary(cars$Weight)
summary(cars$Horsepower)

HISTOGRAM AND BOXPLOTS
-----------------------------------------------------------------------------------------------------------------------
# Create a histograms and boxplots
hist(cars$Price, main = "Price Distribution Histogram", xlab = "Price (in thousands)", ylab = "Frequency")
boxplot(cars$Price, main = "Price Distribution Boxplot", xlab = "Price (in thousands)", ylab = "frequency")

hist(cars$EngineSize, main = "EngineSize Distribution Histogram", xlab = "size (in liters)", ylab = "Frequency")
boxplot(cars$EngineSize, main = "Engine Size Distribution Boxplot", xlab = "Engine Size (in liters)", ylab = "frequency")

hist(cars$RPM, main = "RPM Distribution Histogram", xlab = "RPM (per minute)", ylab = "Frequency")
boxplot(cars$RPM, main = "RPM Distribution Boxplot", xlab = "RPM (per minute)", ylab = "frequency")

hist(cars$Fuel.tank.capacity, main = "Fuel.tank.capacity Distribution Histogram", xlab = "Fuel Tank Capacity (in gallons)", ylab = "Frequency")
boxplot(cars$Fuel.tank.capacity, main = "Fuel.tank.capacity Distribution Boxplot", xlab = "Fuel Tank Capacity (in gallons)", ylab = "frequency")

hist(cars$MPG.city, main = "MPG.city Distribution Histogram", xlab = "MPG.city (in miles per gallon)", ylab = "Frequency")
boxplot(cars$MPG.city, main = "MPG.city Distribution Boxplot", xlab = "MPG.city (in miles per gallon)", ylab = "frequency")

hist(cars$Cylinders, main = "Cylinders Distribution Histogram", xlab = "Cylinders (per car)", ylab = "Frequency")
boxplot(cars$Cylinders, main = "Cylinders Distribution Boxplot", xlab = "Cylinders (per car)", ylab = "frequency")

hist(cars$Passengers, main = "Passengers Distribution Histogram", xlab = "Passengers (per car)", ylab = "Frequency")
boxplot(cars$Passengers, main = "Passengers Distribution Boxplot", xlab = "Passengers (per car)", ylab = "frequency")

hist(cars$Horsepower, main = "Horsepower Distribution Histogram", xlab = "Horsepower (per car)", ylab = "Frequency")
boxplot(cars$Horsepower, main = "Horsepower Distribution Boxplot", xlab = "Horsepower (per car)", ylab = "frequency")

hist(cars$Weight, main = "Weight Distribution Histogram", xlab = "Weight (per car)", ylab = "Frequency")
boxplot(cars$Weight, main = "Weight Distribution Boxplot", xlab = "Weight (per car)", ylab = "frequency")


CORRELATION COEFFICIENTS
----------------------------------------------------------------------------------------------------------------------
# getting the correlation coefficient
correlation_matrix <- cor(cars[c("Price", "EngineSize", "Horsepower", "MPG.city", "Weight")])

# printing the correlation_matrix
correlation_matrix

# create a scatter plot of Price vs. MPG.city
plot(cars$Price, cars$MPG.city, xlab = "Price", ylab = "MPG.city", main = "Price vs. MPG.city")
plot(cars$Price, cars$MPG.city, xlab = "Price", ylab = "MPG.city", main = "Price vs. MPG.city")
plot(cars$Horsepower, cars$Price, xlab = "Horsepower", ylab = "Price", main = "Horsepower vs. Price")
plot(cars$Weight, cars$Price, xlab = "Weight", ylab = "Price", main = "Weight vs. Price")
plot(cars$EngineSize, cars$Price, xlab = "Engine Size", ylab = "Price", main = "EngineSize vs. Price")
plot(cars$EngineSize, cars$HorsePower, xlab = "Engine Size", ylab = "HorsePower", main = "EngineSize vs. Horsepower")
plot(cars$EngineSize, cars$Weight, xlab = "Engine Size", ylab = "Weight", main = "EngineSize vs. Weight")
plot(cars$Horsepower, cars$MPG.city, xlab = "Horsepower", ylab = "MPG.city", main = "Horsepower vs. MPG.city")
plot(cars$Horsepower, cars$Weight, xlab = "Horsepower", ylab = "Weight", main = "Horsepower vs. Weight")
plot(cars$MPG.city, cars$Weight, xlab = "MPG.city", ylab = "Weight", main = "MPG.city vs. Weight")
plot(cars$EngineSize, cars$MPG.city, xlab = "EngineSize", ylab = "MPG.city", main = "EngineSize vs. MPG.city")

REGRESSION 
----------------------------------------------------------------------
# getting regression formula 
reg_model <- lm(Horsepower ~ EngineSize, data = cars)


# Divide the dataset into two groups based on origin
cars_USA <- subset(cars, Origin == "USA")
cars_non_USA <- subset(cars, Origin == 1)

# summary of each group, 1 and 2
summary(cars_USA[c("EngineSize", "Horsepower")])
summary(cars_nonUSA[c("EngineSize", "Horsepower")])

# boxplot for each group, 1 and 2
boxplot(EngineSize ~ Origin, data = cars, 
        ylab = "Engine Size", xlab = "Origin", 
        main = "Engine Size by Origin")
boxplot(Horsepower ~ Origin, data = cars, 
        ylab = "Horsepower", xlab = "Origin", 
        main = "Horsepower by Origin")

# 95% coefficiant 
t.test(cars_USA$EngineSize, conf.level = 0.95)
t.test(cars_nonUSA$EngineSize, conf.level = 0.95)

# coefficient of the regression model
coef(reg_model)

# residuals diagram 
par(mfrow = c(2, 2))
plot(model)

# residuals histogram
hist(residuals(regression_model), xlab = "Residuals", 
main = "Histogram of Residuals")


