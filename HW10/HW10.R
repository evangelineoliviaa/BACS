setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW10")
programmer <-read.csv("programmer_salaries.txt", sep="\t")
library(ggplot2)
library(dplyr)

library(compstatslib)
interactive_regression()

#Question 1

#Part A :
# Answer : Scenario 1 will have a stronger R^2
#Since data points are narrowly dispersed around a clear and strong trend line , the proportion of variability in y that can be explained by x is likely to be higher. In this case, the linear relationship between x and y is likely to be strong, and the R2 value will reflect this.


#Part B :
#Answer : Scenario 3 will have a stronger R^2
#Since the data points are narrowly dispersed around a clear trend line, the linear relationship between x and y are strong. This means that the proportion of variability in y that can be explained by x is still relatively high. As a result, the R2 value is likely to be higher.
# However, it will result in a decreasing manner since most of the data points are around decreasing regression line.



#Part C :
#Answer : 


#Part D :
#Answer :


#Question 2
#Part A
programmer_regression<- lm(Salary ~ Experience + Score + Degree, data=programmer)
programmer_regression

prog_std<-data.frame(scale(programmer))
prog_regr_std<-lm(Salary ~ Experience + Score, data=programmer)
summary(prog_regr_std)

summary(programmer_regression)
head(programmer_regression$fitted.values,5)
head(programmer_regression$residuals,5)

#Part B
#Part I
x <- cbind(1, programmer[, c("Experience", "Score", "Degree")])
#Part II
y <- programmer$Salary
#Part III
beta_hat <- solve(t(x) %*% as.matrix(x)) %*% t(x) %*% y
#Part IV
y_hat <- as.matrix(x) %*% beta_hat
res <- y - y_hat

head(y_hat, 5)
head(res, 5)
#Part V
SSR <- sum((y_hat - mean(y))^2)
SSR
SSE <- sum(res^2)
SSE
SST <- sum((y - mean(y))^2)
SST

#Part C
#Part I
R2_1 <- SSR / SST
R2_1

#Part II
R2_2 <- cor(y, y_hat)^2
R2_2

#Question 3

auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")

#Part A
#Part I
par(mfrow = c(3, 3))
plot(auto$mpg ~ auto$cylinders, xlab = "Cylinders", ylab = "MPG")
plot(auto$mpg ~ auto$displacement, xlab = "Displacement", ylab = "MPG")
plot(auto$mpg ~ auto$horsepower, xlab = "Horsepower", ylab = "MPG")
plot(auto$mpg ~ auto$weight, xlab = "Weight", ylab = "MPG")
plot(auto$mpg ~ auto$acceleration, xlab = "Acceleration", ylab = "MPG")
plot(auto$mpg ~ auto$model_year, xlab = "Model Year", ylab = "MPG")
plot(auto$mpg ~ auto$origin, xlab = "Origin", ylab = "MPG")


#Part II
auto_temp<-auto[1:8]
round(cor(auto_temp, use="pairwise.complete.obs"),2)

#Part III 


#Part V


#Part B

#Part I

autolm <- lm(mpg ~ cylinders + displacement + 
               horsepower+ weight+ acceleration + 
               model_year+ factor(origin), data = auto)
coefficients_table <- data.frame(summary(autolm)$coefficients)
coefficients_table
significant_variables <- coefficients_table[coefficients_table$`Pr...t..` < 0.01,]
significant_variables


#Part II

#No, it is not possible to determine which independent variables are the most effective at increasing mpg since all independent variable are not standardized


#Part C

#Part I
auto_sd <- data.frame(auto[1:7])
auto_sd <- scale(auto_sd)
auto_std <- cbind(auto_sd,auto$origin)
colnames(auto_std)[8] ="origin"
auto_std <- data.frame(auto_std)
auto_stdregr <- lm(mpg ~ cylinders + displacement + horsepower+ weight+ acceleration + model_year+ factor(origin), data = auto_std)
summary(auto_stdregr)


#Part II
#The non-significant independent variables at the 1% level are
#Cylinders,Horsepower,Acceleration

lm_cylinder <- lm(mpg ~ cylinders, data = auto_std)
summary(lm_cylinder)

lm_horsepower <- lm(mpg ~ horsepower, data = auto_std)
summary(lm_horsepower)

lm_acceleration <- lm(mpg ~ acceleration, data = auto_std)
summary(lm_acceleration)

#Answer : All of them will become become significant when we regress mpg over them individually

#Part III
par(mfrow = c(1, 2))
hist(auto_stdregr$residuals, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(auto_stdregr$residuals, main = "Normal Probability Plot of Residuals")
qqline(auto_stdregr$residuals)

#The data is not normally distributed and centered around zero


