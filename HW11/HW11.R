setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW11")
library(ggplot2)
library(ggpubr)
library(car)
cars<-read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")

#Question 1

cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), 
                                  log(horsepower), log(weight), log(acceleration), 
                                  model_year, origin))

#Part 1
cars_regr_log=lm(log.mpg. ~ 
              log.cylinders.+log.displacement.+
              log.horsepower.+log.weight.+log.acceleration.+
              model_year+factor(origin), data = cars_log) 

summary(cars_regr_log)
#if(abs(log(p_value)) > 2) { paste0("<span style='color:red'>", round(p_value, 3), "</span>") } else { round(p_value, 3) }` 
#Answer : 

#Part 2 

#Answer : Yes , log.horsepower , log.weight, log.acceleration, model_year, origin

#Part 3

#Answer : log.cylinders , log.displacement

#Part B

#Part I
regr_wt <- lm(mpg ~ weight, data = cars)
summary(regr_wt)

#Part II
regr_wt_log <- lm(log.mpg. ~ log.weight., data = cars_log)
summary(regr_wt_log)

#Part III 

#Density Plot
par(mfrow = c(1, 2))
A<- ggplot(data.frame(residuals = residuals(regr_wt)), aes(x = residuals)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Residuals for Regression of MPG on Weight (Raw)")
B<- ggplot(data.frame(residuals = residuals(regr_wt_log)), aes(x = residuals)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Residuals for Regression of MPG on Weight (Log-Transformed)")

ggarrange(A,B + rremove("x.text"), 
          ncol = 2, nrow = 1)


#scatterplot of log.weight. vs. residuals
ggplot(cars_log, aes(x = log.weight., y = residuals(regr_wt_log))) +
  geom_point(alpha = 0.5) +
  labs(x = "Log(Weight)", y = "Residuals", title = "Scatterplot of Log(Weight) vs Residuals for Regression of Log(MPG) on Log(Weight)")

#Part IV

#Answer :


#Part V

#Answer : 


#Part VI
confint(regr_wt_log, level = 0.95)


#Question 2
regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
                 log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)


#Part A
weight_regr <- lm(log.weight. ~ log.cylinders. + log.displacement. + log.horsepower. +
                    log.acceleration. + model_year +
                    factor(origin), data=cars_log)

r2 <- summary(weight_regr)$r.squared
vif <- 1 / (1 - r2)
vif

#Part B

#Part I
vif(regr_log)

#Part II
regr_log <- lm(log.mpg. ~ log.cylinders. + log.horsepower. + log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
vif(regr_log)

#Part III
regr_log <- lm(log.mpg. ~ log.cylinders. + log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
vif(regr_log)

#Part IV
regr_log <- lm(log.mpg. ~ log.horsepower. + log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
vif(regr_log)

regr_log <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
vif(regr_log)

final <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
summary(final)


#Part C

summary(regr_log)$r.squared 
summary(final)$r.squared

#Answer: Yes, It is possible that we have lost some variables that were previously significant in the model. 
#Dropping significant variables may hurt the model's explanatory power, as they may have been important predictors of the dependent variable.

#Since the new model has a slight lower R-squared, that means we slightly hurt our explanation by dropping those variables.

#Part D

#Part I
#If an independent variable has no correlation with other independent variables, it means that its coefficient of determination(R^2) will be zero
#Therefore, the denominator in the VIF formula will be equal to 1, and the VIF score will be 1.


#Part II

#1. To get VIF scores of 5 or higher : 
#Correlation between X1 and X2 would need to be at least 0.8944
#R = sqrt(1 / (1/5 - 1)) = 0.8944

#2. To get a VIF score of 10 or higher:
# Correlation between X1 and X2 would need to be at least 0.9487
#R = sqrt(1 / (1/10 - 1)) = 0.9487


#Question 3

origin_colors = c("blue", "darkgreen", "red")
with(cars_log, plot(log.weight., log.mpg., pch=origin, col=origin_colors[origin]))

#Part A

cars_us <- subset(cars_log, origin==1)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[1], lwd=2)

cars_eur=subset(cars_log, origin==2) 
wt_regr_eur=lm(log.mpg. ~ log.weight., data=cars_eur) 
abline(wt_regr_eur, col=origin_colors[2], lwd=2)  

cars_jp=subset(cars_log, origin==3) 
wt_regr_jp=lm(log.mpg. ~ log.weight., data=cars_jp) 
abline(wt_regr_jp, col=origin_colors[3], lwd=2)

#Part B
#Answer : Yes, I think  different origins have different relationships between weight and mpg.
