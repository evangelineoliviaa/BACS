setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW12")
cars<-read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), 
                                  log(displacement), log(horsepower), log(weight), 
                                  log(acceleration), model_year, origin))
library(ggplot2)


#Question 1

#Part A

#Part I
light_cars<-cars_log[cars_log$log.weight. < log(mean(cars$weight)), ]
heavy_cars<-cars_log[cars_log$log.weight. >= log(mean(cars$weight)), ]

#Part II

ggplot() +
  geom_point(data = light_cars, aes(x = log.acceleration., y = log.mpg., color = "Light", shape = "Light")) +
  geom_point(data = heavy_cars, aes(x = log.acceleration., y = log.mpg., color = "Heavy", shape = "Heavy")) +
  scale_color_manual(values = c(Light = "red", Heavy = "blue")) +
  scale_shape_manual(values = c(16, 17)) +
  labs(x = "Acceleration", y = "MPG", color = "Weight", shape = "Weight")


#Part III

ggplot() +
  geom_point(data = light_cars, aes(x = log.acceleration., y = log.mpg., color = "Light", shape = "Light")) +
  geom_point(data = heavy_cars, aes(x = log.acceleration., y = log.mpg., color = "Heavy", shape = "Heavy")) +
  geom_smooth(data = light_cars, aes(x = log.acceleration., y = log.mpg.), method = "lm", se = FALSE,fullrange=T, color = "blue") +
  geom_smooth(data = heavy_cars, aes(x = log.acceleration., y =log.mpg.), method = "lm", se = FALSE,fullrange=T, color = "red") +
  scale_color_manual(values = c(Light = "red", Heavy = "blue")) +
  scale_shape_manual(values = c(16, 17)) +
  labs(x = "Acceleration", y = "MPG", color = "Weight", shape = "Weight")

#Part IV
light_regr <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin, data = light_cars)
heavy_regr <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + origin, data = heavy_cars)

summary(light_regr)
summary(heavy_regr)


#Part V

#Answer : 


#Question 2

#Part A

#Part B

#Part I
regr_mod <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
summary(regr_mod)

#Part II
regr_int <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin) + log.weight.*log.acceleration., data = cars_log)
summary(regr_int)

#Part III
weight_mc <- scale(cars_log$log.weight., center=TRUE, scale=FALSE)
acceleration_mc <- scale(cars_log$log.acceleration., center=T, scale=F)
mpg_mc <- scale(cars_log$log.mpg., center=T, scale=F)
regr_interaction <- summary(lm(mpg_mc ~ weight_mc + acceleration_mc + weight_mc*acceleration_mc))


#Part IV
weight_acc <- cars_log$log.weight.*cars_log$log.acceleration.
interaction_regr <- lm(weight_acc ~ cars_log$log.weight. + cars_log$log.acceleration.)
interaction_ortho <- interaction_regr$residuals
summary(lm(log.mpg.~log.weight.+log.acceleration.+interaction_ortho,data=cars_log))

#Part V

#Raw 
cor(cars_log$log.weight., cars_log$log.weight. * cars_log$log.acceleration.)
cor(cars_log$log.acceleration., cars_log$log.weight. * cars_log$log.acceleration.)

#Mean Centered
cor(weight_mc, weight_mc*acceleration_mc)
cor(acceleration_mc, weight_mc*acceleration_mc)

#Orthogonalized
cor(interaction_ortho, cars_log$log.weight.)
cor(interaction_ortho, cars_log$log.acceleration.)


#Question 3

#Part A

#Part I
model1 <- lm(log.weight. ~ log.cylinders., data = cars_log)
summary(model1)

#Answer : Yes, based on the p-value it has a significant direct effect on weight

#Part II
model2 <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin), data = cars_log)
summary(model2)

#Answer : Yes, based on the p-value weight has a significant direct effect on mpg


#Part B

indirect_effect <- model1$coefficients[2]*model2$coefficients[2]
indirect_effect

#Part C

#Part I
boot_mediation <- function(model1, model2, dataset) {
  boot_index <- sample(1:nrow(dataset), replace=TRUE)
  data_boot <- dataset[boot_index, ]
  regr1 <- lm(model1, data_boot)
  regr2 <- lm(model2, data_boot)
  return(regr1$coefficients[2] * regr2$coefficients[2])
}

set.seed(42)
indirect <- replicate(2000,boot_mediation(model1, model2, cars_log))
quantile(indirect, probs=c(0.025, 0.975))

#Part II
plot(density(indirect))
abline(v=quantile(indirect, probs=c(0.025, 0.975)),lty=2)


