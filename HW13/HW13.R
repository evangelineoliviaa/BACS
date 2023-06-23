setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW13")
library(readxl)
cars<-read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), log(horsepower), 
                                  log(weight), log(acceleration), model_year, origin))
cars_log <- na.omit(cars_log)
                
#Question 1
# Create a new data.frame with the four log-transformed variables

#Part A

#Part I
log_collinear <- with(cars_log, data.frame(log.cylinders., log.displacement., log.horsepower., log.weight.))

#Part II
eigenvalues <- eigen(cov(log_collinear))
variance_explained <- eigenvalues$values / sum(eigenvalues$values)
variance_explained

#Part III
#Answer : 


#Part B
#Part I 
cars_pca <- prcomp(log_collinear)
cars_log$scores <- cars_pca$x
head(cars_log$scores)

#Part II
summary(lm(log.mpg.~ cars_log$scores[,"PC1"]+log.acceleration. + model_year + factor(origin),data = cars_log))

#Part III
cars_log_standardized <- data.frame(scale(cars_log))
regression_model_standardized <- lm(log.mpg. ~ cars_log$scores[,"PC1"] + log.acceleration. + model_year + factor(origin), data = cars_log_standardized)
summary(regression_model_standardized)

cor_matrix <- cor(cars_log_standardized)
pc1_correlation <- cor_matrix[9,]
pc1_correlation


#Question 2
security <- read_excel("security_questions.xlsx",sheet = "data")

#Part A
pca2 <- prcomp(security,scale. = TRUE)
summary(pca2)$importance[2,]

#Part B
eigenvalues2 <- eigen(cor(security))
eigenvalues2$values
num_dimensions_eigenvalue <- sum(eigenvalues2$values >= 1)
num_dimensions_eigenvalue

screeplot(pca2, type="lines")

#Part C


#Question 3
library("compstatslib")

#Part A
interactive_pca()

