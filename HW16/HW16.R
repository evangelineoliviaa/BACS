setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW16")

# Load the data and remove missing values
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", 
                 "model_year", "origin", "car_name")
cars$car_name <- NULL
cars <- na.omit(cars)
# IMPORTANT: Shuffle the rows of data in advance for this project!
cars <- cars[sample(1:nrow(cars)),]
# DV and IV of formulas we are interested in
cars_full <- mpg ~ cylinders + displacement + horsepower + weight + acceleration + 
  model_year + factor(origin)
cars_reduced <- mpg ~ weight + acceleration + model_year + factor(origin)
cars_full_poly2 <- mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + 
  poly(weight, 2) + poly(acceleration, 2) + model_year + 
  factor(origin)
cars_reduced_poly2 <- mpg ~ poly(weight, 2) + poly(acceleration,2) + model_year + 
  factor(origin)
cars_reduced_poly6 <- mpg ~ poly(weight, 6) + poly(acceleration,6) + model_year + 
  factor(origin)

#Part I
lm_full <- lm(cars_full, data = cars)

#Part II
lm_reduced <- lm(cars_reduced, data = cars)

#Part III
lm_poly2_full <- lm(cars_full_poly2, data = cars)

#Part IV
lm_poly2_reduced <- lm(cars_reduced_poly2, data = cars)

#Part V
lm_poly6_reduced <- lm(cars_reduced_poly6, data = cars)

#Part VI
library(rpart)
rt_full <- rpart(formula = cars_full, data = cars)


#Part VII
rt_reduced <- rpart(formula = cars_reduced, data = cars)



#Question 1

mse_in <- function(model, data) {
  predicted <- predict(model, data)
  actual <- data$mpg
  mse <- mean((actual - predicted)^2)
  return(mse)
}

mse_lm_full <- mse_in(lm_full, cars)
# Compute MSEin for lm_reduced
mse_lm_reduced <- mse_in(lm_reduced, cars)

# Compute MSEin for lm_poly2_full
mse_lm_poly2_full <- mse_in(lm_poly2_full, cars)

# Compute MSEin for lm_poly2_reduced
mse_lm_poly2_reduced <- mse_in(lm_poly2_reduced, cars)

# Compute MSEin for lm_poly6_reduced
mse_lm_poly6_reduced <- mse_in(lm_poly6_reduced, cars)

# Compute MSEin for rt_full
mse_rt_full <- mse_in(rt_full, cars)

# Compute MSEin for rt_reduced
mse_rt_reduced <- mse_in(rt_reduced, cars)

# Create a data frame to report the MSEin values
mse_report <- data.frame(
  Model = c("lm_full", "lm_reduced", "lm_poly2_full", "lm_poly2_reduced", "lm_poly6_reduced", "rt_full", "rt_reduced"),
  MSEin = c(mse_lm_full, mse_lm_reduced, mse_lm_poly2_full, mse_lm_poly2_reduced, mse_lm_poly6_reduced, mse_rt_full, mse_rt_reduced)
)

# Print the MSEin report
print(mse_report)

#Question 2

#Part A
train_indices <- sample(1:nrow(cars), size=0.70*nrow(cars))
train_indices

#Part B
#set.seed(27935752) # use your own seed, or use this one to compare to next class notes
train_set <- cars[train_indices,]
trained_model <- lm(lm_reduced, data = train_set)
coefficients(trained_model)

#Part C
test_set <- cars[-train_indices,]
mpg_predicted <- predict(trained_model, test_set)

mse_in2 <- mse_in(trained_model,train_set)
mse_out2 <- mean((test_set$mpg - mpg_predicted)^2)

mse_in2  # In-sample mean squared error
mse_out2  # Out-of-sample mean squared error


#Part D
mpg_actual <- test_set$mpg
pred_err <- mpg_actual - mpg_predicted
results <- data.frame(Actual = mpg_actual, Predicted = mpg_predicted, out = pred_err)

# Show the first several rows of the results data frame
head(results,5)

#Question 3

#Part A
#Part I

k_fold_mse1 <- function(dataset, k=10,model) {
  fold_pred_errors <- sapply(1:k, \(i) {
    fold_i_pe1(i, k, dataset,model)
  })
  pred_errors <- unlist(fold_pred_errors)
  mean(pred_errors^2)
}

fold_i_pe1 <- function(i, k, dataset,model) {
  folds <- cut(1:nrow(dataset), k, labels = FALSE)
  test_indices <- which(folds == i)
  test_set <- dataset[test_indices,]
  train_set = dataset[-test_indices, ]
  trained_model = lm(model,data=train_set)
  predictions = predict(trained_model, test_set)
  actuals=test_set[,1]  
  pred_errors = actuals - predictions  
  return(pred_errors)
}

models <- list(
  lm_full = list(formula = cars_full),
  lm_reduced = list(formula = cars_reduced),
  lm_poly2_full = list(formula = cars_full_poly2),
  lm_poly2_reduced = list(formula = cars_reduced_poly2),
  lm_poly6_reduced = list(formula = cars_reduced_poly6)
)
# Perform k-fold cross-validation for each model and report MSEout
results <- lapply(models, function(model) {
  mse_out <- k_fold_mse1(cars, k = 10,model)
  return(mse_out)
})

results_df <- data.frame(Model = names(results), MSEout = unlist(results))

k_fold_mse2 <- function(dataset, k = 10, model) {
  fold_pred_errors <- sapply(1:k, \(i) {
    fold_i_pe2(i, k, dataset, model)
  })
  pred_errors <- unlist(fold_pred_errors)
  mean(pred_errors^2)
}

fold_i_pe2 <- function(i, k, dataset, model) {
  folds <- cut(1:nrow(dataset), k, labels = FALSE)
  test_indices <- which(folds == i)
  test_set <- dataset[test_indices,]
  train_set <- dataset[-test_indices, ]
  trained_model <- rpart(model, data = train_set)
  predictions <- predict(trained_model, test_set)
  actuals <- test_set[, 1]
  pred_errors <- actuals - predictions
  return(pred_errors)
}

models2 <- list(
  rt_full = list(formula = rt_full),
  rt_reduced = list(formula = rt_reduced)
)

results2 <- lapply(models2, function(model) {
  mse_out2 <- k_fold_mse2(cars, k = 10, model)
  return(mse_out2)
})


# Create a data frame to store the results
results_df2 <- data.frame(Model = names(results2), MSEout = unlist(results2))

MSE_Out<- rbind(results_df,results_df2)

#PartII
cbind(MSE_Out[2],mse_report[2])

#Answer: MSE_Out is bigger

#Part III
set.seed(27935753)
repetitions <- 5

mse_out_repetitions <- replicate(repetitions, {
  mse_out <- k_fold_mse(cars, k = 10, model = lm_full)
  return(mse_out)
})

mse_out_repetitions

#Answer : Yes its the same


#PartB

#Part I

k <- 392
n <- nrow(cars)  # Assuming 'cars' is the dataset

# Initialize empty vectors to store the number of rows
train_rows <- numeric(k)
test_rows <- numeric(k)

# Calculate the number of rows in each dataset for each iteration
for (i in 1:k) {
  test_rows[i] <- ceiling(n / k)
  train_rows[i] <- n - test_rows[i]
  n <- n - test_rows[i]
}

# Print the number of rows in the training dataset and test dataset for each iteration
for (i in 1:k) {
  cat("Iteration", i, ": Train Rows =", train_rows[i], ", Test Rows =", test_rows[i], "\n")
}

#There are 391 rows are in the training dataset and test dataset of each iteration of k-fold CV when k=392

#Part II
results3 <- lapply(models, function(model) {
  mse_out2 <- k_fold_mse2(cars, k = 392, model)
  return(mse_out2)
})

results_df3 <- data.frame(Model = names(results3), MSEout = unlist(results3))
# Create a data frame to store the results
results4 <- lapply(models2, function(model) {
  mse_out <- k_fold_mse1(cars, k = 392, model)
  return(mse_out)
})

results_df4 <- data.frame(Model = names(results4), MSEout = unlist(results4))

MSE_Out2<- rbind(results_df3,results_df4)
