setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW17")
library(rpart)
library(rpart.plot)

insurance <- read.csv("insurance.csv", header = TRUE)
insurance <- na.omit(insurance)

#Question 1

#Part A

model <- lm(charges ~ age + sex + bmi + children + smoker + region , data = insurance)
summary(model)

#Answer : The factors age, bmi, children, smokeryes (smoker status), regionsoutheast, and regionsouthwest are all significantly related to charges. 

#Part B
#Part I
tree_model <- rpart(charges ~ age + sex + bmi + children + smoker + region, data = insurance,cp=0)
rpart.plot(tree_model)

#Part II
min_cp_row <- which.min(tree_model$cptable[, "CP"])
max_depth <- tree_model$cptable[min_cp_row, "nsplit"]
print(max_depth)

#Part III
min_xerror_row <- which.min(tree_model$cptable[, "xerror"])
num_leaf_groups <- tree_model$cptable[min_xerror_row, "nsplit"] + 1
print(num_leaf_groups)

#Part IV
rules <- rpart.rules(tree_model)
print(rules)


#Question 2

#Part A
# Function to calculate RMSEout using LOOCV
models <- list(
  "ols_regr" = lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance),
  "decision_tree" = rpart(charges ~ age + sex + bmi + children + smoker + region, data = insurance, cp = 0)
)

loocv_rmse_out <- function(dataset, model) {
  num_obs <- nrow(dataset)
  pred_errors <- numeric(num_obs)
  
  for (i in 1:num_obs) {
    test_set <- dataset[i, ]
    train_set <- dataset[-i, ]
    
    trained_model <- model
    predictions <- predict(trained_model, test_set)
    actual <- test_set[, "charges"]
    pred_errors[i] <- actual - predictions
  }
  
  rmse_out <- sqrt(mean(pred_errors^2))
  return(rmse_out)
}

# Calculate RMSEout for each model using LOOCV
results <- sapply(models, \(m) loocv_rmse_out(insurance, m))

# Print the results
#Part I
print(results[1])

#Part II
print(results[2])


#Question 3

#Part A
set.seed(123)
train_indices <- sample(1:nrow(insurance), size = 0.8 * nrow(insurance))
train_set <- insurance[train_indices, ]
test_set <- insurance[-train_indices, ]

bagged_learn <- function(model, dataset, b = 100) {
  bagged_models <- lapply(1:b, function(i) {
    # 1. Get a bootstrapped (resampled with replacement) dataset
    resampled_data <- dataset[sample(nrow(dataset), replace = TRUE), ]
    
    # 2. Train the model on the bootstrapped dataset
    trained_model <- update(model, data = resampled_data)
    
    # Return the retrained model
    trained_model
  })
  
  # Return the list of bagged models
  bagged_models
}

bagged_predict <- function(bagged_models, new_data) {
  predictions <- lapply(bagged_models, function(model) {
    # Get predictions of new_data using each bagged model
    predict(model, newdata = new_data)
  })
  
  # Apply mean over the columns of predictions
  predictions <- as.data.frame(predictions)
  mean_predictions <- apply(predictions, 1, mean)
  
  # Return the mean predictions
  mean_predictions
}


#Part A
mse_oos <- function(actuals, preds) {
  mean( (actuals - preds)^2 )
}
bagged_models <- bagged_learn(model, train_set, b = 100)

bagged_predictions <- bagged_predict(bagged_models, test_set)
rmse_out <- sqrt(mse_oos(test_set$charges, bagged_predictions))

#Part B
train_indices2 <- sample(1:nrow(insurance), size = 0.8 * nrow(insurance))
train_set2 <- insurance[train_indices2, ]
test_set_tree <- insurance[-train_indices2, ]

bagged_models2 <- bagged_learn(tree_model, train_set2, b = 100)

bagged_predictions2 <- bagged_predict(bagged_models2, test_set_tree)
rmse_out2 <- sqrt(mse_oos(test_set_tree$charges, bagged_predictions))


#Question 4

