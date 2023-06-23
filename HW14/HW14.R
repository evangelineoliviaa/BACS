setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW14")
library(readxl)
library(ggplot2)
library(magrittr)
library(psych)
#Question 1

#Part A


security <- read_excel("security_questions.xlsx", sheet = "data")
pca <- prcomp(security, scale. = TRUE)

sim_noise_ev <- function(n, p) {
  noise <- data.frame(replicate(p, rnorm(n)))
  eigen(cor(noise))$values
}
evalues_noise <- replicate(100, sim_noise_ev(dim(security)[1], dim(security)[2]))
evalues_mean <- apply(evalues_noise, 1, mean)

screeplot(pca, type="lines",main = "Security Questions PCA Scree Plot")
lines(evalues_mean, type="b",col="orange")
abline(h=1, lty="dotted")

legend(6,8,c("PCA","PA"),lty = c(1,1),col=c("black","orange"))

#Part B

retain_dimensions <- sum(pca$sdev > evalues_mean)
retain_dimensions

#Question 2
pca <- principal(security, nfactors = 3, rotate = "none")
pca
#Part A


#Part B
variance_prop <- pca$Vaccounted[2,1:3]
variance_prop
sum_variance <- sum(pca$Vaccounted[2,1:3])
sum_variance

# The total variance from PC1,PC2,PC3 is 66%

#Part C
threshold <- 0.5
less_explained_items <- rownames(pca$loadings)[pca$communality < threshold]
less_explained_items
pca$communality[2]

#Part D

# Iterate over each item
shared_loadings_items <- c()
for (i in 1:(ncol(pca$loadings) - 1)) {
  for (j in (i + 1):ncol(pca$loadings)) {
    shared_items <- rownames(pca$loadings)[pca$loadings[, i] >= 0.5 & pca$loadings[, j] >= 0.5]
    shared_loadings_items <- union(shared_loadings_items, shared_items)
  }
}

# Print the measurement items that share similar loadings between components
cat("Measurement items with similar loadings between two or more components:",shared_loadings_items)

#Part E

#Answer :

#Question 3

#Part A
pca_rc <-principal(security, nfactors=3, rotate="varimax", scores=TRUE)
pca_rc
rotated_components <- pca_rc$Vaccounted[2,1:3]
rotated_components
variance_prop

comparison_PC_RC <- t(data.frame(pca$values[1:3], pca_rc$values[1:3]))

#Part B
total_rc <- sum(pca_rc$Vaccounted[3,1:3])
total_not <- sum(pca$Vaccounted[3,1:3])

if (total_rc==total_not) {
  print("Yes, the total the three rotated components explain the same cumulative variance as the three principal components combined")
} else {
  print("No, the total the three rotated components does not explain the same cumulative variance as the three principal components combined")
}

#Part C

items_of_interest <- c("Q4", "Q12", "Q17")
principal_loadings_interest <- pca$loadings[items_of_interest, ]
rotated_loadings_interest <- pca_rc$loadings[items_of_interest, ]
loadings_diff <- abs(principal_loadings_interest - rotated_loadings_interest)
loadings_comparison <- cbind(principal_loadings_interest, rotated_loadings_interest,loadings_diff)
colnames(loadings_comparison)[(ncol(loadings_comparison)-2):ncol(loadings_comparison)] <- c("Loadings Difference1", "Loadings Difference2", "Loadings Difference3")
print(loadings_comparison)

#Answer : The items have more clearly differentiated loadings

#Part D 
pca_rc$loadings[pca_rc$loadings[,1]>0.7,1]

#Part E

reduced <- principal(security, nfactors=2, rotate="varimax", scores=TRUE)
reduced
