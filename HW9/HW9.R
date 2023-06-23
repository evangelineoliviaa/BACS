library(data.table)
library(lsa)
library(compstatslib)
setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW9")
ac_bundles_dt <- fread("piccollage_accounts_bundles.csv")
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1, with=FALSE])

#Question 1

#Part A
#Let’s explore to see if any sticker bundles seem intuitively similar:
#i) How many recommendations does each bundle have?
#Answer : Six recommendations for each bundles

#ii) Find a single sticker bundle that is both in our limited data set and also in the app’s Sticker Store 
#(e.g., “sweetmothersday”). Then, use your intuition to recommend (guess!) five other bundles in our dataset that might have similar usage patterns as this bundle.


#Part B 
# Let’s find similar bundles using geometric models of similarity
#i) Let’s create cosine similarity based recommendations for all bundles:
#1) Create a matrix or data.frame of the top 5 recommendations for all bundles
#2) Create a new function that automates the above functionality: it should take an accounts-bundles matrix as a parameter, and return a data object with the top 5 recommendations for each bundle in our data set, using cosine similarity.
recom_cos <- function(ac_bundles_matrix){  
  # Get the cosine similarity matrix
  sim_matrix <- cosine(ac_bundles_matrix)
  diag(sim_matrix)<-100
  
  
  recommendations <- matrix(NA, nrow = ncol(sim_matrix), ncol = 5)
  
  for (i in 1:ncol(sim_matrix)) {
    similar <- order(sim_matrix[, i], decreasing = TRUE)[2:6]
    id <- rownames(sim_matrix)[similar]
    recommendations[i,] <- id
  }
  recommendations <- as.data.frame(recommendations)
  colnames(recommendations) <- c("1st","2nd","3rd","4th","5th")
  recommendations$Bundle_Name <- rownames(sim_matrix)
  return(recommendations)
}

recom_cos(ac_bundles_matrix)[76,]

#ii) Let’s create correlation based recommendations.
#1) Reuse the function you created above (don’t change it; don’t use the cor() function)
#2) But this time give the function an accounts-bundles matrix where each 
# bundle (column) has already been mean-centered in advance.
bundle_means<-apply(ac_bundles_matrix, 2, mean)
bundle_means_matrix<-t(replicate(nrow(ac_bundles_matrix), bundle_means))
ac_bundles_mc_b<-ac_bundles_matrix-bundle_means_matrix

#3) Now what are the top 5 recommendations for the bundle you chose to explore earlier?
recom_cor <- recom_cos(ac_bundles_mc_b)[76,]
recom_cor

#iii) Let’s create adjusted-cosine based recommendations.
#1) Reuse the function you created above (you should not have to change it)
#2) But this time give the function an accounts-bundles matrix where each 
# account (row) has already been mean-centered in advance.
bundle_means<-apply(ac_bundles_matrix, 1, mean)
account_means_matrix<-replicate(ncol(ac_bundles_matrix), bundle_means)
ac_bundles_mc_ac<-ac_bundles_matrix-account_means_matrix

recom_cor <- recom_cos(ac_bundles_mc_ac)[76,]
recom_cor

#Part C
#(not graded) Are the three sets of geometric recommendations similar in nature (theme/keywords) to the recommendations you picked earlier using your intuition alone? 
#What reasons might explain why your computational geometric recommendation models produce different results from your intuition?


#Part D
#(not graded) What do you think is the conceptual difference in cosine similarity, correlation, and adjusted-cosine?

#Question 2

#Part A
# Scenario A: Create a horizontal set of random points, with a relatively narrow but flat distribution.

# i) What raw slope of x and y would you generally expect?
# I expect a raw slope of x and y to be close to zero (or 0)

# ii) What is the correlation of x and y that you would generally expect?
# The correlation of x and y would generally be close to 0 if the horizontal set of random points has a relatively narrow but flat distribution.

#Part B
# Scenario B: Create a random set of points to fill the entire plotting area, along both x-axis and y-axis

# i) What raw slope of the x and y would you generally expect?

# ii) What is the correlation of x and y that you would generally expect?


#Part C
# Scenario C: Create a diagonal set of random points trending upwards at 45 degrees

# i) What raw slope of the x and y would you generally expect? (note that x, y have the same scale)

# ii) What is the correlation of x and y that you would generally expect?


#Part D 
# Scenario D: Create a diagonal set of random trending downwards at 45 degrees

# i) What raw slope of the x and y would you generally expect? (note that x, y have the same scale)

# ii) What is the correlation of x and y that you would generally expect?


pts<- interactive_regression()
pts
cor(pts)
  