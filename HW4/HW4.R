#Question 1

#a

#i
setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW4")
verizon<- read.csv("verizon.csv")
plot(density(verizon$Time),lwd=2,col="red",main="Verizon Repair Time")
abline(v=mean(verizon$Time),lwd = 2)
ILEC<- subset(verizon,Group=="ILEC")
plot(density(ILEC$Time),lwd=2,col="red",main="ILEC Repair Time")
abline(v=mean(ILEC$Time),lwd = 2)
CLEC<- subset(verizon,Group=="CLEC")
par(mfrow=c(1, 2))
plot(density(ILEC$Time),lwd=2,col="red",main="ILEC Repair Time")
abline(v=mean(ILEC$Time),lwd = 2)
plot(density(CLEC$Time),lwd=2,col="red",main="CLEC Repair Time")
abline(v=mean(CLEC$Time),lwd = 2)

#ii
#Given what the PUC wishes to test, how would you write the hypothesis? (not graded)
# h0 : Verizon take 7.6 minutes on average to repair phone services 
# h1 : Verizon

#iii Estimate the population mean, and the 99% confidence interval (CI) of this estimate.

population_mean <- mean(verizon$Time)
cat("The Population Mean is :",population_mean)

sd_error <- sd(verizon$Time)/(nrow(verizon)^0.5)

ci99_low <- population_mean - (sd_error*2.58)
ci99_high <- population_mean + (sd_error*2.58)

cat("The 99% CI of the population is :", ci99_low, " to ", ci99_high)


#iv Find the t-statistic and p-value of the test
hypothesized_mean <- 7.6
t <- (population_mean-hypothesized_mean)/sd_error

df<- nrow(verizon) - 1
p <- 1 - pt(t,df)

#v Briefly describe how these values relate to the Null distribution of t (not graded)

#vi What is your conclusion about the company’s claim from t his t-statistic, and why?
#reject because p value below 0.05

#PART B

#i Bootstrapped Percentile: Estimate the bootstrapped 99% CI of the population mean

set.seed(86472122)
num_boots<-7000
sample_statistic<-function(sample0) {
  resample <-sample(sample0, length(sample0), replace=TRUE)
  mean(resample)
}
sample_means<-replicate(num_boots, sample_statistic(verizon$Time))
quantile(sample_means, probs= c(0.005, 0.995))

#ii Bootstrapped Difference of Means: 
#What is the 99% CI of the bootstrapped difference between the sample mean and the hypothesized mean

boot_mean_diffs<-function(sample0, mean_hyp) {
  resample <-sample(sample0, length(sample0), replace=TRUE)
  return( mean(resample)-mean_hyp)
}
mean_diffs<-replicate(num_boots, boot_mean_diffs(verizon$Time, 7.6)) 
quantile(mean_diffs, probs=c(0.005, 0.995))

#iii Plot distribution the two bootstraps above
par(mfrow=c(1, 2))
plot(density(sample_means), main="Means of bootstrapped samples")
plot(density(mean_diffs), main="Population and Hypothesized mean difference\nof bootstrapped samples")
abline(v=quantile(mean_diffs, probs=c(0.005, 0.995)), lty="dashed")

#iv Does the bootstrapped approach agree with the traditional t-test in part [a]?
#Yes the bootstrapped difference means doesn't shows that there are any 0 difference between bootstrap mean and verizon's claim so we should reject the null hypothesis

#PARTC

#i Bootstrapped Percentile: Estimate the bootstrapped 99% CI of the population median
sample_medians<-replicate(num_boots, sample_statistic(verizon$Time))
quantile(sample_medians, probs= c(0.005, 0.995))

#ii What is the 99% CI of the bootstrapped difference between the sample median and the hypothesized median?
boot_median_diffs<-function(sample0, median_hyp) {
  resample <-sample(sample0, length(sample0), replace=TRUE)
  return(median(resample)-median_hyp)
}
median_diffs<-replicate(num_boots, boot_median_diffs(verizon$Time, 3.5)) 
quantile(median_diffs, probs=c(0.005, 0.995))


#iii Plot distribution the two bootstraps above
par(mfrow=c(1, 2))
plot(density(sample_medians), main="Medians of bootstrapped samples")
plot(density(median_diffs), main="Medians and Hypothesized median difference\nof bootstrapped samples")
abline(v=quantile(median_diffs, probs=c(0.005, 0.995)), lty="dashed")

#iv What is your conclusion about Verizon’s claim about the median, and why?

#QUESTION 2
library("compstatslib")
interactive_t_test()

#PARTA

#i Would this scenario create systematic or random error (or both or neither)?
#This scenario would create systematic error (bias)

#ii Which part of the t-statistic or significance (diff, sd, n, alpha) would be affected?
# The alpha



