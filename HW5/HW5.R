#1

#PART A
#https://stackoverflow.com/questions/57410033/tidyr-vs-dplyr-reshape2
#reshape2 focuses on reshaping data : provides more flexibility for transforming wide data to long format 
#especially when dealing with multiple variables that need to be reshaped simultaneously.

#PART B
setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW5")
verizon<- read.csv("verizon_wide.csv")
library(reshape2)
loads_long<-melt(verizon, na.rm= TRUE, 
                 variable.name= "PhoneService", 
                 value.name= "ResponseTime")

#PART C
head(loads_long)
tail(loads_long)

#PART D
ILEC<-subset(loads_long, loads_long$PhoneService == "ILEC")
CLEC<-subset(loads_long, loads_long$PhoneService == "CLEC")

plot(density(ILEC$ResponseTime),lwd = 2, main = "ILEC & CLEC Density Plot")
lines(density(CLEC$ResponseTime),lwd = 2, col = "red")

legend(110,
       0.12,
       c("ILEC","CLEC"),
       lwd = c(2,2),
       lty = c("solid","solid"),
       col = c("black","red"))


#QUESTION 2

#PART A
#Null: The Mean of Response Time for CLEC equal or less than ILEC. 
#Alternative: The Mean of Response Time for CLEC is greater than ILEC. 

#PART B

#i Conduct the test assuming variances of the two populations are equal
t.test(verizon$CLEC,verizon$ILEC, alt = "greater",var.equal=TRUE)

#ii Conduct the test assuming variances of the two populations are not equal
t.test(verizon$CLEC,verizon$ILEC, alt = "greater",var.equal=FALSE)


#PART C

#i Visualize the distribution of permuted differences, and indicate the observed difference as well.
observed_diff <- mean(verizon$CLEC,na.rm=TRUE) - mean(verizon$ILEC)

permute_diff<-function(values, groups) {
  permuted <-sample(values, replace = FALSE)
  grouped <-split(permuted, groups)
  permuted_diff<-mean(grouped$CLEC) - mean(grouped$ILEC)
}

nperms <- 10000

permuted_diffs<-replicate(nperms, permute_diff(loads_long$ResponseTime, loads_long$PhoneService))
hist(permuted_diffs, breaks = "fd", probability = TRUE)
lines(density(permuted_diffs), lwd=2)

#ii What are the one-tailed and two-tailed p-values of the permutation test?

p_1tailed <-sum(permuted_diffs > observed_diff) / nperms

p_2tailed <-sum(abs(permuted_diffs) > observed_diff) / nperms

#Would you reject the null hypothesis at 1% significance in a one-tailed test?
# Yes

#QUESTION 3

#Part A

#a Compute the W statistic comparing the values. You may use either the permutation approach (try the functional form) or the rank sum approach.

time_ranks <-rank(loads_long$ResponseTime)
ranked_groups<-split(time_ranks, loads_long$PhoneService)

U1 <-sum(ranked_groups$ILEC)
n1 <-length(verizon$ILEC)

W<-U1 -(n1 * (n1 + 1))/2


#b Compute the one-tailed p-value for W.

n2 <-length(na.omit(verizon$CLEC))

wilcox_p_1tail <- 1 -pwilcox(W, n1, n2)

#c Run the Wilcoxon Test again using the wilcox.test() function in R – make sure you get the same W as part [a]. Show the results.

wilcox.test(verizon$ILEC,verizon$CLEC , alternative = "greater")

#At 1% significance, and one-tailed, would you reject the null hypothesis that the values of CLEC and ILEC are similar?
#We cannot reject the null hypothesis


#QUESTION 4

#Part A
norm_qq_plot <- function(values) {
  probs1000 <- seq(0, 1, 0.001)
  q_vals <- quantile(values,probs = probs1000)
  q_norm <- qnorm(probs1000, mean = mean(values), sd = sd(values))
  plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles")
  abline(a = 0, b = 1, col="red", lwd=2)
  }

#PART B

set.seed(978234)
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)
d123 <- c(d1, d2, d3)

plot(density(d123))
norm_qq_plot(d123)


#PART C
#ILEC
plot(density(permuted_diffs))
norm_qq_plot(permuted_diffs)

#CLEC
plot(density(na.omit(verizon$CLEC)))
norm_qq_plot(na.omit(verizon$CLEC))

