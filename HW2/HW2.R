#1a

# Three normally distributed data sets
d1 <- rnorm(n=500, mean=45, sd=4)
d2 <- rnorm(n=200, mean=30, sd=4)
d3 <- rnorm(n=100, mean=15, sd=4)

# Combining them into a composite dataset
d123 <- c(d1, d2, d3)

# Let’s plot the density function of d123
plot(density(d123), col="blue", lwd=2, 
     main = "Distribution 2")

# Add vertical lines showing mean and median
abline(v=mean(d123),lwd = "3")
abline(v=median(d123))

#1b

# Three normally distributed data sets
d1 <- rnorm(n=800, mean=20, sd=4)

# Let’s plot the density function of d123
plot(density(d1), col="blue", lwd=2, 
     main = "Distribution 3")
mean(d1)
median(d1)
# Add vertical lines showing mean and median
abline(v=mean(d1),lwd = "3",col = " red")
abline(v=median(d1),col = " green")

#1C
#Mean is more likely to be affected by outliers in the data since we have to sum all the values and divided by the number of data which the data may have outliers


#2a
rdata <- rnorm(n=2000, mean=0, sd=1)
plot(density(rdata), col="blue", lwd=2, main = "Distribution Q2")
abline(v=mean(rdata),lwd = "4",col = " red")

lines <- seq(-3,3)
abline(v=sd(rdata)*lines,lty = "dashed")

dist<- function(data){
  first = (quantile(data,0.25)-mean(data))/sd(data)
  second = (quantile(data,0.50)-mean(data))/sd(data)
  third = (quantile(data,0.75)-mean(data))/sd(data)
  cat(round(first,5),"standard deviations away from the mean corresponding to 1st quartile\n")
  cat(round(second,5),"standard deviations away from the mean corresponding to 2nd quartile\n")
  cat(round(third,5),"standard deviations away from the mean corresponding to 3rd quartile\n")
}

#2b
q_data<- c(0.25,0.50,0.75)
rdata_q<-abline(v=quantile(rdata,q_data),col = "green")
dist(rdata)


#2C 
rdata.new<-rnorm(n = 2000,mean = 35,sd = 3.5)
dist(rdata.new)


#2D

dist(d123)


#3a


