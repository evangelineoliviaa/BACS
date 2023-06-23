setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS")
bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
resamples<-replicate(2000,sample(minday,length(minday),replace = TRUE))
dim(resamples)
# Plot population and original sample densities
plot(density(minday), lwd=0, ylim=c(0, 0.009))
# Draws lines for each sampling mean
plot_resample_density<-function(sample_i) {
  lines(density(sample_i), col=rgb(0.0, 1, 0.0, 0.01))
  return(mean(sample_i))
  }
sample_means<-apply(resamples, 2, FUN=plot_resample_density)
lines(density(minday), lwd=2, lty="dashed")