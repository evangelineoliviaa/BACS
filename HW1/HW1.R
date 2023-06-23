setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS")
data<-read.table(file="customers.txt",header=TRUE)
require(dplyr)
#1
data[5,]

#2
temp<-unique(arrange(data,age))
temp[5,]

#3
temp[1:5,]

#4
temp2<-sort(unique(data$age),decreasing = TRUE)
temp2[1:5]

#5
mn<-mean(data$age)
mn
#6
sd(data$age)

#7
data$age_diff<-abs(data$age-mn)
data

#8
av<-mean(data$age_diff)
av

#9a
hist(data$age)

#9b
d<-density(data$age)
plot(d)

#9c
boxplot(data$age, horizontal = TRUE)
stripchart(data$age, method = "stack", add = TRUE)
