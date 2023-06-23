setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW7")
require("ggplot2")
require("tidyverse")
Media_1 <- read.csv("pls-media1.csv")
Media_2 <- read.csv("pls-media2.csv")
Media_3 <- read.csv("pls-media3.csv")
Media_4 <- read.csv("pls-media4.csv")

#Question 1

#A) What are the means of viewers’ intentions to share (INTEND.0) on each of the four media types?


m1_mean <- mean(Media_1$INTEND.0)
m2_mean <- mean(Media_2$INTEND.0)
m3_mean <- mean(Media_3$INTEND.0)
m4_mean <- mean(Media_4$INTEND.0)


#B) Visualize the distribution and mean of intention to share, across all four media.
m <-rbind(Media_1,Media_2,Media_3,Media_4)
boxplot(INTEND.0 ~ m$media,
        data = m,
        main = "Distribution Across All Four Media",
        ylab = "Intention.0",
        xlab = "Media",
        horizontal = FALSE)
means <- aggregate(INTEND.0 ~  m$media, m, mean)
points(1:4, means$INTEND.0, col = "red")
means<-round(means,digit=2)
text(1:4, means$INTEND.0 + 0.08, labels = means$INTEND.0)

#C) From the visualization alone, do you feel that media type makes a difference on intention to share?
#I think the media type doesn not really affect the intention to share since the mean of each media is not that far from each other


#Question 2

#A) State the null and alternative hypotheses when comparing INTEND.0 across four groups in ANOVA

#Null Hypothesis: there is no difference in the means of INTEND.0 between the four groups (H0: μ1 = μ2 = μ3 = μ4)
#Alternative Hypothesis : there is a significant difference in the means of INTEND.0 between at least two of the four groups
#(Ha: μ1 ≠ μ2 ≠ μ3 ≠ μ4 or Ha: μ1 = μ2 ≠ μ3 = μ4 or Ha: μ1 ≠ μ2 = μ3 ≠ μ4 or Ha: μ1 = μ2 = μ3 ≠ μ4 or Ha: μ1 ≠ μ2 ≠ μ3 = μ4 )

#B) Let’s compute the F-statistic ourselves:
#i) Show the code and results of computing MSTR, MSE, and F
mbind <- list(Media_1$INTEND.0,Media_2$INTEND.0,Media_3$INTEND.0,Media_4$INTEND.0)
k <- length(mbind)
grandmean<- mean(sapply(mbind,mean))
#MSTR
SSTR<-function(){
  (nrow(Media_1) * (mean(Media_1$INTEND.0) - grandmean)^2)+
  (nrow(Media_2) * (mean(Media_2$INTEND.0) - grandmean)^2)+
  (nrow(Media_3) * (mean(Media_3$INTEND.0) - grandmean)^2)+
  (nrow(Media_4) * (mean(Media_4$INTEND.0) - grandmean)^2)
}
df_mstr<-n-1
mstr<-SSTR()/df_mstr

#MSE
SSE<-function(){
    ((nrow(Media_1)-1) * (sd(Media_1$INTEND.0)^2))+
    ((nrow(Media_2)-1) * (sd(Media_2$INTEND.0)^2))+
    ((nrow(Media_3)-1) * (sd(Media_3$INTEND.0)^2))+
    ((nrow(Media_4)-1) * (sd(Media_4$INTEND.0)^2))
}
nt <- nrow(Media_1)+nrow(Media_2)+nrow(Media_3)+nrow(Media_4)
df_mse <- nt - k
mse <- SSE()/df_mse

f_value <-mstr/mse
f_value

#ii) Compute the p-value of F, from the null F-distribution; is the F-value significant? 
oneway.test(m_new1$INTEND.0 ~ factor(m_new1$media),var.equal = TRUE)

#C) Conduct the same one-way ANOVA using the aov() function in R – confirm that you got similar results.
anova_model <- aov(m_new1$INTEND.0 ~ factor(m_new1$media))
summary(anova_model)

#D) Regardless of your conclusions, conduct a post-hoc Tukey test (feel free to use the TukeyHSD() function included in base R) to see if any pairs of media have significantly different means – what do you find?
TukeyHSD(anova_model, conf.level = 0.05)

#E) Do you feel the classic requirements of one-way ANOVA were met?


#Question 3

#A)State the null and alternative hypotheses

#B)Let’s compute (an approximate) Kruskal Wallis H ourselves (use the formula we saw in class or another formula might have found at a reputable website/book):
#i) Show the code and results of computing H
mkw <- m[1:2]
ranks <- rank(mkw$INTEND.0)
N<-length(ranks)
group_ranks <- split(ranks,f = mkw$media)

sum_ranks <-sapply(group_ranks,sum)
length_ranks<- (sapply(group_ranks,length))
Total<- sum((sum_ranks^2)/length_ranks)

H<- (((12/(N*(N+1))) * Total) - 3*(N+1))
H

#ii) Compute the p-value of H, from the null chi-square distribution; is the H value significant?
# If so, state your conclusion of the hypotheses.
kw_p<-1 -pchisq(H, df=k-1)
kw_p

#Answer : Reject the null hypothesis since the p-value < 0.05

#C) Conduct the same test using the kruskal.wallis() function in R – confirm that you got similar results.
kruskal.test(mkw$INTEND.0 ~ mkw$media, mkw)

#D) Regardless of your conclusions, conduct a post-hoc Dunn test (feel free to use the dunnTest() function from the FSA package) to see if the values of any pairs of media are significantly different – what are your conclusions?
library(FSA)
dunnTest(INTEND.0~media, data = mkw , method = "bonferroni")
