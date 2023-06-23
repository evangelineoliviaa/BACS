library(seminr)
library(DiagrammeR)
setwd("/Users/olivia/Documents/Documents/Study/Semester 6/BACS/HW15")
security <- read.csv("security_data_sem.csv")
#Part A

#Part I 
security_mm <- constructs(
  composite("REP", multi_items("PREP", 1:4)),
  composite("INV", multi_items("PINV", 1:3)),
  composite("SEC", multi_items("PSEC", 1:4)),
  composite("TRUST", multi_items("TRST", 1:4)),
  composite("POL", multi_items("PPSS", 1:3)),
  composite("FAML",single_item("FAML1")),
  interaction_term(iv = 'REP', moderator = 'POL', method = orthogonal)
)

#Part II
security_sm <- relationships(
  paths(from = c("REP", "INV","POL","FAML","REP*POL"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)


#Part B

#Part I
security_pls <- estimate_pls(data = security,
                        measurement_model = security_mm,
                        structural_model = security_sm)
sec_report<-summary(security_pls)

plot(security_pls,title = "PLS Model")
save_plot("myfigure.png")

#Part II
sec_report$weights

sec_report$composite_scores

#Part III

sec_report$paths

#Part IV

boot_pls <- bootstrap_model(security_pls, nboot = 1000)
summary(boot_pls)


#Question 2

#Part A
#Part I
security_cf_mm <- constructs(
  reflective("REP", multi_items("PREP", 1:4)),
  reflective("INV", multi_items("PINV", 1:3)),
  reflective("SEC", multi_items("PSEC", 1:4)),
  reflective("TRUST", multi_items("TRST", 1:4)),
  reflective("POL", multi_items("PPSS", 1:3)),
  reflective("FAML",single_item("FAML1")),
  interaction_term(iv = 'REP', moderator = 'POL', method = orthogonal)
)

#Part II
security_cf_sm <- relationships(
  paths(from = c("REP", "INV","POL","FAML","REP*POL"), to = "SEC"),
  paths(from = "SEC", to = "TRUST")
)

#Part B

#Part I
security_cf_pls <- estimate_cbsem(data = security,
                                  measurement_model = security_cf_mm,
                                  structural_model = security_cf_sm)
sec_cf_report<-summary(security_cf_pls)
plot(security_cf_pls,title = "CF PLS Model")
save_plot("myfigure2.png")

#Part II
sec_cf_report$loadings

#Part III
sec_cf_report$paths

