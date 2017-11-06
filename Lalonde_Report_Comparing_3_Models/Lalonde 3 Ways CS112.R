###### DATA PREP ######
#setting the workspace, libraries, data
setwd("~/Google Drive/Academics (Tomer)/0-2017 Buenos Aires Sem2.2/CS112 DataScience/R_CS112")
library(Matching)
data("lalonde")
lalonde
#head(lalonde)
#View(lalonde)
#summary(lalonde)
attach(lalonde)


##### 1. MULTIVARIATE REGRESSION ######
#multivariate regression of real earnings 
lm2 <- lm(re78 ~ treat + educ + re75 + age + black + hisp + nodegr + u74 + u75 + married, data=lalonde) 
head(lalonde)
summary(lm2)
coefficients(lm2)

#CONFIDENCE INTERVALS
confint(lm3,level = 0.95)

#Are estimated treatment effects higher for individuals with (or without) a high school degree? Explain.

#split dataset by degree and nondegree
data_degree <- subset(lalonde, nodegr == 0) 
head(data_degree)
data_nodegree <- subset(lalonde, nodegr == 1) 
#head(data_nodegree)

#fit regressions for degree or no degree
lm_nodgr <- lm(re78 ~ treat + educ + re75 + age + black + hisp + nodegr + u74 + u75 + married, data = data_nodegree) 
lm_dgr <- lm(re78 ~ treat + educ + re75 + age + black + hisp + nodegr + u74 + u75 + married, data = data_degree) 

#view regression results
summary(lm_nodgr)
summary(lm_dgr)
coefficients(lm_nodgr)
coefficients(lm_dgr)
confint(lm_nodgr)
confint(lm_dgr)

###### 2. RANDOM FOREST REGRESSION ######
library(randomForest)

#2 fit random forest model, fitting 78 onto the same predictors as before
forest <- randomForest(re78 ~ treat + educ + re75 + age + black + hisp + nodegr + married, data = lalonde, importance=TRUE, ntree=500)
summary(forest)
print(forest)
plot(forest)
#2a - predictor importanct table
importance(forest)
varImpPlot(forest)
plot( importance(forest), lty=2, pch=16)

###2b - predict treatment effect for the treatment units###

# create dataset of only treated units, without re78
data_treated <- lalonde[treat == 1,]
data_treated_cf <- lalonde[treat == 1,] 
#dim(data_treated)
#head(data_treated_cf)

#predict counterfactuals
#change all "treated = 1" to 0 (control)
data_treated_cf$treat <- 0
head(data_treated_cf)

#use predict function, basing on the forest model fitted upon the entire dataset, what would be the results of 
data_treated_cf
predict(forest, data_treated_cf)
rf_predicted_cf <- predict(forest, data_treated_cf)
mean(data_treated_cf$re78 - rf_predicted_cf)
head(rf_predicted)
print(sort(rf_predicted))
hist(rf_predicted)
plot(density(rf_predicted))
summary(rf_predicted)
head(sort(rf_predicted))
mean(rf_predicted) # = 5507.349


mean(data_treated$re78)
mean(rf_predicted_cf)
#treatment_effect_for_treated:
mean(data_treated$re78) - mean(rf_predicted_cf)


####### 2.g. Degree / vs NoDegree Random Forest ######

# create dataset of only treated units, without re78
head(data_treated)
head(data_treated_cf)


treated_yesdegree <- subset(data_treated, nodegr == 0) 
head(data_degree)
treated_nodegree <- subset(data_treated, nodegr == 1) 
head(data_nodegree)


#predict counterfactuals:
#create new datasets that will w=contain counterfactuals
treated_yesdg_cf <- subset(treated_yesdegree, nodegr == 0) 
treated_nodg_cf <- subset(data_treated, nodegr == 1) 

#change all "treated = 1" to 0 (control)
treated_yesdg_cf$treat <- 0
treated_nodg_cf$treat <- 0


#use predict function, basing on the forest model fitted upon the entire dataset, what would be the results of 
predicted_re78_yesdg_treated_cf <- predict(forest,data=treated_yesdg_cf)
predicted_re78_nodg_treated_cf <- predict(forest, data=treated_nodg_cf)

## WITH DEGREE COMPARISON
#treatment effect (only treated)
mean(treated_yesdegree$re78)
mean(predicted_re78_yesdg_treated_cf)
mean(treated_yesdegree$re78) - mean(predicted_re78_yesdg_treated_cf)
#plot WITH DEGREE counterfactual comparison
plot(density(treated_yesdegree$re78), xlim = c(-11000,46000),ylim = c(0,0.00015), col="green", main = "Density of earnings for people WITH a degree", ylab = "Density", xlab = "Earnings 78")
lines(density(predicted_re78_yesdg_treated_cf),col="red")
legend(x=38000, y=0.00015, xjust=1, col = c("green","red"), legend = c("true earnings for treated", "predicted earnings if not treated"),  bty="n", pch= c(16, 16) )

## WITH NO DEGREE COMPARISON
#treatment effect
mean(treated_nodegree$re78)
mean(predicted_re78_nodg_treated_cf)
mean(treated_nodegree$re78) - mean(predicted_re78_nodg_treated_cf)
#plot WITHOUT DEGREE counterfactual comparison
plot(density(treated_nodegree$re78), xlim = c(-11000,46000),ylim = c(0,0.00015), col="blue", main = "Density of earnings for people with NO degree", ylab = "Density", xlab = "Earnings 78")
lines(density(predicted_re78_nodg_treated_cf),col="orange")
legend(x=38000, y=0.00015, xjust=1, col = c("blue","orange"), legend = c("true earnings for treated", "predicted earnings if not treated"),  bty="n", pch= c(16, 16) )






########## 3 ########
storage.vector



plot
abline(v=-794)