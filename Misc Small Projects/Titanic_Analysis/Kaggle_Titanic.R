# Set working directory and import datafiles
setwd("~/Google Drive/Academics (Tomer)/0-2017 Buenos Aires Sem2.2/CS112 DataScience/R_CS112")

#devide dataset into training and test set
train <- read.csv("train.titanic.csv",stringsAsFactors=FALSE)
test <- read.csv("test.titanic.csv", stringsAsFactors=FALSE)

#look at the structure of the dataframe, ie the types of variables that were loaded.
str(train)

#table(): summary of a column -  counts the occurrence of each value in it
table(train$Survived)
#counts occurences as a proportion from the occureneces in the column
prop.table(table(train$Survived))

#start precidting!
#rep: repeats something by the # of times given
#mentionning a table$NewColumn = will CREATE that column for us, but If this column already existed, it would overwrite it with the new values, so be careful! 
test$Survived <- rep(0, 418)

#create a dataframe wiht specific columns (and name titles)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

#write csv
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)


#the proportion table command by default takes each entry in the table and divides by the total number of passengers. What we want to see is the row-wise proportion, ie, the proportion of each sex that survived, as separate groups. So we need to tell the command to give us proportions in the 1st dimension which stands for the rows (using ???2??? instead would give you column proportions)
prop.table(table(train$Sex, train$Survived))
#get row-wise relations: ,1 (or column-wise: ,2)
prop.table(table(train$Sex, train$Survived),1)

#add new column of 0s
test$Survived <- 0
# alter that same column with 1???s for the subset of passengers where the variable ???Sex??? is equal to ???female???. ( equality operator: == "is equal?")
test$Survived[test$Sex == 'female'] <- 1

#see summarysummary:distribution of numerical values in a column
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1

#The aggregate command takes a formula with the target variable on the left hand side of the tilde symbol and the variables to subset over on the right. We then tell it which dataframe to look at with the data argument, and finally what function to apply to these subsets.
#subsetting the whole dataframe over the different possible combinations of the age and gender variables 
#how many entries do we have in each subset?
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
#subset and applies the sum function to the Survived vector for each of these subsets.
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
#percentage
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#bin the fares into less than $10, between $10 and $20, $20 to $30 and more than $30 and store it to a new variable:
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#aggregate
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#finding that higher fare subset was more deathly

#make a new predcition
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

###### decision trees ######
library(rpart)
#fit a regression tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class")
plot(fit) ; text(fit)

#INSTALL GREAT PACKAGES FOR GRAPHICS FOR RPART
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
