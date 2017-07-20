##### Classification using Decision Trees and Rules -------------------

#### Part 1: Decision Trees -------------------

# Instalación de Paquetes
install.packages(c("foreing","gmodels","rpart","rpart.plot","RColorBrewer",
                   "partykit","rattle","C50","party","adabag", "ROCR", "pROC", "gplots",
                   "foreach","randomForest","ISLR","tree"),
                 dependencies = c("Depends", "Suggests"))

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
getwd()
setwd('D:/Cursos/18.PEA_ML/0.Data')
credit <- read.csv("DT_R/credit.csv")
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

# compare the credit and credit_rand data frames
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)

# split the data frames
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree


library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

