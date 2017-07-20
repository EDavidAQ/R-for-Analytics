
## Curso : Analytics

################################################################################

# Unidad 1: Introducción al R
# Capacitador : Ebson David Allende Quintana
# email: david.allende@outlook.com
# Script Version : 1.0

################################################################################

##### Lab1: Managing and Understanding Data -------------------

## Set work area

setwd("D:/Cursos/14.Promperu/2_Data/")

##### R data structures --------------------

## Vectors -----

# create vectors of data for three medical patients
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

promperu <- c("Exportaciones","Turismo")

# access the second element in body temperature vector
temperature[2]

## examples of accessing items in vector
# include items in the range 2 to 3

temperature[2:3]
temperature[1]

# exclude item 2 using the minus sign
temperature[-2]

# use a vector to indicate whether to include item
temperature[c(TRUE, TRUE, FALSE)]

## Sequence -----

x<-1:100
y<-rep(1,5)

## Factors -----

# add gender factor
gender <- factor(c("MALE", "FEMALE", "MALE"))

gender

# add blood type factor
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))
blood

## Lists -----

# display information for a patient
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]

# create list for a patient
subject1 <- list(fullname = subject_name[1], 
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1])

# display the patient
subject1

## methods for accessing a list

# get a single list value by position
subject1[2]
# get a single list value by name
subject1$temperature

# get several list items by specifying a vector of names
subject1[c("temperature", "flu_status")]

## access a list like a vector
# get values 4 and 5
subject1[4:5]

## Data frames -----

# create a data frame from medical patient data

pt_data <- data.frame(subject_name, temperature, flu_status, gender,
                      blood, stringsAsFactors = FALSE)

# display the data frame
pt_data

## accessing a data frame

# get a single column
pt_data$subject_name

# get several columns by specifying a vector of names
pt_data[c("temperature", "flu_status")]

# this is the same as above, extracting temperature and flu_status
pt_data[2:3]

# accessing by row and column
pt_data[1, 2]

# accessing several rows and several columns using vectors
pt_data[c(1, 3), c(2, 4)]

## Leave a row or column blank to extract all rows or columns

# column 1, all rows
pt_data[, 1]
# row 1, all columns
pt_data[1, ]
# all rows and all columns
pt_data[ , ]

# the following are equivalent to the above
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5)]
pt_data[-2, -c(1,3,5)]

## Matrixes -----

# create a 2x2 matrix
m <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)
m

# equivalent to the above
m <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)
m

# create a 2x3 matrix
m <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 2)
m

# create a 3x2 matrix
m <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), ncol = 2)
m

# extract values from matrixes
m[1, 1]
m[3, 2]

# extract rows
m[1, ]

# extract columns
m[, 1]

##### Exploring and understanding data --------------------

## data exploration example using used car data

setwd("D:/Cursos/14.Promperu/2_Data")
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

# get structure of used car data
str(usedcars)

## Exploring numeric variables -----

# summarize numeric variables
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])

# calculate the mean income
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))

# the median income
median(c(36000, 44000, 56000))

# the min/max of used car prices
x<-range(usedcars$price)

# the difference of the range
diff(range(usedcars$price))
diff(x)

# IQR for used car prices
IQR(usedcars$price)

# use quantile to calculate five-number summary
quantile(usedcars$price)

# the 99th percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# boxplot of used car prices and mileage
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")

boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
      ylab="Odometer (mi.)")

# histograms of used car prices and mileage
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# variance and standard deviation of the used car data
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring numeric variables -----

# one-way tables for the used car data
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# compute table proportions
model_table <- table(usedcars$model)
prop.table(model_table)

# round the data
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploring relationships between variables -----

# scatterplot of price vs. mileage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

# new variable indicating conservative colors

setwd("D:/Cursos/14.Promperu/2_Data")
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

colnames(usedcars)
table(usedcars$color)
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")
head(usedcars)
ls()

# checking our variable
table(usedcars$conservative)



# Crosstab of conservative by model
#install.packages("gmodels")
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)

## Basic Programming -----

# IF

if(1>2) {
  cat("One is the most biggest number\n")  # Say something wrong.
} else {
  cat("One is the loneliest number\n")     # Say something less wrong
}

# FOR

for(lupe in seq(1,2,by=0.33))
{
  cat("The value of lupe is ",lupe,"\n")
}

# WHILE

lupe <- 1.0;
while(lupe <= 2.0)
{
  cat("The value of lupe is ",lupe,"\n")
  lupe <- lupe + 0.33
}

# FUNCTION

promperu<-function(x)
{
  y<-x+5
  print(y)
}


promperu(6)
promperu(10)

updatePosition <- function(currentPos,angle,stdDev)
{
  angle <- angle + rnorm(1,0,stdDev)
  list(newPos=currentPos + exp(angle*1.0i),
       newAngle=angle)
}

pos <- updatePosition(2.0,0.0,1.0)
pos
pos$newPos

## Read Data ----- 

# Read Text data

data_jefaturas <- read.table(file = "Jefaturas.txt", 
                             header = TRUE)
head(data_jefaturas)

data_BwBank<- read.delim("BwBankTab.dat", header = TRUE, sep = "\t") 
head(data_BwBank) #delimitados

# Read Excel data

install.packages("devtools")
devtools::install_github("hadley/readxl")

library(readxl) # xls and xlsx files
data_BwBank2<-read_excel("BwBank.xls")
head(data_BwBank2)

data_BwBank3<-read_excel("BwBank.xls", sheet =
                           "BWBANK") 

data_BwBank4<-read_excel("BwBank.xls", sheet = 1)

GSS2004Sub_es<-read_excel("GSS2004Sub_es.xlsx") # Formato xlsx

# Read SPSS Data

library(foreign)

data_hijos <- read.spss("Data_Hijos_VF1.sav", to.data.frame = TRUE)

head(data_hijos)

data_hijos[1:5, c(1,2,3,10,15)]

library(Hmisc)

BankSatisfaction <- spss.get('BankSatisfaction.sav')
head(BankSatisfaction)

# Read data from a library

install.packages("agricolae")
library(agricolae)

data(package="agricolae")
data("yacon")
head(yacon)

# Export datasets from R

write.csv(yacon, 'yacon.csv')
getwd()

write.table(yacon, 'yacon.txt', sep='|')

write.foreign(data_BwBank, 'data_BwBank.txt', 'Code_spss.sps', package='SPSS')

library(rattle)
rattle()


















