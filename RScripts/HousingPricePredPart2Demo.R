m_

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Title  : Predicting housing price 
#           Author : Sasken     
#           Date   : 07-Jan-2017
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# clean the workspace
ls()
rm(list=ls(all=TRUE))
   
   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import the data set 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
housingdf <- read.csv("D:/dat1/HousingS22Dec.csv",sep=",")
str(housingdf)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import the Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(Amelia)
library(mice)
library(dummies)
library(randomForest)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform Data Exploration and pre processing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using summary we can view the 5 number summary
# for all the features 
summary(housingdf)
options(width=100)
head(housingdf)
class(housingdf$price)


# rename the X12size column name as size 
colnames(housingdf)[1]  <- "size"
names(housingdf)[1] <- "size"

library(dplyr)
comma <- housingdf %>% filter(size==",")

#Detect pattern from the character and find out the rows which has special characters (,) 

grep(",", housingdf$size)
grep(",", housingdf$price)

head(housingdf)

#  remove the special character @ from State
housingdf$state <- gsub("#","",housingdf$state)
head(housingdf)

# Display the number of observations and the features 
dim(housingdf)

# Remove commas from the size and price features
housingdf$size <- gsub(",","",housingdf$size)
housingdf$price <- gsub(",","",housingdf$price)
head(housingdf)

str(housingdf)

# Remove space and punctuation from all the column names 
library(stringr)
colnames(housingdf) <- str_replace_all(colnames(housingdf),
                                       "[:punct:]|[:space:]","")

#Convert the size and price columns as numeric data type
housingdf$size <- as.numeric(housingdf$size)
housingdf$price <- as.numeric(housingdf$price)
str(housingdf)
head(housingdf)

# Check if there are any duplicate observations presence
housingdf[duplicated(housingdf),]

# View(housingdf)
ind <- which(duplicated(housingdf)) 
housingdf <- housingdf[-ind,]
dim(housingdf)
str(housingdf)

# to check the no. of NA observations
nmiss <- sapply(housingdf,function(x) {sum(is.na(x))})
print(nmiss)

# to check the no. of blank records 
nnull <- sapply(housingdf,function(x) {sum(x =="")})
str(housingdf)
head(housingdf)
dft <- housingdf

dft <- dft[, -which(sapply(dft,function(x) {sum(is.na(x))}) > 5)]

dft1 <- dft[, -which(sapply(dft,function(x) {sum(x == "")}) > 5)]

housingdf <- dft1
rm(dft)
rm(dft1)
str(housingdf)
head(housingdf)

# discard the redundant and irrelevant features
housingdf <- housingdf[,-c(6,7,9)]
str(housingdf)
head(housingdf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dummy indicator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

housingdf <- dummy.data.frame(housingdf,names = c("state"),sep ="." )
rm(housingdf1)
str(housingdf)
# View(housingdf)

library(dplyr)
housingdf %>% filter(price > 20) %>% nrow()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Perform Data Imputation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(mice)
dataImput <- mice(housingdf,m=4,method = "pmm",maxit = 50)
completed <- complete(dataImput,1)
housingdf1 <- completed
head(housingdf1)
housingdf <- housingdf1
summary(housingdf1)
head(housingdf)
str(housingdf)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       Create train & test split
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ind <- createDataPartition( housingdf$price,p=0.6,list = FALSE)

train <- housingdf[ind,]
dim(train)
test <- housingdf[-ind,]
dim(test)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       Fit the model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(train)

# features = c("size","uds+park+misc+state.california+state.Pennsylvania)

lmFit <- train(price ~ size+uds+park+state.California+state.Pennsylvania, data = train,method = "lm")
summary(lmFit)

options(width = 100)

coefficients(lmFit)



lmFit1 <- lm(price ~ size+uds+park+misc+state.California, data = train)
summary(lmFit1)
plot(fitted(lmFit),residuals(lmFit), xlab = "Fitted")
abline(h=0)

lmFit$coefnames

sqrt(mean((pred - test$price)^2))

test$lmpred <- predict(lmFit, newdata = test)
head(test)
View(test)
sqrt(mean(test$lmpred - test$price)^2)
str(train)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       Model Selection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Linear Vs Ridge Regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(825)
fitControl <- trainControl(method = "cv",
                           number = 10)

lmfit <-  train(price ~ size+uds+park, data = train,
                method='lm',
                # lambda = seq(0.001, .05, .001),
                trControl = fitControl
)

lmpred <- predict(lmfit,test)
sqrt(mean(lmpred - test$price)^2)

ridge <-  train(price ~ size+uds+park, data = train,
                method='ridge',
                # lambda = seq(0.001, .05, .001),
                trControl = fitControl
                    )
ridge$bestTune
# compute coefficients
predict(ridge$finalModel,type="coef",mode='norm')$coefficients[4,]

rpred <- predict(ridge,test)
sqrt(mean(rpred - test$price)^2)




rm(list=ls(all=TRUE))

hdf <- read.csv('D:/dat1/HousingData.csv',sep=',')
str(hdf)

summary(hdf)


train <-hdf[1:120,]
test <- hdf[121:148,]

plot(hdf$size,hdf$price)

# H0: beta1 (Size) = beta2 = 0
# Ha:  
str(train)
head(train[,c(1,4)])
head(test[,c(1)])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lmfit <- lm(price~size,data=train)
summary(lmfit)
# fitted values for the training data.
lmfit$fitted.values

pred <- predict(lmfit,newdata=data.frame(size=test$size), 
                interval = "confidence")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lmfit2 <- lm(price~size+uds,data=train[,c(1,2,4)])
summary(lmfit2)

pred2 <- predict(lmfit2,test, 
                interval = "confidence")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

str(test)
test$pred <- NULL
test$pred <- predict(lmfit,newdata=test)
View(test)
write.csv(test,'D:/dat1/predHousing.csv')

print(pred)

abline(lmfit)
summary(lmfit)



library(car)
outlierTest(lmfit)

vcov(lmfit)
# in statistics it is standard to work with 95% 
# confidence intervals, which means we are 95% 
# certain the true value lies within our interval 
confint(lmfit,conf.level=0.95)
? confint
# H0: beta coefficient = 0 (independent variable has no impact on 
# the dependent variable)
pred <- predict(lmfit,newdata=test)
print(pred)

par(mfrow(c(2,2)))
plot(lmfit)

plot(test$price,col="blue",type="l")
lines(pred,col="red",type="l")

# Regression Assumptions
#1. Linearity

#3. Variation of observations around the regression line (the residual SE)
#   is constant (homoscadasticity)
# 4. For a given value of x, y values are normally distributed


