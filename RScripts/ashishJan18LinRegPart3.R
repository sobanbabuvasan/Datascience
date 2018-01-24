print ("Om Mahaa Ganapataye Nama")
ls()
rm(list=ls(all=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Import the libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(corrgram)
library(mice)  # data imputation
library(dummies)  # dummy variable creation


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Import the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

housing.df <- read.csv("D:/dat1/housingdata.csv", sep = ",")
str(housing.df)
head(housing.df)
dim(housing.df)
summary(housing.df)
attach(housing.df)


#outlier plots
par(mfrow=c(2,7))
list<-names(housing.df)
# list<-list[-4]
for(i in 1:length(list))
{
  boxplot(housing.df[,list[i]],main=list[i], horizontal = TRUE)
}


dev.off()

#Outlier Treatment
for(i in 1:length(list))
{
  x<-boxplot(housing.df[,list[i]],main=list[i])
  out<-x$out
  index<-which(housing.df[,list[i]]%in% x$out)
  housing.df[index,list[i]]<-mean(housing.df[,list[i]])
  rm(x)
  rm(out)
}
for(i in 1:length(list))
{
  y<-boxplot(housing.df[,list[i]],main=list[i])
}
y

for(i in 1:length(list))
{
  plot(housing.df[,list[i]],main=list[i])
}

str(housing.df)
#Exploratory Analysis
library(ggplot2)
hist(housing.df$price,breaks=6,label=TRUE,xlim=c(10,35))

#Function to get the correlation between the IDV's and DV.
list
list1<-list[-13]
for(i in 1:length(list1))
{
  x<-cor(housing.df$price,housing.df[list[i]])
  print(x)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #     create train and test split (70/30)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set.seed(90)  # to reproduce the same result
id <- createDataPartition(housing.df$price, p=0.7,list=FALSE)
id
train <- housing.df[id,]
dim(train)
sum(train$price)

test <- housing.df[-id,]
dim(test)
sum(test$price)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     fit the model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pairs(train)
corrgram(housing.df)
# lm(output variable ~ independent variable 1 + independent variable 2 )
set.seed(90) # reproduce the same result
# model 1 
fit1 <- lm(price~size,data=train)
summary(fit1)
#lines(sort(train$uds),fit1$fit[order(train$uds)],col='red')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Polynomial Regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ffit1 <- lm(price~poly(uds,2),data=train)
# summary(ffit1)
# lines(sort(train$uds),ffit1$fit[order(train$uds)],col='red')

# predict
test$pred <- predict(fit1,newdata = test)

# model 1 RMSE 
SSE1 = sum(pred - test$price)^2
MSE1 = mean(test$pred - test$price)^2
rmse = sqrt(MSE1)


# model 1 
fit <- lm(price~.,data=train)
summary(fit)

# stepwise regression - perform the feature selection
step(fit,direction=c('both'))

? step()
# model 2 
fit11 <- lm(price~size+uds,data=train)
summary(fit11)

test$pred11 <- predict(fit11,newdata = test)



# model 2 RMSE 
SSE = sum(test$price - test$pred11 )^2
MSE = mean(test$price - test$pred11)^2
rmse1 = sqrt(MSE)


# model 2 
fit2 <- lm(price~uds+size+park,data=train)
pred1 <- predict(fit2,newdata = test)
pred1

# model 2 RMSE 
SSE2 = sum(pred1 - test$price)^2
MSE = mean(pred1 - test$price)^2
rmse2 = sqrt(mean(pred1 - test$price)^2)

plot(uds,price)
abline(fit1)
summary(fit1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# validate Linear regrssion assumptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# linearity  - all the indep variables should be linearly correlated with 
# the target outcome variable
library(car)

crPlots(fit11)

# residuals should be normally distributed
plot(fit11$residuals)

# Homoscadasticity
library(lmtest)
bptest(fit11)


# Multicollinearity
vif(fit) # variable inflation factor

fit111 <- lm(price~size+uds+park)
summary(fit111)


fit112 <- lm(price~size+uds+misc)
summary(fit111)


# autocorrelation
durbinWatsonTest(fit11)

plot(fit11)
