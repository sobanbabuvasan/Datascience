rm(list=ls(all=TRUE))

library(ggplot2)

mkt <- read.csv("D:/dat1/retailMarketing.csv",sep=',')
str(mkt)

mkt <- na.omit(mkt)
sum(is.na(mkt))
# data exploration
attach(mkt)

color1 <- c('Red','Blue')
boxplot(AmountSpent ~ factor(Married, level= c('Married','Single')),
                             data = mkt, xlab = 'Married',
                             ylab = 'AmountSpent'
                             )

boxplot(AmountSpent ~ factor(Age, level= c('Old','Middle','Young')),
        data = mkt, xlab = 'Age',
        ylab = 'AmountSpent')


boxplot(AmountSpent ~ factor(Location, level= c('Far','Close')),
        data = mkt, xlab = 'Age',
        ylab = 'AmountSpent')


boxplot(AmountSpent ~ factor(Children, level= c("0","1","2","3")),
        data = mkt, xlab = 'Age',
        ylab = 'AmountSpent')


boxplot(AmountSpent ~ factor(History, level= c("High","Medium","Low")),
        data = mkt, xlab = 'Age',
        ylab = 'AmountSpent')

plot(AmountSpent~ Salary, data = mkt)


library(sm)

cdplot(Gender~AmountSpent, data=mkt, col=color1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for any categorical variable with n classes there are always n-1 dummy 
# variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

flag=1
noflag=0

View(mkt)
mkt$Old <- ifelse(mkt$Age == 'Old',flag,noflag)
mkt$Middle <- ifelse(mkt$Age == 'Middle',flag,noflag)
mkt$Male <- ifelse(mkt$Gender == 'Male',flag,noflag)
mkt$Own <- ifelse(mkt$Own == 'Own',flag,noflag)
mkt$Single <- ifelse(mkt$Own == 'Single',flag,noflag)
mkt$HighHistory <- ifelse(mkt$History == 'High',flag,noflag)
mkt$LowHistory <- ifelse(mkt$History == 'Low',flag,noflag)
mkt$Close <- ifelse(mkt$Location == 'Close',flag,noflag)

mkt <- mkt[,-8]
str(mkt)
mkt <- mkt[,6:17]
mkt <- na.omit(mkt)
# train the model
fit <- lm(AmountSpent ~., data = mkt)
summary(fit)

step(fit,direction = c("both"))

fit2 <- lm(formula = AmountSpent ~ Salary + Children + Catalogs + Male + 
     HighHistory + LowHistory + Close, data = mkt)

summary(fit2)


