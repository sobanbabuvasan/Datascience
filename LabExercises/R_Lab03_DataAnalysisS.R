print ("Om Mahaa Ganpataye Namah")
rm(list=ls(all=TRUE))
library(lubridate)
dt = Date()
dt
setwd('D:/dat4')

data <- read.csv(file="orderHist.csv",header=TRUE, sep=",")

library(ggplot2)

#Undertanding data structure
str(data)
head(data)
tail(data)
edit(data)
names(data)
summary(data)

#Missing values handling

sum(is.na(data))#To check null values in data
data2 <- na.omit(data)

# converting int to factors
data$cc <- as.factor(data$cc)
data$cd <- as.factor(data$cd) 
data$securities <- as.factor(data$securities) 
data$online <- as.factor(data$online)
data$loan <- as.factor(data$loan)

# perform data imputation
data3<-knnImputation(data,scale=T,k=5) #KNN Imputation
sum(is.na(data3))
library(vegan)
library(infotheo)
IncBin <- discretize(data3$inc,disc="equalfreq",nbins=6)
tapply(data3$inc,IncBin,mean)

names(data3)
# recoding
data3$ageNew <- NA
for (i in 1:nrow(data3)) {
  if (data3$age[i] >= 45) {
    data3$ageNew[i] = 2
  }
  else {
    data3$ageNew[i] = 1
  }
}
View(data3)
data2$ageNew<-0
for (i in 1:nrow(data2)){
  if (data2$age[i]>=45){ 
    data2$ageNew[i]=2
  }
  else {
    data2$ageNew[i]=1
  }
}

View(data3)
data3$ageNew2 <- NA
for (i in 1:nrow(data3)) {
  if (data3$age[i] <= 45) {
   data3$ageNew2[i] <- 1  
  }
  else {
    if (data3$age[i] > 45 & data3$age[i] <=50) {
      data3$ageNew2[i] <- 2
    }
    else{
      data3$ageNew2[i] <- 3
    }  
  }
}


names(data3)
View(data3)
attach(data)

m <- ggplot(data,aes(x=inc))
m+ geom_histogram()


boxplot(inc~edu,data=data,main='Income by Education',
        xlab='Education', ylab='Income')

p1 <- ggplot(data, aes(factor(edu), inc))
p1 + geom_boxplot()

library(car)
scatterplotMatrix(~inc+edu+cc+cd, data=data, main="Scatter Plot Matrix")


str(mtcars$mpg)
str(data$inc)





# create dummy variables
library(dummies)
data3$edu <- as.integer(data3$edu)
eduVar <- dummy.data.frame(data3,names=c("edu"),sep='.')
str(data3)
View(eduVar)


rm(eduVar)
rm(data4)

names(Cust)
require(reshape2)
data2 <- dcast(Cust,Quarter+Month~Year,fun.aggregate = sum,value.var = 'Cost')
aggregate(Cust$Cost,by=list(Quarter,Month,Year),FUN=sum)
names(Cust)

melted <- melt(Cust[,c(6,4,5)],id=c('Month','ProductID'))
head(melted)

names(melted)