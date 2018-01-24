rm(list=ls(all=TRUE))

library(car)
data("mtcars")
head(mtcars)
str(mtcars)

str(mtcars[,-c(11)])

cardf <- mtcars[,-c(11)]
str(cardf)

View(cardf)

setwd('D:/dat4')
Score <- read.csv('Score1.csv',header = T, sep = ',')
Score2 <- read.csv('Score2.csv',header = T, sep = ',')

View(Score)
str(Score)

Score$grade <- NA
for(i in 1:nrow(Score)) {
  if( Score$OverallPct1[i] < 40) {
    Score$grade[i] <- "C"  
  } else {
        if(Score$OverallPct1[i] < 60) {
          Score$grade[i] <- "B"  
        } else {
          Score$grade[i] <- "A"
        }
      }
  }

# drop the column 
Score$grade1 <- NULL
# create or bin the data 
Score$grade <- ifelse(Score$OverallPct1 < 40,'C',
                        ifelse(Score$OverallPct1 < 60,'B',
                               'A'))

Score$grade3 <- ifelse(Score$OverallPct1 < 40 | Score$Math1 <60
                      ,'C',
                      ifelse(Score$OverallPct1 < 60 | Score$Math1,'B',
                             'A'))

View(Score)

attach(mtcars)
data <- mtcars
A <- apply(data[,2:11],2,min)
A
str(data)

B <- apply(data[,2:11],2,max)
B

desStat <- function(x) {
  Mn <- mean(x)
  min1 <- min(x)
  max1 <- max(x)
  sd1 <- sd(x)
  df <- data.frame(min1,max1,Mn,sd1)
  return(df)
}
# 1 is row wise operation
# 2 is column wise operation
stats <- apply(data[,2:11],2,FUN = desStat)
res <- do.call(rbind,stats)
str(res)
View(data)

lappy <- lapply(data[,2:11],mean)
class(lappy)

tappy <- tapply(mtcars$mpg,mtcars$cyl,mean)

unique(mtcars$cyl)

data[which.max(mpg),]

data[which.min(mpg),]

which.min(data$mpg)

stats <- apply(data,2,max)
str(data)

data[row.names(data) %in% c("Mazda RX4", "Datsun 710"),]

name <- c('Raj','Bala','Vijay')
sal <- c(1000,2000,300)
city <- c('Bengaluru','Pune','Mumbai')
df1 <- data.frame(name,sal,city)

row.names(df1) <- df1$city

View(Score)
mergeScore <- merge(Score,Score2)
View(mergeScore)

mergeScore1 <- merge(Score,Score2)
View(mergeScore1)

temp <- data.frame(x=1:10, y = c(T,T,F,F,F,F,NA,NA,T,T),
                        z = c(NA,NA,7,8,9,5,11,9,9,4) )

y1 = c(T,T,F,F,F,F,NA,NA,T,T)
y2 <- na.omit(y1)
y1 <- data.frame(y1)
y1[! is.na(y1),]
? is.na
anyNA(temp)
summary(is.na(temp))

library(DMwR)
temp2 <- knnImputation(temp,scale=T,k=1)
temp2

