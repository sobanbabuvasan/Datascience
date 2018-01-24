###########clearing environment############################################################
rm(list=c("m","x","y"))
rm(list = ls())


###########Reading Files into R ############################################################
setwd("D:\\dat4")

##Reading the csv files and RData files into R environment
Score<-read.csv("Score1.csv",header=T,sep=",")

##Reading other formats we use read.table command
# read<-read.table("C:\\Users\\Satya\\Desktop\\20160107_Batch24_CSE7112c_R_Lab_Day\\INPUTS\\greek.txt",sep="\t",header=T)


####Reading Excel files  ##There might be an RJava issue
#install.packages("XLConnect")
require(XLConnect) #library(XLConnect) # To loas an add-on package
wb<-loadWorkbook("Book3.xlsx")
sheet<-readWorksheet(wb,"Sheet1",header=T)

################ Writing Output Files #########################################################################

#### writing into a CSV file
write.csv(Score,"data.csv", row.names=F)

#### writing into Excel
library(xlsx)
write.xlsx(sheet,"D:\\dat4\\write.xlsx",row.names=F)


####### Writing condition Statements ########################################################################################

#### if Conditional Statement

"
if(boolean_expression) {
   // statement(s) will execute if the boolean expression is true.
}

"

### Example
x <- 30
if(is.numeric(x)) {
  print("X is an numeric")
}

#### if-else Conditional Statement

"
if(boolean_expression) {
   // statement(s) will execute if the boolean expression is true.
} else {
// statement(s) will execute if the boolean expression is false.
}

"

### Example
x <- c("what","is","truth")

if("Truth" %in% x) {
  print("Truth is found")
} else {
  print("Truth is not found")
}

######## Writing Loops ##############################################################################################################

###For Loop
"
for (value in vector) {
  statements
}
"

v <- LETTERS[1:4]

for ( i in v) {
  print(i)
}

##### While loop
"
while (test_expression) {
   statement
}
"

v <- c("Hello","while loop")
cnt <- 0

while (cnt < 4) {
  print(v)
  cnt = cnt + 1
}



"For example I want to categorize the students into 3 bins based 
on the overall percentage as follows
if pct < 40 then C, if 40 < pct < 60,B and if greater than 60 then A
"
##Using for loop
head(Score)
Score$class<-NA
for(i in 1:nrow(Score)){
  if(Score$OverallPct1[i]<40){
    Score$class[i]<-"C"
  }else{
    if(Score$OverallPct1[i]<60){
      Score$class[i]<-"B"
    }else{
      Score$class[i]<-"A"
    }
  }
}



View(Score)


###Doing the same using ifelse
Score$Class1<-ifelse(Score$OverallPct1<40,"C",
                     ifelse(Score$OverallPct1<60,"B","A"))

##Again Giving multiple conditions in the if or ifelse statements
Score$class2<-ifelse(Score$OverallPct1<40 | Score$Math1<60, "C",
                     ifelse (Score$OverallPct1<60 |Score$Math1<80,"B","A")) ##for "and" use "&"


####### Writing function in R #################################################################################

"
function_name <- function(arg_1, arg_2, ...) {
   Function body 
}
"

#Built-in
#To calculate sum of numbers in R
v<-c(1,2,3,4,5)
sum(v) #Output/return the sum of the numbers in v
mean(v) #returns the average value of the numbers in v

##Custom functions
#To write a customized function for squaring
square<-function(x){
  return(x^2)
}
#Try out the following and observe the output
square(FALSE)
v<-c(1,2,3,4,5)
square(v)
y<-data.frame(A=c(1,2,3,4),B=c(5,6,7,8))
square(y)


##################################Some useful functions used in data manipulations##################
#Apply
##
attach(mtcars)
data<-mtcars
##Want to find max value for each column
apply(data,2,min) #This generates the min values for each numeric attribute

apply(data,1,max)
##writing this to a data frame
A<-apply(data[,2:11],2,min)
A<-data.frame(min=apply(data[,2:11],2,min))
B<-apply(data[,2:11],2,max)
##We can find the stats for each of the variable separately

##If we want to have all the stats in a data frame we can write a customize function for this
stat<-function(x){
  "Mean"=mean(x)
  "Min"=min(x)
  "Max"=max(x)
  A<-data.frame(Min,Mean,Max)
  return(A)
}
stats<-apply(data[,2:11],2,FUN=stat) ##Observe the ouptput of apply.. it is a list

# do.call takes a function as input and splatters its other arguments to the function. 
# It is widely used, for example, to assemble lists into simpler structures 
# (often with rbind or cbind).

result<-do.call(rbind,stats)

x <- lapply(data,class)
do.call(c,x)

#lapply
##to use apply on a vector and return a list 

lappy<-lapply(data[,2:11],mean)   
#tapply-- gives a table wrt to a categorical attribute

table(mtcars$mpg,mtcars$cyl)

tappy<-tapply(mtcars$mpg,mtcars$cyl,mean) # takes one function and gives the values and not a dataframe
tappy ##This gives out the mean mileage for each cylinder types


#check tapply, lapply, table#

#########################################Subsetting###############################
##This might form an important aspect in Data analysis where we might want to work on a subset of data

##Subset on vectors
v<-c(1,2,3,4,5)
v[v>3]  #Output all elements greater than 3

##Subset on matrices and data frames
#a. Calling by cell positions
library(dplyr)

data1<-data[,2:11]
data1<-data[1:10,2:11]

#b. By using column names
data1<-data[,c("mpg","cyl")]

name<-c("mpg","cyl","disp","hp")
data1<-data[names(data)%in% name] ## %in% comes in handy for subsetting

select(data,mpg,cyl,disp,hp)

#c. Using a subset function ##from help identify the argument to be given
data1<-subset(data,mpg>25,select=mpg:carb) #From data extracts all the records whose mpg>25 and all columns

#d. The same dataframe can be obtained in another way
data1<-data[mpg>25,]

#e. The same dataframe can be obtained in another way using dplyr
x<-filter(data,mpg>25)
x
x <- 1:100
filter(x, rep(1, 3))
# check filter

##Multiple conditions can be given using "&" or "|"
data2<-data[mpg>25 & hp>75,]
data2<-subset(data,mpg>25 | gear==5,select=mpg:carb)
filter(data,mpg>25 | gear==5)

##Using which.max
data[which.max(mpg),]

##Using which.min
data[which.min(mpg),]

##Using which
data[which(data$mpg==max(data$mpg)),]
data[which(row.names(data) %in% c("Mazda RX4","Datsun 710")),]


