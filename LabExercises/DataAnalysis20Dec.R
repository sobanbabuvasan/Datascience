rm(list =ls(all=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import all the required packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages('dplyr')
# install.packages('ggplot2')
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(DT)
library(knitr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# import the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("D:/dat5")
sales01 <- read.csv('sales_w05.csv',sep=',',header=T,
                    stringsAsFactors = FALSE)

sales02 <- read.csv('sales_w06.csv',sep=',',header=T,
                    stringsAsFactors = FALSE)

str(sales01)
nrow(sales01)
nrow(sales02)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data preprocessing 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sales01$Sales <- gsub(',','',sales01$Sales)
sales01$Sales <- as.numeric(substr(sales01$Sales,2,10))

# merge the two data sets
sales <- rbind(sales01,sales02) 

sales <- as.data.table(sales)

sales[ , Order_Date := as.Date(sales$Order_Date,'%m/%d/%Y')]
sales[, Ship_Date := as.Date(sales$Ship_Date,'%m/%d/%Y')]


sales <- sales[ ! is.na(sales$Order_Date) 
                 & sales$Order_Date > as.Date("2013-03-05")  
                 &sales$Order_Date > as.Date("2014-07-31")  
                  ,    ]

sales$month <-  as.integer(format(sales$Order_Date,"%m"))
sales$year <-  as.integer(format(sales$Order_Date,"%Y"))
sales$day <-  as.integer(format(sales$Order_Date,"%d"))
str(sales)

head(sales)

sales$month <- as.factor(sales$month)
sales$year <- as.factor(sales$year)
sales$day <- as.factor(sales$day)

unique(sales$month)
nrow(sales)

View(sales)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Top 10 Store which has highest sales by Category
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

anyNA(sales)
sales <- na.omit(sales)
anyNA(sales)
nrow(sales)

head(select(sales,month))

unique(sales$City)

fil_4cities <- c('Detroit','San Francisco','Los Angeles','Houston')
# select category,city,sales from sales
# where city in 
# grou

sales %>%
      select (Category,City,Sales) %>%
      filter(City %in% fil_4cities)  %>%
      group_by(City,Category)  



sales %>%
  select (Category,City,Sales) %>%
  filter(City %in% fil_4cities)  %>%
  group_by(City,Category)   %>%
  summarize(Tot.Sales  = sum(Sales),No.cust = n()) 
  

tmp <- sales %>%
 select (Category,City,Sales) %>%
   filter(City %in% fil_4cities)  %>%
  group_by(City,Category)   %>%
  summarize(Tot.Sales  = sum(Sales),No.cust = n())  %>%
  arrange(City,desc(Tot.Sales))

# grammer of graphics   

ggplot(sales,aes(x=Sales)) + 
   geom_histogram(fill = 'red', binwidth = 1000)

ggplot(tmp,aes(x=City,y=Tot.Sales)) + 
  geom_bar(stat = 'identity', fill='red')  + 
  ggtitle('Total Sales by City') 


ggplot(tmp,aes(x=City,y=Tot.Sales,fill=Category)) + 
  geom_bar(stat = 'identity', color='red')  + 
  ggtitle('Total Sales by City') 
  
  
ggplot(tmp,aes(x=City,y=Tot.Sales,fill=Category)) + 
  geom_bar(stat = 'identity', color='red',
           position = position_dodge() )  + 
  ggtitle('Total Sales by City') 






