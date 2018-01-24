print("Om Mahaa Ganapataye Namah")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clear up the environment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls(all=TRUE))

library(data.table)
library(ggplot2)
library(ggthemes)
library(DT)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Preparation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd('D:/dat5')
sales01 <- read.csv('sales_w05.csv',sep=',')
sales02 <- read.csv('sales_w06.csv',sep=',')

sales <- rbind(sales01,sales02)
dim(sales)

sales <- as.data.table(sales)
str(sales)
sales <- na.omit(sales)

sales[, Order_Date := as.Date(Order_Date,'%m/%d/%Y')]
sales[, Ship_Date := as.Date(Ship_Date,'%m/%d/%Y')]

# dt <- "02/05/2017"
# class(dt)
# as.Date(dt,'%m/%d/%Y')
# as.Date(dt,'%d/%m/%y')
# dt <- "02-05-2017"
# as.Date(dt,'%m-%d-%Y')
# 
# dt1 <- format(dt,'%d %b %Y')

t <- ('2011-03-05')
class(t)
format(t,'%d/%m/%Y')

# extract sales data betweek 2011-03-05
sales <- sales[! is.na(sales$Order_Date) 
               & sales$Order_Date > as.Date('2011-03-05')
               & sales$Order_Date < as.Date('2014-07-31'),
               ]

dim(sales)


miss_val_col <- sapply(sales,function(x) sum(is.na(x)))
sales$Sales <- as.numeric(substr(gsub(',','',sales$Sales
),2,10))

# Create month, date and year features
sales$month <- as.integer(format(sales$Order_Date,'%m'))
sales$day <- as.integer(format(sales$Order_Date,'%d'))
sales$year <- as.integer(format(sales$Order_Date,'%Y'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Univariate Analysis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(sales,aes(x=Sales)) +
  geom_histogram(fill='red' )

ggplot(sales,aes(x=Shipping_Cost)) +
  geom_histogram(fill='red' )

ggplot(sales,aes(x=Profit)) +
  geom_histogram(fill='red' )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bivariate Analysis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(sales,aes(x=Category,y=Sales)) +
  geom_boxplot(fill='red' )

Cat <- ggplot(sales,aes(x=Category,y=Sales)) +
  geom_bar(stat = 'identity',fill='red' )

Cat
pie <- Cat + coord_polar(theta='y')
pie

ggplot(sales,aes(x=Category,y=Sales,color=Segment)) +
  geom_bar(stat = 'identity' )

ggplot(sales,aes(x=Category,y=Sales,color=Segment)) +
  geom_bar(stat = 'identity' , position = position_dodge())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Top 10 returns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
names(sales)

ret <- sales %>%
       select(Product.Name,Returned)  %>%
       filter(Returned != 0)   %>%
       group_by(Product.Name) %>%
       summarise(No.of.Returns=n()) %>%
       arrange(No.of.Returns) %>%
       top_n(5)
  
ggplot(ret,aes(x=Product.Name,y=No.of.Returns)) +
    geom_bar(stat = 'identity', fill='red') +
  labs(y = "Number of items returned ",
       title = "Product returns across Products") +
  theme(axis.title.x = element_blank()) +
    coord_flip()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Top 5 Sales by Stores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

storeSale <- sales %>%
             group_by(State,Category)  %>%
             summarise(Tot=sum(Sales)/100)%>%
             arrange(desc(Tot))

top10 <- head(storeSale,10)
ggplot(top10,aes(State ,Tot, fill=Category)) +
  geom_bar(stat = 'identity')+
  coord_flip()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Sales by Day
#  Sales Month by day
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

heatMap <- sales %>% 
           mutate(Month = lubridate::month(Order_Date,label=TRUE),
                  Day = lubridate::wday(Order_Date,label=TRUE)) %>%
           group_by(Month,Day) %>%
           summarise(TotalSales=sum(Sales)/100) %>%             
           ggplot(aes(x=Month,y=Day)) +
           geom_tile(aes(fill=TotalSales)) +    
           scale_fill_gradient(name = 'sales',low='white',
                               high = 'red'
                                 )  +
           labs(title = 'Total Revneue across months and days ($1000)',
                caption = 'Heatmap Visualization for Gross REvenue')

heatmap2 <- sales %>%
            mutate(Month = lubridate::month(Order_Date,label=TRUE),
                   Day = lubridate::wday(Order_Date,label=TRUE)) %>%
            group_by(Month,Day) %>%
            summarise(TotSales=sum(Sales)/100) %>%
            ggplot(aes(x=Month,y=Day)) +
            geom_tile(aes(fill=TotSales)) +
            scale_fill_gradient(name = 'sdf',low='white',high = 'red') +
            labs(title = 'sdfdsf',caption = 'Om shivaya namah')



# faceting 
tot_sales <- sales %>% 
  mutate(Month = lubridate::month(Order_Date,label=TRUE),
         Day = lubridate::wday(Order_Date,label=TRUE)) %>% 
  group_by(Month, Day) %>% 
  summarise(TotalAmount = sum(Sales)/1000) %>% 
  ggplot(aes(x = Month, y = TotalAmount)) +
  geom_bar(stat = "identity", aes(fill = Day)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Total Sales Amount Across Months ($1000)") +
  facet_wrap(~Day, nrow = 4) +
  theme(axis.title =x`` element_blank())



# faceting 
tot_sales <- sales %>% 
  mutate(Month = lubridate::month(Order_Date,label=TRUE),
         Day = lubridate::wday(Order_Date,label=TRUE)) %>% 
  group_by(Month, Day) %>% 
  summarise(TotalAmount = sum(Sales)/1000) %>%
  top_n(5) %>%
  ggplot(aes(x = Month, y = TotalAmount)) +
  geom_bar(stat = "identity", aes(fill = Day)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Total Sales Amount Across Months ($1000)") +
  facet_wrap(~Day, nrow = 4) +
  theme(axis.title = element_blank())


p2 <- ggplot(sales,aes(Category,
                       Sales)) + 
  geom_bar(stat = "identity", aes(fill = Category)) +
  # p2 + geom_line(aes(color=Category))
  facet_wrap(~State, nrow = 10)
