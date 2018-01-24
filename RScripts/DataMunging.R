#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Author      : Sasken
#     Date        : 02/Jul/2017
#     Version     : v0.1
#     Description : Preform data munging
#     and data wrangling on a retail marketing  
#     data to perform data analysis which can help
#     the marketing department to come up with the 
#     tailor-made offerings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# clean up the environment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ls()
rm(list=ls(all=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import libraries 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- read.csv("D:/dat1/retailMarketing.csv")
str(df)
dim(df)
head(df)
summary(df)
attach(df)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Explore the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(select(df,-Married))

head(filter(df,Salary > 10000))

head(filter(df,Salary > 10000 & 
              Married == 'Single'))
glimpse(df)

head(df[df$Salary > 10000,])

hist(AmountSpent)

out <- boxplot(AmountSpent~df$Married,data = df,
               main='Amount Spent by both Married and Single',
               xlab = 'Marital Status',
               ylab= 'Amount Spent',
               col = 'red' ,horizontal = TRUE)

top25 <- (filter(df,(AmountSpent > 2100) & 
              (AmountSpent <  4100)    ))

head(top25)
summary(top25)
# History has NA's 5 (5 missing values )
# 137 Married customers spend is between 2100 - 4100
# 15 Single customers spend is between 2100 - 4100

out <- boxplot(AmountSpent~df$Married,data = df,
               main='Amount Spent by both Married and Single',
               xlab = 'Marital Status',
               ylab= 'Amount Spent',
               col = 'red' ,horizontal = TRUE)
head(Married)

# ggplot , grammar of graphics components 
# 1st component data frame
# 2n component aesthetics
# 3rd component is geometric objects
# 4th component is scale

ggplot(top25,aes(x=Gender,
                 y=AmountSpent)) +
  geom_bar(stat = 'identity')+
  ggtitle('Amount Spent by Gender
          and their housing status')
# male spend is more than female

df %>% 
  group_by(Gender) %>%
summarise(AvgSpend=sum(AmountSpent))

# Out of the total sales , Male's spend is 57%
# whereas females spend is 42%

gMarried <- df %>% 
  group_by(Gender,Married) %>%
  summarise(AvgSpend=sum(AmountSpent))
dev.off()

Gender <- factor(Gender,levels = c("Male","Female"))

ggplot(top25,aes(x=Gender,
                 y=AmountSpent)) +
  geom_bar(stat = 'identity')+
  ggtitle('Amount Spent by Gender
          and their housing status')

ggplot(df,aes(x=Gender,
                 y=AmountSpent)) +
  geom_bar(stat = 'identity')+
  ggtitle('Amount Spent by Gender
          and their housing status')

gSpend <- df %>% 
  select(Gender,Married,AmountSpent) %>%
  group_by(Gender,Married) %>%
  summarise(AmtSpent=sum(AmountSpent))

print(gSpend)

ggplot(gSpend,aes(x=Married,
              y=AmtSpent, fill=Gender)) +
  geom_bar(stat = 'identity')+
  ggtitle('Amount Spent by Gender
          and their housing status')

# Married & Single male spend is more than Married & 
# Single women

# Below one displays the top 5 spenders by Gender,M.Status
gSpend <- df %>% 
  select(Gender,Married,AmountSpent) %>%
  arrange(Gender,Married) %>%
  group_by(Gender,Married) %>%
  top_n(5)

print(gSpend)

ggplot(gSpend,aes(x=Gender,
                 y=AmountSpent, fill=Married)) +
  geom_bar(stat = 'identity')+
  ggtitle('Top 5 Amount Spent by Gender
          ')


ggplot(top25,aes(x=AmountSpent,
                 y=Salary)) +
  geom_bar(stat = 'identity')+
  ggtitle('Amount Spent by Gender
          and their housing status')

# Those with higher salary they spend more 

ggplot(top25,aes(x=Location,
                 y=AmountSpent)) +
  geom_bar(stat = 'identity')+
  ggtitle('Amount Spent by Gender
          and their housing status')
# Location doesn't seems to be significant

#~~~~~~~~~~~~~~
# pairplot
#~~~~~~~~~~~~~~
pairs(df[,-c(8)])
# There exists a relationship between salary and A.Spent

df %>% 
  select(Gender,AmountSpent) %>%
  

ggplot(top25,aes(x=Married,
                 y=AmountSpent,fill=OwnHome)) +
        geom_bar(stat = 'identity')+
         ggtitle('Top 25 % of the Amount Spent by Married
                 and their housing status')
# Most of the houses are owned by Married than Single 


ggplot(top25, aes(x=Catalogs,y=AmountSpent,
                        fill=History)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = c('orange','blue','yellow')) +
  ggtitle('Amount Spent by Catalogs by History')

# ~~~~~~~~~~~~~~~~~~~~
# Faceting
# ~~~~~~~~~~~~~~~~~~~~
mas <- ggplot(df,aes(x=Married,y=AmountSpent))+
  geom_line(aes(color=OwnHome))

mas + geom_line()+ facet_wrap(~Catalogs) + theme_bw()
# More the catalogs sent , higher response has been noticed

# ~~~~~~~~~
# Tree map
# ~~~~~~~~~
mas <- ggplot(df,aes(x=Married,y=AmountSpent))+
  geom_tile(aes(color=OwnHome))

mas + geom_tile()+ facet_wrap(~Catalogs) + theme_bw()

















# ~~~~~~~~~~~~~~~~~~~~
# dplyr
# ~~~~~~~~~~~~~~~~~~~~
close_spent <- df %>%
select(Location, AmountSpent) %>%
  filter(Location == 'Close')  %>%
  summarise(Total=sum(AmountSpent))
  

str(df)
ggplot(df,aes(x=AmountSpent , y=)+ 
        geom_bar(stat = 'identity') +
        ggtitle("AmountSpent By Married & single"))
dev.off()
        
ggplot(df, aes(x=Location,y=AmountSpent)) + 
  # geom_bar( stat = "identity") +
  geom_tile( ) +
  ggtitle("by Day and Month")  

































sal <-  function(hra,basic){
  sal = (hra+basic)
  return(sal)
}
sal(10000,2000)

nsal <- function(sal,hra,basic,leave)
  {
  nsal = sal(hra,basic) - leave
  return(nsal)
}

nsal(sal,10000,8000,5000)

# 1. m.f1  low
# 2 m.f2   med
# 3.m.f5   low
# 3. m.f3  high



fback <- read.csv("D:/datasets/feedback.csv")
str(fback)

unclass(fback$ratings)
# 
# High, Low , Medium
# 1,2,3
# 
# High,  Medium Low 
# factor(df, levels = )


fback <- read.csv("D:/datasets/feedback2.csv")
str(fback)
unclass(fback)
# low, medium, high

fback$ratings <- factor(fback,
                      levels=c('low','medium','high'))

unclass(fback$ratings)
levels(fback$ratings)

library(data.table)
st_tmsp = Sys.time()
dt <- fread("https://github.com/Sasken2k/AI2/raw/master/train_2017.csv")
end_tmsp = Sys.time()
end_tmsp - st_tmsp

st_tmsp = Sys.time()
df <- read.csv("D:/casestudy/train_2017.csv")
end_tmsp = Sys.time()
end_tmsp - st_tmsp

st_tmsp = Sys.time()
df <- fread("D:/casestudy/train_2017.csv")
end_tmsp = Sys.time()
end_tmsp - st_tmsp

