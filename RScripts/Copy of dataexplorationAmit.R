
rm(list=ls(all=TRUE))

mkt <- read.csv("D:/dat1/retailMarketing.csv")
summary(mkt)


AmountSpent

attach(mkt)
unique(Children)

# select  from mkt
# where Married == 'Married' ;

# frequency distribution
table(Gender)

df.married <- mkt[mkt$Married == 'Married',]
df.single <- mkt[mkt$Married == 'Single',]

sum(df.married$AmountSpent)
sum(df.single$AmountSpent)

boxplot(AmountSpent~factor(Gender),
     data=mkt,
     xlab = "Married", ylab="Amount Spent")
  
names(mkt)

boxplot(AmountSpent~factor(Children),
        data=mkt,
        xlab = "Married", ylab="Amount Spent")


names(mkt)


hist(AmountSpent)

hist(log(AmountSpent))

a <- log(AmountSpent)
head(cbind(AmountSpent,a,exp(a)))

pairs(mkt)

library(dplyr)

by_hist <- mkt %>% 
     filter(Married == 'Married') %>%
   group_by(Children,Married) %>%
  summarise(Total = sum(AmountSpent))

by_hist1 <- mkt %>% 
  select(Catalogs,Children,Married,Salary,AmountSpent) %>%
  filter(Married == 'Married') %>%
  group_by(Children,Married,Catalogs) %>%
  summarise(Total = sum(AmountSpent)) %>%
  arrange(Children,desc(Total)) 
dev.off()

ggplot(by_hist1,aes(x=Children, y=Total, fill=Catalogs 
                    )) +
  geom_bar(stat='identity',color='red') + 
  ggtitle("Heat map by total amountspent by Married customers")

mkt %>%
  select(History,AmountSpent) %>%
  filter(History != 'NA') %>% 
  group_by(History) %>%
  arrange(History,desc(AmountSpent)) %>%
  top_n(n=3, wt=AmountSpent)

rm(disp_High_IncGroup)

disp_High_IncGroup <- mkt %>% 
  select(Location,AmountSpent)  %>%
#  filter(Married == "Married") %>%
  group_by(Location) %>%
  summarise(Total=sum(AmountSpent))

ggplot(disp_High_IncGroup,aes(x=Location, y=Total)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Bar plot  by total amountspent by Married customers")

boxplot(AmountSpent,factor(Location),
        data=mkt,
        Orientation = 'Horizontal',
        xlab = "Location", ylab="Amount Spent")


plot(Salary,AmountSpent,data=mkt)



disp_High_Salary <- mkt %>% 
  select(Married,Salary,AmountSpent)  %>%
  #  filter(Married == "Married") %>%
  group_by(Married) %>%
  summarise(Total=sum(AmountSpent))

 ggplot(disp_High_Salary,aes(x=Married, y=Total )) +
   geom_bar(stat = 'identity') + 
   ggtitle("Bar plot  by total amountspent by Married customers")

 
ggplot(disp_High_IncGroup,aes(x=Location, y=Total)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Bar plot  by total amountspent by Married customers")

# grammar of graphics - its framework
# data frame 
# aes - aesthetic 
# geom - geometric object -  type of chart 


ggplot(by_hist,aes(x=Children, y=Total)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Bar plot  by total amountspent by Married customers")

ggplot(mkt,aes(x=Gender,y=AmountSpent))+
  geom_bar(stat='identity') 

ggplot(by_hist,aes(x=Total)) +
  geom_histogram(color="blue", binwidth = 50000) + 
  ggtitle("Heat map by total amountspent by Married customers")

ggplot(by_hist,aes(x=Children, y=Total)) +
  geom_boxplot(aes(fill=Children, group=Children))
  

qqplot(Children,AmountSpe)






