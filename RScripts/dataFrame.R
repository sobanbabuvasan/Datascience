# Om Gam Ganapataye Namah
# Om Gurave Namah
# Om Swami malai muruga potri
# Om hayagreeva Potri
# Om Bhudhaya Namah
#
# Probability
# Om Vallam Ganapataye Namah

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Working with Data Frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

products <- data.frame(product = c('Logitech Memory Card',
                                   'USB Apple Speaker Phone', 'VoIP',
                                   'Epson Phone', 'White Wilson Jones Binding Machine',
                                   'Ibico Binder Covers'),
                       Soh = c(2,3,5,5,3,10),
                       uprice = c(850,6850,4210,2180,2170,25) )

# to view no. of rows and columns
dim(products)

# To view the structure of the df
str(products)

# To view only the number of rows
nrow(products)


# Accessing the data 
products
# To view the 2nd column alone 
products[,2]
# to display the 2 row with all the columns 
products[2,]
# to display 2 row and 2rd column
products[2,2]
# to display 2nd row with 3rd column
products[2,3]

# display all except the first column
products[-1,]
# Exclude only the 1st column
products[,-1]

products$product
products$Rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Joining Data Frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Left Outer join
products <- data.frame(product = c('Logitech Memory Card',
                                   'USB Apple Speaker Phone', 'VoIP',
                                   'Epson Phone', 'White Wilson Jones Binding Machine',
                                   'Ibico Binder Covers'),
                       Soh = c(12,13,15,15,13,15),
                       uprice = c(850,6850,4210,2180,2170,25) )

Order <- data.frame(Order_ID = c('MA-2013-1850','MA-2013-1851'),
                    product = c('Logitech Memory Card',
                                'USB Apple Speaker Phone'),
                    #Quantity = c(2,3,5,5,3,10),
                    #  SellingPrice = c(950,6950,4310,2280,2270,15)
                    Quantity = c(2,3),
                    SellingPrice = c(950,6950),
                    State = c('California','California'),
                    stringsAsFactors = FALSE 
)
Order
str(Order)

names(Order)
ncol(Order)
nrow(Order)
row.names(Order)


# Adding a new column
Order1 <- data.frame(Order_ID = c('MA-2013-1850','MA-2013-1851'),
                     product = c('Logitech Memory Card',
                                 'USB Apple Speaker Phone'),
                     #Quantity = c(2,3,5,5,3,10),
                     #  SellingPrice = c(950,6950,4310,2280,2270,15)
                     Quantity = c(2,3),
                     SellingPrice = c(950,6950),
                     State = c('California'),stringsAsFactors = FALSE 
)
# Adding a new column
Order1$Ship_Mode <- c('First Class','Second Class')

# Adding a new column at the end ( diff. approach)
Order1 <- cbind(Order1,Ship_Mode)
str(Order1)

# removing a column
Order1$Ship_Mode <- NULL
Ship_Mode <- c('First Class','Second Class')

Order1 <- cbind(Order1,Ship_Mode)
str(Order1)

# Removing first 3 rows 
dfnew <- dfunion[-c(1:3),]
dfnew 

# Removing a specific row 
dfnew1 <- dfunion[-c(2,3),]
dfnew1
dim(dfnew1)
dim(dfunion)

options(width = 200)
# Removing a row based on a criteria
dfunion[dfunion$Quantity > 2,]

dfunion[ dfunion$Quantity == 2,]

dfunion[! (dfunion$Quantity == 2)]
str(dfunion)
dfunion$Quantity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Joining the data frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
merge(x=products, y=Order, by = 'product'
      ,all.x = TRUE)

merge(x=products, y=Order, by = 'product'
      ,all.y = TRUE)

merge(x=products, y=Order, by = 'product' )


merge(x=products, y=Order, by = 'product' ,all=TRUE)

merge(x=products, y=Order,by=NULL)

# data frame 1
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Oven", 3), rep("Television", 3)))
df1

# data frame 2
df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("California", 2), rep("Texas", 1)))
df2

Order[,4]

Order25Jun <- data.frame(Order_ID = c('MA-2013-1852','MA-2013-1853'),
                         product = c('Logitech Memory Card',
                                     'USB Apple Speaker Phone'),
                         #Quantity = c(2,3,5,5,3,10),
                         #  SellingPrice = c(950,6950,4310,2280,2270,15)
                         Quantity = c(8,2),
                         SellingPrice = c(945,6945),
                         State = c('California')
)

dfunion = rbind(Order,Order25Jun)
dfunion

aggregate(dfunion[3:4], by=list(Order$State,Order$product),
          FUN=mean,na.rm=TRUE)

aggregate(Order[3:4],by=list(Order$State, Order$product), 
          FUN=mean)


agg_mean <- aggregate(Order[3:4],
                      by=list(Order$State,Order$product),
                      FUN=mean, na.rm=TRUE)
function(x) 
  c(mn=mean(x),n=length(x),tot=sum(x))


agg_mean <- aggregate(dfunion[3:4],
                      by=list(dfunion$State,dfunion$product),
                      FUN=function(x) 
                        c(mn=mean(x),n=length(x),tot=sum(x)))


summary(dfunion)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate function in R with mean summary statistics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str(Order)
agg_mean <- aggregate(Order[,4],
                      by=list(Order$State),
                      FUN=mean, na.rm=TRUE)
agg_mean

by(Order[,4],list(Order$State,Order$product),mean)

aggregate(dfunion[3:4],by=list(dfunion$State,dfunion$product),
          FUN=function(x) {
            c(mn=mean(x),ln=length(x))
          })

agg_mean <- aggregate(Order[,4],
                      by=list(Order$State),
                      FUN=mean, na.rm=TRUE)



agg_mean <- aggregate(Order[,4],
                      by=list(Order$State),
                      FUN=sum, na.rm=TRUE)
agg_mean

agg_mean <- aggregate(Order[,4],
                      by=list(Order$State),
                      FUN=function(x) 
                        c(mn=mean(x),n=length(x),tot=sum(x)))
agg_mean

agg_mean <- aggregate(dfunion[3:4],
                      by=list(dfunion$State),
                      FUN=function(x) 
                        c(mn=mean(x),n=length(x),tot=sum(x)))
agg_mean


agg_mean <- aggregate(dfunion[3:4],
                      by=list(dfunion$State,dfunion$product),
                      FUN=function(x) 
                        c(mn=mean(x),n=length(x),tot=sum(x)))
agg_mean

Order[,4]

require(reshape2)
df_melt <- melt(dfunion, grp = c("Order_Id",
                                 "State"))
df_melt

# Melting and Casting function
df_melt <- melt(dfunion, 
                grp = c("Order_ID","product","State"))
df_melt

dcast(df_melt,State~variable,sum)
options(width = 1000)

wsales <- dcast(df_melt,State+product~variable,sum)
wsales

write.csv(wsales,"D:/dat1/wreport.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(RSQLite)

##Database connectivity
sqlite<-dbDriver("SQLite")
customersdb<-dbConnect(sqlite,"D:/dat1/customersDB.db")

##Listing tables
dbListTables(customersdb)

##Listing fields from a table
dbListFields(customersdb,"newyork")

##Running SQL queires
dbGetQuery(customersdb,
           "select count(*) from newyork")


dbGetQuery(customersdb,
           "select * from newyork limit 10")


dbGetQuery(customersdb,
           "select city,count(*) as cnt
           from newyork group by city order by cnt
           desc")


##Reading data from SQLite to R
newyork<-dbReadTable(customersdb, "newyork")
head(newyork)





matrix1 <- matrix(c(1, 3, 5, 7), nrow = 2)

matrix1 <- matrix(c(1, 3, 5, 7,8,10), nrow = 2, ncol=3)

matrix2 <- matrix(c(2, 4, 6, 8), nrow = 2)

# Add the matrices.
Mat.Add <- matrix1+ matrix2

# Subtract the matrices
Mat.Sub <- matrix1 - matrix2
Mat.Sub




# Factors in R: Convert to Factor without order.
data = c(4,5,5,4,4,3,5,4,5,4,5,4,3,3)
length(data)
class(data)

fdata = factor(data) # converting numeric to factor
class(fdata)
print(fdata)




i <- 1 
while (i <= 6){
  print (i*2)
  i = i + 1
}

i <- 1 
while (i <=8)
{
  if (i == 6)
  {
    
    i = i + 1
    next;
  }
  print (i+2)
  i = i +1
}

i <- 1
while (i <= 6) {
  if (i==4)
  {
    i=i+1
    next;
  }
  print(i*i);
  i = i+1;
}

i <- 1
while (i <= 6) {
  if (i==4)
    break;
  print(i*i)
  i = i+1
}

# while loop in R with next statement
# skip one step of the loop. Once the next statement 
# is read, the loop will skip the while once.
i <- 1
while (i <= 6) {
  if (i==4)
  {
    i=i+1
    next;
  }
  print(i*i);
  i = i+1;
}
options(width = 200)
dfunion[order(dfunion$product),]


# R nested for loop

for(i in 1:5)
{
  for(j in 1:2)
  {
    print(i*j);
  }
}


# R for loop with break statement

x <- 1:5
for (i in x) {
  if (i == 3){
    break
  }
  print(i)
}