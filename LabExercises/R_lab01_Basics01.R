##############################Basic calculations##############################               
#Mathematical Operations
2+2
2+3^2
(2+2)^4
sqrt(2)
log(2)  # default base is exp(1)
# Assiging a value and performing operations
x = 5
x=6
y = 10
z <- x+y
z
#To create sequence of nummbers
seq(1,5, by=.5)
#Repeating a number
rep(1,10)


#############################   Evaluation and Printing    #########################################################
# The <- symbol is the assignment operator.
x <- 5 	## nothing printed  
x 		## auto-printing occurs
print(x) 	## explicit printing
# The : operator is used to create integer sequences.
x <- 1:20
x

##################################### Creating Vectors######################################
# The c() function can be used to create vectors of objects.
x <- c(0.5, 0.6) ## numeric
x <- c(TRUE, FALSE) ## logical
x <- c(T, F) ## logical
x <- c("a", "b", "c") ## character
x <- 9:29 ## integer
x <- c(1+0i, 2+4i) ## complex
class(x)

#Using the vector() function
x <- vector("numeric", length = 10)
x

#################################  Mixing Objects ##################################
y <- c(1.7, "a") ## character
y <- c(TRUE, 2) ## numeric
y <- c(TRUE, 2,FALSE) ## numeric
y <- c("a", TRUE) ## character
###############################  Explicit Coercion #########################
x <- 0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

#Coercion results in NAs.
x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)
x <- c("a", "b", "c", 3)
as.numeric(x)

###########################################  Matrices  ########################
 m <- matrix(nrow = 2, ncol = 3)
 m
dim(m)
attributes(m)
m <- matrix(1:6, nrow = 2, ncol = 3)  # here byrow=FALSE
m
m <- matrix(1:6, nrow = 2, ncol = 3,byrow = TRUE)
m
# Matrices can be constructed row-wise also by defining parameter byrow
m <- matrix(1:6, nrow = 2, ncol = 3, byrow=TRUE)
m

m <- 1:10
m

dim(m) <- c(2, 5)
m

##################  cbind-ing and rbind-ing   ###########################################

x <- 1:3
y <- 10:12
cbind(x, y)

rbind(x, y)

##############################  Lists ###########################

x <- list(c(1,2), "a", TRUE)
x

##############################  Factors  ##################################
x <- factor(c("yes", "yes", "no", "yes", "no"))
x

############################   Missing Values  ##############################

# NaN ("Not a Number") means 0/0
# NA ("Not Available") is generally interpreted as a missing value
# and has various forms - NA_integer_, NA_real_, etc. 
# Therefore, NaN ??? NA and there is a need for NaN and NA.
# is.na() returns TRUE for both NA and NaN, however is.nan()
# return TRUE for NaN (0/0) and FALSE for NA.


x <- c(1, 2, NA, 10, 3)
is.na(x)
is.nan(x)


as.character(NA)
x[3]
5 * x[3]

x <- c(1, 2, NaN, NA, 3)
is.na(x)
is.nan(x)


# NA is not equal to any other value or to itself
if (x[4]= NA) {
  print('is null value')
} else { print ('cant be compared')}


###################   Data Frames   ###############################

x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
x
nrow(x)
ncol(x)

#####################################  Names ###############################
x <- 1:3
names(x)

names(x) <- c("foo", "bar", "norf")
x
names(x)

############################ Names & list #####################
x <- list(a = 1, b = 2, c = 3)
x

########################  Names & Matrix ###########################
m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))
m
#############################  Sub-set of a vector ####################

x <- c(2,4,6,6,8,2)
x[1]
x[2]
x[1:4]
x[x > 2]
u <- x  >2
u
x[u]
#############################  Sub-set of a matrix ####################
#Matrices can be subset in the usual way with (i , j) type indices.
x <- matrix(1:6, 2, 3)
x[1, 2]
x[2, 1]

# row/column  Indices can also be excluded as cited below to get only columns or only rows respectively
x[1, ]

x[, 2]

###################  Removig NA values ########################

x <- c(1, 2, NA, 4, NA, 5)
x[! is.na(x)]
y=na.omit(x)
y

x[! x %in% 4]

################################  Vector Operations  #####################

x <- 1:4; y <- 6:9
x + y
x > 2
x * y


############################  Matrix Operations #########################

x <- matrix(1:4, 2, 2); y <- matrix(rep(10, 4), 2, 2)
x * y ## element-wise multiplication
x / y
x %*% y        ## true matrix multiplication

#################### Data Types (vectors, matrices & dataframes) ####################
v1=c(1,2,3,4,5) #numeric vector
v2=c("a","b","c","d","e") #character vector
v3=c(TRUE,FALSE,TRUE,FALSE,TRUE) #logical vector
str(v1) #to view structure of the vector
length(v1) #to get number of  elements in the vector
str(v2)
str(v3)
str(c("fg","gh"))
cbind(v1,v2,v3) #column binding
rbind(v1,v2,v3) #row binding

#Playing with a vector
v4 = c(10,9,8,7,6,5)
max(v4)
min(v4)
mean(v4)
sd(v4)
v4[c(1,3)]#To get 1st and 3rd item
v4[v4=8] #To get elements which are greater than 8
m1 <- matrix(c(1,2,3,4),nrow=2,ncol=2)
class(m1)
#Matrix and data frame
M=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE) # to create a matrix
ls()
rm(M)
data = data.frame(v1,v2,v3) # to create a data frame
names(data)
colnames(data)
names(data) = c("ID","Name","Selected") # to assign variable names
data1 = data[,c(2:3)]
#Handling missing values in the data
v=c(10,25,20,NA,36,100)
mean(v,na.rm=TRUE)
mean(v)
is.na(v)
v[is.na(v)]=555
v

##################################  Central tendencies ############################

vec = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,2,3,4,3,4,4)
length(vec)
mean(vec)
median(vec)
quantile(vec)
sd(vec)
var(vec)







a1 <- array(c(1:12),dim=c(3,2,2))
class(a1)
