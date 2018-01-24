print ("Om Mahaa Ganapataye Namah")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Anova , Case Study 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))
Top <- c(2,3,7,2,6)
Mid <- c(10,8,7,5,10)
FShelve <- c(10,13,14,13,15)

group = data.frame(cbind(Top,Mid,FShelve))

df1 = stack(group)

SalesAnova <- aov(values~ind,data = df1 )
summary(SalesAnova)

# post HPC test - Multiple pairwise-comparisons
TukeyHSD(SalesAnova)
plot(TukeyHSD(SalesAnova))

# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.

oneway.test(values~ind,data=df1,var.equal = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checking the Assumptions of Anova
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check the homogenity of variance assumptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(car)
leveneTest(values ~ ind, data = df1)

# from the output above we can see that the p-value is not less
# than the significance level of 0.05. 
# This means that there is no evidence to suggest that the
# variance across groups is statistically significantly different.
# Therefore, we can assume the homogeneity of variances in 
# the different treatment groups.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check the normality assumption
# Extract the residuals
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aov_residuals <- residuals(object = SalesAnova )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run Shapiro-Wilk test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shapiro.test(x = aov_residuals )
# this test reveals that there is no indication 
# that normality is violated.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Non-parametric alternative to one-way ANOVA test
# this can be used when ANNOVA assumptions are not met.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kruskal.test(values ~ ind, data = df1)

