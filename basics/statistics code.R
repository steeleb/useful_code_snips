# bits of code for statistics


# CALCULATE MEANS GROUPING BY FACTORS

# AGGREGATE
# calculate the mean
dframe2=aggregate(list(dframe1$var1, dframe1$var2, dframe1$var3), list(dframe1$var5, dframe1$var6), mean, na.rm=TRUE)
      # first list are the variables to calculate means on
      # second list are the grouping variables
      # na.rm=T means leave out the NA values when calculating the means.  If na.rm=F then any variable with an NA value will have a mean of NA.
# assign column names for the variables
colnames(dframe2)=c("var5","var6", "var1", "var2", "var3")

# DOBY
library(doBy)
dframe2 <- summaryBy(var1 + var2 + var3 ~ var5 + var6, data = dframe1, na.rm=TRUE, FUN = mean)
      # variables before the ~ are to calculate the means on
      # variables after the ~ are the grouping variables
      # FUN can be mean, min, max, etc



# CALCULATE MORE THAN ONE SUMMARY STATISTIC BY GROUPS
library(doBy)
# define function
sumfun <- function(x, ...){c(mean = mean(x,...), min = min(x,...), max=max(x,...), obs = sum(!is.na(x)))}
      # all before the {} can remain the same as here
      # before the = is what to name the variables, eg var1.mean
      # after the = is the function
      # obs is a special function to sum the number of non-missing observations included in the stats
# calculate stats
dframe2 <- summaryBy(var1 + var2 + var3 ~ var5 + var6, data = dframe1, na.rm=TRUE, FUN = sumfun )
# other stats options
cv=sd(x,...)/mean(x,...) # coefficient of variation
se = sd(x,...)/sqrt(length(x)) # standard error
# to keep variables but not group by them, include
id=~var10+var14

sumfun <- function(x, ...){c(mean = mean(x,...), min = min(x,...), max=max(x,...), median=median(x,...))}
rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}


# GET STATS OF A LINEAR MODEL TO ACCOMPANY A REGRESSION
LM1 <- lm(yvar ~ xvar, data=dframe)
summary(LM1)
# regression equation (y=(num under variable)x + (num under intercept))
# R2 and p-value are listed in the last paragraph of output



# CALCULATE A MEAN OR SUM OF PARTICULAR VARIABLES ACROSS A ROW
dframe$meanvar=rowMeans(dframe[,c("var2", "var5", "var6")], na.rm=TRUE)
dframe$sumvar=rowSums(dframe[,c("var2", "var5", "var6")], na.rm=TRUE)



# CALCULATE MEAN PH
# calculate H+ ion in molar concentration
dframe$H_M=10^(dframe$pH * -1)
# calculate the mean
# convert H+ back to pH
dframe2$pH.mean=log10(dframe2$H_M.mean) * -1



