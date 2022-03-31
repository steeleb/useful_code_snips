# bits of code for data management

#### MERGING ####

# merge dataframes side by side by common columns
dframe_combined=merge(dframe1, dframe2, all=T)
# all=T adds NA values for variables in rows that are not in one of the data frames
# can specify by variables if they don't have the same name: by=intersect(names(dframe1$var1), names(dframe2$var2))
# can sort by the "by" variables by adding: sort=T
# can only merge 2 data frames this way, so if you have many, merge 2 of them into dframe_combined, then merge the next to dframe_combined, etc for each one.
# this one works both when all have the same columns and when there are some additional columns in some of the data frames to be combined.  NAs are added where there are missing values

# merge dataframes one on top of the other (SAS set)
dframe_combined <- rbind(dframe1, dframe2)
# this one is useful if each data frame to be combined have the exact same columns and column names




#### SELECTING AND SORTING ####

# to select and reorder columns in a dataframe
dframe2 <- subset(dframe, select = c(var2, var1, var5))

# sort dataset by variable values
dframe <- dframe[with(dframe, order(var1, var2, var3)), ]

#sort by datetime
DF <- DF[ order(DF$DTime , decreasing = TRUE ),]




#### REMOVE ROWS BASED ON CONDITION ####

ix=which(dframe$variable == value)
dframe=dframe[-ix, ]
# which statement gives the index numbers for which rows the condition is true - ex: where a variable is <0 or ==NA 
# [] allows for subsetting and the - indicates leaving those rows out

# to see which rows the which statement chose
dframe[ix,]

# for more than one condition in the which, use & for and, | for or
ix=which(dframe$var1 == value & dframe$var2 == value)
ix=which(dframe$var1 == value1 | dframe$var1 == value2)

# only allows for one condition at a time
dframe2 <- subset(dframe, subset = dframe$variable > 0)

# to drop factor levels that aren't used anymore now that you've removed rows
dframe <- drop.levels(dframe)




##### REMOVE VARIABLES ####
library(gdata)
# Note: must have perl installed on pc at C:\\perl\bin\perl.exe
dframe <- remove.vars(dframe, names=c("var1", "var2", "var3"))
# if only one then omit the c() and just use names="var1"




#### RENAME VARIABLES ####
library(gdata)
# Note: must have perl installed on pc at C:\\perl\bin\perl.exe
dframe <- rename.vars(dframe, from=c("var1", "var2", "var3"), to=c("var01", "var02", "var03"))




#### SPLITTING VARIABLES ####

# split sample_label on the underscore into 2 parts
# part1
dframe$part1=sapply(strsplit(as.character(dframe$sample_label),"_"), "[", 1)
# part2
dframe$part2=sapply(strsplit(as.character(dframe$sample_label),"_"), "[", 2)




#### CONCATENATE VARIABLES ####

# concatenate 2 variables into a third with a separator of _
dframe$var_comb=paste(dframe$var1, dframe$var2, sep="_")




#### CHANGE OR SET VARIABLE VALUES BASED ON CONDITION ####

dframe$variable [dframe$variable==999] =NA
# inside the brakets is the condition, outside the brackets is the action you want to take
# the variables inside and outside the brackets can be the same (for reassignment) or different

# can use is.na inside brackets as well
dframe$variable1 [is.na(dframe$variable2)] = 0.01

# to use more than one condition use &
dframe$variable1 [is.na(dframe$variable1) & dframe$variable2=="23Jul2008"] = 0.01

# to change more than one thing on each subsetted row
ix=which(dframe$var1=="x" & dframe$var2=="y" & dframe$var3==0)
dframe$var5[ix]= dframe$var10[ix] * 100
dframe$var6[ix]= dframe$var11[ix] * 1000

# to replace a missing value with another variable
# useful for filling in missing deposition data
# create vector of missing data rows
ix=which(is.na(dframe$var1))
# if deps are missing, substitute the means
dframe$var1 [is.na(dframe$var1)] = dframe$var2[ix]

# to see which rows the which statement chose
dframe[ix,]




#### CREATE A DATAFRAME OUT OF VECTORS ####
a=c(1,2,3,4,5)
b=c("a","b","c","d","e")
c=c(5,4,3,2,1)
dframe <- data.frame(a,b,c)

#### FUNCTIONS ####

rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}


#### substring ####
substr(dframe, numbertostart, numbertoend)

#substring from end
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrRight(dataframe, numberofcharacterstoinlcude)

#### MISCELLANEOUS ####
# number of observations in variable x
length(x)

# formatting variables
# setting the number of decimal places and/or significan figures
dframe$var=signif(dframe$var, digits=4)
dframe$var=trunc(dframe$var)

# making all values in a variable upper or lower case
dframe$x=toupper(x)
dframe$x=tolower(x)

# reshaping data frames from long to wide (see LSTS_DOC.R)
library(reshape)
dframe2 <- as.data.frame(cast(dframe1, row_var~column_var))

# clean up workspace so only those actually needed in the end are visible
rm(list=setdiff(ls(), c("dframe1", "dframe2", "dframe3")))

# explore data just read in
levels(as.factor(dframe$var))
unique(dframe$var)
# this will print below the various values contained in variable var
colnames(dframe)
# this will print the column names


#### ASSIGN FACTOR LEVELS ####
fog_flux$landuse_auto=factor(fog_flux$landuse_auto, c("tropical montane", "tropical lowland", "subtropical montane", "subtropical lowland", "temperate montane", "temperate lowland"))


####FOR LOOP####
NYVTNHME$chla_obs_class <- as.character('')
for(i in 1:nrow(NYVTNHME)){
  if(NYVTNHME$chla_obs_c[i]>=1&NYVTNHME$chla_obs_c[i]<26){
    chla_class = '1-25'
    NYVTNHME$chla_obs_class[i] = chla_class
  } else 
    if(NYVTNHME$chla_obs_c[i]>=26&NYVTNHME$chla_obs_c[i]<101){
      chla_class = '26-100'
      NYVTNHME$chla_obs_class[i] = chla_class
    } else 
      if(NYVTNHME$chla_obs_c[i]>=101&NYVTNHME$chla_obs_c[i]<226){
        chla_class = '101-225'
        NYVTNHME$chla_obs_class[i] = chla_class
      } else 
        if(NYVTNHME$chla_obs_c[i]>=226&NYVTNHME$chla_obs_c[i]<1001){
          chla_class = '225-1000'
          NYVTNHME$chla_obs_class[i] = chla_class
        } else 
          if(NYVTNHME$chla_obs_c[i]>=1001&NYVTNHME$chla_obs_c[i]<3100){
            chla_class = '1001-3075'
            NYVTNHME$chla_obs_class[i] = chla_class
          } 
}



#### find and replace in entire dataframe ####
templight_2009b[templight_2009b=='NaN'] = NA  #replace NaN's with NA

