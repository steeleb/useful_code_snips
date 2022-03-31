# bits of code for importing and exporting data

library(readxl)
#import assuming character
read_csv('filepath',
         col_types = cols(.default = 'c'))


# IMPORT CSV FILE
dframe <- read.csv("path/file name.csv", header=T, skip=3, na.strings=c("", "."))

# if the your column headings are not what you want your variables to be in the file
# define vabiable names lists
varlist=c("var1","var2", "var3")
# input data file
dframe <- read.csv("path/file name.csv", header=F, skip=1, na.strings=c("", "."), col.names=varlist)



# IMPORT XLS FILE
library(gdata)
# Note: must have perl installed on pc at C:\\perl\bin\perl.exe

dframe <- read.xls("path/filename.xls", sheet="sheet name", header=T)

# if the your column headings are not what you want your variables to be in the file
# define vabiable names lists
varlist=c("var1","var2", "var3")
# input data file
dframe <- read.xls("path/filename.xls", sheet="sheet name", header=F, skip=1, col.names=varlist)



# IMPORT XLSX FILE
library(xlsx)
dframe <- read.xlsx("path/filename.xlsx",  sheetName="sheet name", header=T, stringsAsFactors=F)
# seems to be slow in reading

# another method
library(gdata)
installXLSXsupport(perl = "perl", verbose = FALSE)
dframe <- read.xls("path/filename.xlsx", sheet="sheet name", header=T)
# also seems to be super slow



# IMPORT DBF FILE
library(foreign)
dframe <- read.dbf("path/filename.dbf")



# IMPORT TXT FILE
dframe <- read.table("path/filename.txt", header=T, sep="", row.names=NULL)




# common options for importing
# define what constitutes a missing value in you file (one or more)
na.strings=c(".", "")
# skip rows at the top
skip=3
# define whether a header exists from which to take variable names
header=T # or F
# all string variables will be read in as character not factor
stringsAsFactors=F




# EXPORT CSV FILE
write.csv(dframe, file="path/filename.csv", row.names=F, na="")


# common options for exporting
# suppress the creation of a variable with row names (sequential number, usually)
row.names=F
# specify what missing values should be if you don't want them to be "NA"
na="" # empty quotes are blanks, or ".", "m", "-9999" or whatever
