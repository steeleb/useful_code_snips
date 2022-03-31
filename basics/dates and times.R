setwd("//LINDSEYA1/Users/elliotta/Dropbox/travel/R helps")
raw <- read.csv("datetest.csv", header=TRUE, sep=",")
  str(raw)
  # convert dates read in as a factor in form m/d/y to a date format
  raw$date1=as.Date(raw$date,"%m/%d/%Y")
  # convert a SAS date9 date read in as a factor to a date in POSIXlt format
  raw$date6=strptime(raw$date5,"%d%b%y")
  # return the day of year from a date variable
  raw$jday1=as.numeric(format(raw$date1, "%j"))
  # return the year from a date variable
  raw$year1=as.numeric(format(raw$date1, "%Y"))
  str(raw)

# not to be run:
# to build a date from separate parts
# for year and julian day
dframe$date=as.Date(paste(dframe$dayofyr, dframe$year, sep="-"), "%j-%Y")
# for day, month, year
dframe$date=as.Date(paste(dframe$day, dframe$month, dframe$year, sep="-"), "%d-%m-%Y")

# to build a time and datetime from a numeric time (e.g. 125=1:25, 5=0:05, 1340=13:40) and date
# hours
dframe$hr=trunc(dframe$time/100)
# minutes - might not need the round function if all data behave
dframe$min=round(((all_metar$time/100-all_metar$hr)*100), digits=0)
# time
dframe$time2=paste(dframe$hr, dframe$min, sep=":")
# datetime - might not need time zone if all data behave (got warnings)
dframe$datetime=as.POSIXlt(paste(dframe$date, dframe$time2), "%Y-%m-%d %H:%M", tz="EST")

#or use for-loop
for (i in 1:nrow(buoy1416)) {
  if(nchar(buoy1416$Hr.Min) ==4) { #if the number of characters is 4
    buoy1416$hour <- substr(buoy1416$Hr.Min, 1,2) #then select the first two characters
  } else #assign it to the column
    if(nchar(buoy1416$Hr.Min) ==3) { #if the number of characters is 3
      buoy1416$hour <- substr(buoy1416$Hr.Min, 1, 1) #then select the first character
    } else #assign it to the column
      if(nchar(buoy1416$Hr.Min) ==2) { #if the number of characters is 2
        buoy1416$hour <- '00'} #assign 00 to the column
}

dframe$time2=paste(dframe$hr, dframe$min, sep=":")


# note: R's default origin is 1970-01-01.  If you do math or stats on dates, you can convert them back to dates using:
dframe$date=as.Date(dframe$date, origin="1970-01-01")
# Windows default origin is 1900-01-01 - but see help in as.Date due to windows funkiness


  # format codes (case sensitive:
# %a abbreviated weekday
# %A full weekday
# %b abbreviated month
# %B full month
# %c data and time
# %d numeric day of month
# %H numeric hours (24 hours)
# %I numeric hours (12 hours)
# %j numeric day of year
# %m numeric month
# %M numeric minute
# %p AM/PM
# %S numeric second
# %U numeric week of year (starts Sunday)
# %W numeric week of year (starts Monday)
# %w numeric weekday (0=Sunday)
# %x date
# %X time
# %y two digit year
# %Y four digit year
# %Z time zone text