# bulk read-in of hobo data

# temp/light loggers only ----

dirlist <- list.dirs(datadir, recursive = F) #get directory list of files
dirlist <- dirlist[!grepl('archive', dirlist)] #remove the archive folder

for(i in 1:length(dirlist)){ 
  files = list.files(dirlist[i]) #get list of files in [i] directory
  files = files[!grepl('hobo', files)] #remove the hobo folder
  
  for(j in 1:length(files)){
    df = read.csv(file.path(dirlist[i], files[j]),
                  skip = 1)
    #get TZ
    df$tz = substr(colnames(df)[2], 12, 21)
    
    #harmonize cols
    colnames(df)[1] = 'obsno' #these are always the same
    colnames(df)[2] = 'datetime'
    #find/replace
    cn = colnames(df)
    colnames(df)[grep('temp', cn, ignore.case = T)] = 'waterTemperature_degC'
    colnames(df)[grep('intensity', cn, ignore.case = T)] = 'luminousFlux_lux'
    colnames(df)[grep('batt', cn, ignore.case = T)] = 'batteryVoltage_v'
    colnames(df)[grep('attach', cn, ignore.case = T)] = 'coupAttach'
    colnames(df)[grep('detach', cn, ignore.case = T)] = 'coupDetach'
    colnames(df)[grep('connect', cn, ignore.case = T)] = 'coupConnect'
    colnames(df)[grep('end', cn, ignore.case = T)] = 'EOF'
    colnames(df)[grep('stop', cn, ignore.case = T)] = 'coupStop'
    
    #add metadata
    df$source_file = files[j]
    df$logger = strsplit(gsub("[^[:alnum:] ]", "", read.table(file.path(dirlist[i], files[j]), nrows=1)[1,3]), " +")[[1]]
    
    #format time
    if(df$tz[1] == 'GMT.04.00'){
      timez = 'Etc/GMT+4'
    } else if (df$tz[1] == 'GMT.05.00'){
      timez = 'Etc/GMT+5'
    } else {
      stop('Did not recognize timezone in file ', files[j], '. Fix in code.')
    }
    df$datetime = as.POSIXct(df$datetime, tz = timez, format = '%m/%d/%y %I:%M:%S %p')
    if(j == 1){
      logger_raw <- df
    } else {
      logger_raw <- full_join(logger_raw, df)
    }
  }
  
  if(i ==1){
    PAN_L0 <- logger_raw
  } else {
    PAN_L0 <- full_join(PAN_L0, logger_raw)
  }
}
