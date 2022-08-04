
# Script that updates the COUNTRY_variables CSV files with what's actually
# present in the GDB. If new variables are found, the user will have to manually
# open the CSVs and write variable descriptions and themes.

### Setup ######################################################################
setwd("/srv/geospatial_dashboard/dashboard/Data")

### Functions ##################################################################
determine_time_tag <- function(selected_time, timeunit){
  # Function that creates the time tag for the selected time (year, month, each 16 days) in
  # case the selected_time vector contains several elements, formatting them properly
  # Inputs : selected_time : character vector indicating the selected time (months, years)
  #                          from the nested mselect inputs
  #          timeunit : character indicating the nature of the time (year, month, each 16 days)
  # Outputs : time_tag 
  
  month_format <- function(m){
    switch(m, 'jan'='January',
           'feb'='February',
           'mar'='March',
           'apr'='April', 
           'may'='May', 
           'jun'='June',
           'jul'='July', 
           'aug'='August', 
           'sep'='September', 
           'oct'='October', 
           'nov'='November', 
           'dec'='December')
  }
  #based on timeunit, a chronological list is computed
  if (timeunit=='month'){
    list <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    selected_time <- sapply(selected_time, month_format)
  }
  else if (timeunit=='year') list <- as.character(2000:3000)
  else { 
    list <- c()
    for (year in 2000:3000){
      timeseq <- paste0(as.character(year), str_pad(seq(1,353,16), 3, pad = "0"))
      list <- append(list, timeseq)
    }
  }
  
  #getting the indices of the selected_time vector present in the list
  match <- match(selected_time, list)
  
  #sorting the times (according to the list)
  selected_time <- list[sort(match)] 
  match <- sort(match)
  
  #this logical vector will indicate whether time elements in selected_unit are
  #consecutive or not
  difference <- diff(match) == 1
  
  #initializing with empty vector
  time_tag <- c()
  
  #To make it simple, the following section will iterate over elements of selected_time,
  #appending them to time_tag. However, whenever some elements are consecutive, they will
  #be appended with a more compact form -> instead of 2012_2013_2014_2015, it will be 2012-2015
  
  i <- 1
  while (i <= length(selected_time)){ #iterating over selected time elements
    
    time_tag <- append(time_tag, selected_time[i]) #appending the time to time_tag
    sig <- F #sig = signal whether elements are consecutive, initialized to F
    
    #testing if the current element is consecutive to the next
    while((difference[i]) && (i<=length(difference))){
      #if so, iterating continues (i increases), until the condition is not met
      #basically, it's skipping the in-between years (between start and stop)
      i <- i + 1
      sig <- T #sig becomes TRUE
    }
    
    #if the program went into the while loop above
    if (sig){
      time_tag <- append(time_tag, '-')
      time_tag <- append(time_tag, selected_time[i])
      #the time_tag will get something like 2010-2015 for example
    }
    
    i <- i + 1
  }
  
  #converting the vector to a single character
  time_tag <- paste0(time_tag, collapse='_')
  
  #removing those '_-_' introduced to the tag during the while loops
  time_tag <- gsub('_-_', '-', time_tag)
  time_tag <- gsub('_', ', ', time_tag)
  
  
  return(time_tag)
}

### Script #####################################################################

#extracting the countries' names from the GDB
files <- list.dirs(full.names = FALSE)[-1]
countries <- files[!grepl("/", files)]
countries <- countries[countries != 'shapes']

list_csv <- list()

#iterating the GDB : countries, sources, resolutions
for (country in countries){
  new_csv <- list()
  
  #reading the "old csv"
  old_csv <- read.csv(paste0(country,"/",country,"_variables.csv"))

  files <- list.dirs(paste0(country), full.names = FALSE)[-1]
  sources <- files[!grepl("/", files)]
  
  for (src in sources){
    
    files <- list.dirs(paste0(country,"/",src), full.names = FALSE)[-1]
    resolutions <- files[!grepl("/", files)]
    
    for (res in resolutions){
      
      #listing S and M-variables
      vars_multi <- list.dirs(paste0( country, "/", src, "/", res), full.names = FALSE)[-1]
      vars_unique <- setdiff(list.files(paste0( country, "/", src, "/", res), full.names = FALSE), vars_multi)
      vars_unique <- unlist(sapply(vars_unique, function(x){gsub('.tif','',x)}, USE.NAMES=F))
      
      #S-variables
      for (var in vars_unique){
        
        #if the variable is present in the old csv
        if (var %in% old_csv$Variable.Name){
          old_line <- old_csv[old_csv$Variable.Name==var,]
          new_line <- data.frame(Variable.Name=old_line["Variable.Name"],
                                 Time='',
                                 Description=old_line$Description,
                                 Resolution=old_line$Resolution, 
                                 Source.Website=old_line["Source.Website"], 
                                 Source.Name=old_line["Source.Name"], 
                                 Theme=old_line$Theme)
        } else { #if not
          new_line <- data.frame(Variable.Name=var,
                                 Time='',
                                 Description='',
                                 Resolution=res, 
                                 Source.Website=old_csv[old_csv['Source.Name']==src,"Source.Website"][1], 
                                 Source.Name=src, 
                                 Theme='')
        }
        
        new_csv[[length(new_csv)+1]] <- new_line
      }
      
      #M-variables
      for (var in vars_multi){
        files_multi <- list.files(paste0( country, "/", src, "/", res, "/", var), full.names = FALSE)
        time_obs <- sapply(files_multi, USE.NAMES=F,function(x){
          obs <- gsub('.tif', "",x, fixed=T)
          obs <- gsub(var, "", obs)
          obs
        })

        if (substr(time_obs[1],1,1) %in% letters) sig <- 'month'
        else if (nchar(time_obs[1])==4) sig <- 'year'
        else sig <- 'each_16_days'
          
        vartime <- determine_time_tag(time_obs, sig)
        
        varname <- var
        if (varname %in% old_csv$Variable.Name){        #if the variable is present in the old csv
          old_line <- old_csv[old_csv$Variable.Name==varname,]
          new_line <- data.frame(Variable.Name=old_line["Variable.Name"],
                                 Time=vartime,
                                 Description=old_line$Description,
                                 Resolution=old_line$Resolution, 
                                 Source.Website=old_line["Source.Website"], 
                                 Source.Name=old_line["Source.Name"], 
                                 Theme=old_line$Theme)
        } else { #if not
          new_line <- data.frame(Variable.Name=varname,
                                 Time=vartime,
                                 Description='',
                                 Resolution=res, 
                                 Source.Website=old_csv[old_csv['Source.Name']==src,"Source.Website"][1], 
                                 Source.Name=src, 
                                 Theme='')
        }
        new_csv[[length(new_csv)+1]] <- new_line
      }
      
    }
  }
  new_csv <- do.call(rbind, new_csv)
  list_csv[[length(list_csv)+1]] <- new_csv
}

#writing the csvs
for (i in 1:length(list_csv)){
  write.csv(list_csv[[i]], paste0('Data/', countries[i], '/', countries[i], '_variables.csv'), row.names=F)
}

