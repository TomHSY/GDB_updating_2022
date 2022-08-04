
# Script used to update the Geospatial Data Backbone (GDB) :
# - to include the most recent temporal observations of already-present variables
# - to include new variables that were not present before

# See the documentation related to the updating process at the following URL :
# https://docs.google.com/document/d/1JXn8sNHV4MkjccVh2VsKN8qekz0f5Im3XbE50VWfYoU/edit#

### Packages ###################################################################
library(raster)
library(stringr)
library(rgdal)

### Setup ######################################################################
setwd("/srv/geospatial_dashboard/dashboard/Data")
options(timeout = max(1000000, getOption("timeout")))

### Parameters for the update ##################################################

#countries that will be updated : all by default
all_countries <- c("Ethiopia", "Kenya", "Rwanda", "Uganda", "Tanzania")

#year range for the downloaded data : by default, from 2000 until the current year
# (if data already exists for a given year, it will not be downloaded again)
start_year <- 2000
current_year <- as.numeric(strsplit(as.character(Sys.Date()), '-')[[1]][1])
year_range <- start_year:current_year

#time range every 16 days (for ndvi data) : based on the year range above
range_16 <- sapply(year_range, function(y){paste0(as.character(y), str_pad(seq(1,353,16), 3, pad = "0"))})


### Functions ##################################################################

worldpoptag <- function(country){
  # Function that determines which country tag to used based on the country.
  # (The country tag is the one present in download links for WorldPop data)
  # Inputs : country : character indicating the country
  # Output : the corresponding tag (character)
  
  switch(country,
         "Tanzania"="TZA",
         "Ethiopia"="ETH",
         "Uganda"="UGA",
         "Kenya"="KEN",
         "Rwanda"="RWA")
}

determine_start_date <- function(src){
  # Function that determines the starting date for temporal observation of
  # variables, depending on the source of the data. (For some data, it is not
  # always relevant to include all observations, and it takes up space)
  # Inputs : src : character indicating the source
  # Output : the corresponding starting date (numeric)
  
  switch(src, 
         "GLAM"=2015, 
         "WorldPop"=2010)
}

generate_ziptag <- function(filename, zipinfo){
  # Function that determines how to handle downloaded zip files. For example, 
  # the WorldClim data are downloaded as zip files. 12 raster files are extracted
  # from 1 zip file (the 12 monthly observations of the variable, averaged over years)/
  # In that case, the program needs to know which extracted file corresponds to 
  # which month of the year. This is the purpose of this function.
  # The input argument zipinfo, present as a column in variables.csv, specifies how 
  # data needs to be processed. If "month" is given as zipinfo, the program will know 
  # that the extracted 12 filenames will contain numbers (01-12) corresponding to months
  # Inputs : zipinfo : character indicating a special tag, specifying how data 
  #          needs to be processed in the function.
  #          filename : character vector containing the names of the extracted files
  # Output : the corresponding tag (character)
  
  if (zipinfo=='month'){
    
    month_no <- substr(filename, nchar(filename)-5, nchar(filename)-4)
    
    s <- switch(month_no,
                "01"="jan",
                "02"="feb",
                "03"="mar",
                "04"="apr",
                "05"="may", 
                "06"="jun", 
                "07"="jul", 
                "08"="aug", 
                "09"="sep", 
                "10"="oct", 
                "11"="nov", 
                "12"="dec")
    
    return(s)
  }
  
}

generate_ziptag2 <- function(zipinfo){
  # Function that determines the zip time tags based on the value of zipinfo. Used
  # in the function process_csv_file to fill up the time field if zipinfo != NA
  # Inputs : zipinfo : character indicating a special tag
  # Output : the corresponding vector of time tags (character)
  
  switch(zipinfo, 
         "month"=c("jan","feb","mar","apr","may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
}

expand_line <- function(init_lines, condition, vector, FUN1, FUN2){
  # Function that allows to "multiply" some lines of "df" (see process_csv_file)
  # based on a condition and a vector.
  # Inputs : init_lines, the initial line(s) present in the process_csv_file function, 
  #             that will be expanded.
  #          condition, character that will be evaluated as a logical expression.
  #             This condition tells whether lines need to be multiplied or not.
  #          vector, a character vector whose elements are the base for the line
  #             multiplication (example : countries)
  #          FUN1, function indicating how the multiplied lines are created from
  #             the original line, in case the condition is met
  #          FUN2, function indicating how the original line needs to be modified
  #             in case the condition is not met
  # Output : the expanded lines
  
  #initialisation of the new expanded data as an empty list
  list_exp <- list()
  
  #iterating over rows of init_lines
  for (i in 1:nrow(init_lines)){
    line <- init_lines[i, ] #current line

    #evaluating the condition (example : "grepl('COUNTRYTAG', line$url)", in other
    # words : "the variable URL contains COUNTRYTAG", indicating that this line needs
    # to be "multiplied" by 5, for the 5 African countries).
    condition <- eval(parse(text=condition))
    
    if (condition){ #if the condition is met
      
      list_exp2 <- list()
      
      if (length(vector)==1) vector <- eval(parse(text=vector))
      
      #for each element of the vector (ex: countries), a new line is created and
      # appended to list_exp2, which then will contain the new multiplied lines.
      for (k in vector){
        new_line <- line #copy
        
        #each line is multiplied thanks to the FUN1 function, given as an argument
        new_line <- FUN1(new_line, k)
        list_exp2[[length(list_exp2)+1]] <- new_line
      }
      
      #converting the list to a dataframe, containing the new multiplied lines
      new_lines <- do.call(rbind, list_exp2) 
  
    } else { #if the condition is not met (the line does not need multiplication)
      new_line <- line
      new_lines <- FUN2(new_line) # a different treatment is applied (given by FUN2)
    }
    
  list_exp[[length(list_exp)+1]] <- new_lines #appending the multiplied lines to the list
  }
  
  expanded <- do.call(rbind, list_exp) #converting the list to a dataframe :
  # the expanded version of the initial "init_lines"
  
  return(expanded)
}

process_csv_file <- function(var, countries, year_range, range_16){
  # Function that processes the variables.csv file : iterating over rows, it
  # multiplies the necessary rows  and then compares the data to the GDB to skip 
  # already-present variables and their observations.
  # Inputs : var : the variables.csv file read as a dataframe
  #          countries : character vector indicating the countries that need updating
  #          year_range : character vector indicating the selected year range
  #          range_16 : character vector indicating the selected time range for ndvi data
  # Output : df, the dataframe containing exactly the data to download
  
  #initialisation as an empty list
  df <- list()
  
  #copy
  global_year_range <- year_range
  
  #iterating over rows of variables.csv
  for (i in 1:nrow(var)){
    line <- var[i,]
    
    #the starting date is determined according to the source
    start_date <- determine_start_date(line$source)
    
    #if no specific starting date is specified for the source, it is set as the
    #starting date of the year range (2000 by default)
    if (is.null(start_date)) start_date <- global_year_range[1]
    
    #the 2 vectors are updated with the new starting date
    year_range <- seq(start_date, global_year_range[length(global_year_range)])
    range_16 <- sapply(year_range, function(y){paste0(as.character(y), str_pad(seq(1,353,16), 3, pad = "0"))})
    
    ### the 5 following calls allow the "multiply" the lines of df, each based on a condition
    
    #multiplying lines of variables from the WorldPop source, with country-specific
    # URLs, replacing each one of them by 5 new lines (for the 5 countries)
    line <- expand_line(line, 
                      condition = "grepl('COUNTRYTAG', line$url)",
                      vector = countries,
                      
                      FUN1 = function(x, c){

                          tag <- worldpoptag(c) #getting the right tag
                          
                          #replacing the generic "countrytag" indication by the real tag
                          new_link <- gsub('COUNTRYTAG', tag, x$url)
                          new_link <- gsub('countrytag', tolower(tag), new_link)
                          
                          x$url <- new_link
                          x$country <- c #identifying the line with the right country
                          x
                      }, 
                      
                      FUN2 = function(x){
                        x$country <- NA #in case the URL is not country-specific
                        x
                      }
                      )
    
    #multiplying lines of variables with URLs indicating the year, replacing 
    # each one of them by new lines (as many lines as there are years in the year range)
    line <- expand_line(line, 
                      condition = "grepl('YEAR', line$url)",
                      vector = year_range,
                      
                      FUN1 = function(x, y){
                        #replacing the YEAR generic URL tag by the actual year y
                        x$url <- gsub('YEAR', as.character(y), x$url) 
                        x$time <- y
                        x
                      }, 
                      
                      FUN2 = function(x){
                        x$time <- NA #in case the URL doesn't contain a year
                        x
                      }
    )

    #multiplying lines of variables with URLs indicating the 16-days time tag, replacing 
    # each one of them by new lines (as many lines as there are elements in range_16)
    line <- expand_line(line, 
                      condition = "grepl('16DAYS', line$url)",
                      vector = range_16,
                      
                      FUN1 = function(x, s){
                        #replacing the 16DAYS generic URL tag by the actual tag (ex: 2015017)
                        x$url <- gsub('16DAYS', as.character(s), x$url)
                        x$time <- s
                        x
                      }, 
                      
                      FUN2 = function(x){
                        if(is.null(x$time)) x$time <- NA #in case the URL doesn't contain "16DAYS"
                        x
                      }
    )
    
    #multiplying lines of variables that do not have country-specific URLs. Up
    # to this point, those lines have a empty "country" field. This call allows
    # to fill up that gap with the 5 countries
    line <- expand_line(line, 
                      condition = "is.na(line$country)",
                      vector = countries,
                      
                      FUN1 = function(x, c){
                        x$country <- c #filling the country column
                        x$country_spec <- FALSE #logical indicating that the variable
                        #is not country-specific (that information needs to be retained
                        #for the following steps of the updating process)
                        x
                      }, 
                      
                      FUN2 = function(x){
                        x$country_spec <- TRUE #logical indicating that the variable
                        #is country-specific (that information needs to be retained
                        #for the following steps of the updating process)
                        x
                      }
    )
    
    #multiplying lines of variables (WorldClim) that have a non-empty "zipinfo" field.
    #Based on the value of "zipinfo", lines are multiplied to get the right tag
    # in their "time" field
    line <- expand_line(line, 
                      condition = "!is.na(line$zipinfo)",
                      vector = "generate_ziptag2(line$zipinfo)",
                      
                      FUN1 = function(x, t){
                        x$time <- t #adding the ziptag to the time field (ex: months)
                        x
                      }, 
                      
                      FUN2 = function(x){
                        x #if there's no zipinfo, nothing needs to be done
                      }
    )
    
    ### The rest of the function is dedicated to remove the lines of df that correspond
    # to already-existing variables and time observations.
    
    to_remove <-c() #empty vector for init
    
    #iterating over the expanded line (dataframe)
    for (j in 1:nrow(line)){
      mult_exists <- file.exists(paste0(line$country[j], "/", line$source[j], "/", line$resolution[j], "/", line$varname[j], "/", line$varname[j], line$time[j],'.tif'))
      sgl_exists <- file.exists(paste0(line$country[j], "/", line$source[j], "/", line$resolution[j], "/", line$varname[j],'.tif'))

      if (mult_exists || sgl_exists){ #if an S or M-variable already exist for the current line, 
        to_remove <- append(to_remove, j) #its index is added to the list
      }
    }
    
    if (!is.null(to_remove)) line <- line[-to_remove, ] #removing the right lines
    
    #changing the column order
    line <- line[,c("varname", "source", "resolution", "country", "country_spec", "time", "url", "zipinfo")]
    
    #adding the expanded line to the df list
    df[[i]] <- line
    
  }
  
  df = do.call(rbind, df) #converting to a dataframe
  
  return(df)
}

write_raster <- function(layer, c, src, res, varname, time, zipinfo, tempFile){
  # Function responsible for writing raster data at the right place in the GDB, 
  # after files are downloaded
  # Inputs : layer : a raster layer object containing the data to write
  #          c : the country (character)
  #          src : the source (character)
  #          res : the resolution (character)
  #          varname : the name of the variable (character)
  #          time : the time tag (year, month, 16days tag) (character)
  #          zipinfo : the zipinfo
  #          tempFile : a temporary file used to store the downloaded data before writing
  
  print('write')
  #for M-variables 
  if ((!is.na(time)) && (!is.na(zipinfo))){
    
    if(!dir.exists(paste0(c, "/", src, "/", res, "/", varname))){
      dir.create(paste0(c, "/", src, "/", res, "/", varname), recursive = TRUE)
    }
    
    sapply(layer, function(x){
      writeRaster(x, paste0(c, "/", src, "/", res, "/", varname, "/", varname, time, ".tif"),
                  options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
    })
    
  #for M-variables with zipinfo
  } else if (!is.na(zipinfo)){
    
    if(!dir.exists(paste0(c, "/", src, "/", res, "/", varname))){
      dir.create(paste0(c, "/", src, "/", res, "/", varname), recursive = TRUE)
    }
    
    for (i in 1:length(layer)){
      ziptag <- generate_ziptag(names(layer)[i], zipinfo)
      
      writeRaster(x, paste0(c, "/", src, "/", res, "/", varname, "/", varname,ziptag,".tif"),
                  options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
    }
    
  # for S-variables
  } else {
    
    if(!dir.exists(paste0(c, "/", src, "/", res))){
      dir.create(paste0(c, "/", src, "/", res), recursive = TRUE)
    }
    
    sapply(layer, function(x){
      writeRaster(x, paste0(c, "/", src, "/", res, "/", varname, ".tif"),
                  options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)})
  }
  
  # remove files and objects
  rm(layer)
  file.remove(tempFile)
}


### Main script ################################################################

###### PART 1 : fetching and organizing to-be-downloaded data, based on the input
#parameters and the data already present in the GDB

#reading the variables.csv file
var <- read.csv("variables.csv")

#call to the process csv_file to get the data that needs to be downloaded (df)
df <- process_csv_file(var, all_countries, year_range, range_16)


###### PART 2 : downloading the data

### PART 2.1 : preliminary step, pop2020 / copper rasters are downloaded first 
# because they are needed to crop / resample the geospatial data (respectively)

# Using the 2020 population dataset to get precise boundaries for the countries
pop2020 <- list()

for(c in all_countries){
  tag <- worldpoptag(c)
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/", tag, "/", tolower(tag), "_ppp_2020.tif")
  download.file(file_url, tempFile, mode = "wb")
  pop2020[[c]] <- raster(tempFile)
  file.remove(tempFile)
}


# Using the copper in the soil data to resample NDVI data to exactly 250m resolution (Originally a bit larger?)
copper <- list()

tempFile <- tempfile()
file_url <- "https://files.isric.org/public/af250m_nutrient/af250m_nutrient_cu_m_agg30cm.tif"
download.file(file_url, tempFile, mode = "wb")
layer <- raster(tempFile)
for(c in all_countries){
  copper[[c]] <- crop(layer, pop2020[[c]])
}
file.remove(tempFile)


### PART 2.2 : data in the "df" dataframe is downloaded and added to the GDB
# at the right place in the Data folder

#potential errors are stored in the "errors" list
errors <- list()

to_skip <- FALSE

#iterating over rows of df
for (i in 1:50){
  
  print(paste('---------', i, '/', nrow(df)))
  
  #extracting info from the current row
  varname <- df$varname[i]
  src <- df$source[i]
  res <- df$resolution[i]
  url <- df$url[i]
  country <- df$country[i]
  country_spec <- eval(parse(text=df$country_spec[i]))
  time <- df$time[i]
  zipinfo <- df$zipinfo[i]

  ### download
  tempFile <- tempfile()
  result_download <- try(download.file(url, tempFile, mode = "wb"), silent=T)
  
  #reporting the download error
  if (class(result_download)=='try-error'){
    print(paste(url,'could not be downloaded : this specific time observation likely does not exist yet (too recent)'))
    errors[[length(errors)+1]] <- data.frame(varname=varname,
                                             country=country,
                                             time=time,
                                             error_type=as.character(result_download))
  }
  
  else {
    
    ### reading the downloaded files as raster layers
    
    result_unzip <- try(unzip(tempFile, list=T), silent=T)
    
    #if unzipping failed (= the file is not a zip file)
    if (class(result_unzip)=='try-error'){
      
      #reading the file as a raster directly
      layer <- list(try(raster(tempFile)))
      
    } else { #the file is a zip file
      filenames <- result_unzip
      layer <- sapply(unzip(tempFile), function(x){try(raster(x))}, USE.NAMES=F)
      names(layer) <- filenames$Name
    }
    
    if (class(layer[[length(layer)]])!='try-error'){
      
      if (!country_spec){ #if the variable is not country-specific, it needs cropping
        
        ### cropping using the boundaries of pop2020 data
        layer <- sapply(layer, USE.NAMES=F, function(x){crop(x, pop2020[[country]])})
      }
      
      ### re-sampling using ndvi data
      if (varname %in% c('ndvi', 'ndviano')){
        layer <- sapply(layer, USE.NAMES=F, function(x){resample(x, copper[[country]])})
      }
      
      ### writing data
      if ((!to_skip)){
        write_raster(layer, country, src, res, varname, time, zipinfo, tempFile)
      }
      
      # skipping the next line if the values of varname, country and zipinfo are
      #the same as the current line's. This allows to skip downloading a zip file
      #over and over again, since 1 zip file will contain all monthly observations
      #for WorldClim data
      if ((i!=nrow(df))&(!is.na(zipinfo))&(zipinfo=='month')){
        if ((varname==df$varname[i+1]) &&
            (country==df$country[i+1]) &&
            (zipinfo==df$zipinfo[i+1])) to_skip <- TRUE
        else to_skip <- FALSE
      }
      print(to_skip)
    #reporting the following error
    } else errors[[length(errors)+1]] <- data.frame(varname=varname,
                                                    country=country,
                                                    time=time,
                                                    error_type="Cannot create a RasterLayer object from this file.")
    
  }
}

errors <- do.call(rbind, errors)

