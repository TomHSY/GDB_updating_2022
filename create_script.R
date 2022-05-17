library(raster)

#Choose countries you want to download/update (Have to think about time and space before, huge quantities of data)
countries <- c("Rwanda", "Uganda", "Kenya", "Ethiopia")

for(c in countries){
  if(!dir.exists(c)) dir.create(c, recursive = TRUE)
}

# Using the 2020 population dataset to get precise boundaries for the countries
pop2020 <- list()
existing_files <- which(file.exists(paste0(countries, "/WorldPop/100m/pop/pop2020.tif")))
for(i in existing_files){
  pop2020[[countries[i]]] <- raster(paste0(countries[i], "/WorldPop/100m/pop/pop2020.tif"))
}
for(c in countries[-existing_files]){
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/", toupper(substr(c, 0, 3)), "/", tolower(substr(c, 0, 3)), "_ppp_2020.tif")
  download.file(file_url, tempFile, mode = "wb")
  pop2020[[c]] <- raster(tempFile)
  file.remove(tempFile)
}


# Using the copper in the soil data to resample NDVI data to exactly 250m resolution (Originally a bit larger?)

copper <- list()
existing_files <- which(file.exists(paste0(countries, "/ISRIC/250m/copper.tif")))
for(i in existing_files){
  copper[[countries[i]]] <- raster(paste0(countries[i], "/ISRIC/250m/copper.tif"))
}
if(length(existing_files)!=length(countries)){ 
  tempFile <- tempfile()
  file_url <- "https://files.isric.org/public/af250m_nutrient/af250m_nutrient_cu_m_agg30cm.tif"
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  for(c in countries[-existing_files]){
    copper[[c]] <- crop(layer, pop2020[[c]])
  }
  file.remove(tempFile)
}

###### ALL COUNTRIES


#################################### 1 km resolution ####################################

##################################################################################
# WorldClim Data
##################################################################################

for(c in countries){
if(!dir.exists(paste0(c, "/WorldClim/1km"))) dir.create(paste0(c, "/WorldClim/1km"), recursive = TRUE)
}

# Minimum Temperature
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tmin.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

for(c in countries){
if(!dir.exists(paste0(c, "/WorldClim/1km/mintemp"))) dir.create(paste0(c, "/WorldClim/1km/mintemp"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_tmin_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_tmin_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("mintemp", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/mintemp/mintemp", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_tmin_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_tmin_", i, ".tif"))
}
file.remove(tempFile)


# Average Temperature
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldClim/1km/avgtemp"))) dir.create(paste0(c, "/WorldClim/1km/avgtemp"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_tavg_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_tavg_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("avgtemp", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/avgtemp/avgtemp", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_tavg_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_tavg_", i, ".tif"))
}
file.remove(tempFile)


# Maximum Temperature
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tmax.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldClim/1km/maxtemp"))) dir.create(paste0(c, "/WorldClim/1km/maxtemp"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_tmax_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_tmax_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("maxtemp", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/maxtemp/maxtemp", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_tmax_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_tmax_", i, ".tif"))
}
file.remove(tempFile)


# Precipitation
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldClim/1km/prec"))) dir.create(paste0(c, "/WorldClim/1km/prec"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_prec_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_prec_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("prec", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/prec/prec", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_prec_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_prec_", i, ".tif"))
}
file.remove(tempFile)


# Wind Speed
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_wind.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldClim/1km/wind"))) dir.create(paste0(c, "/WorldClim/1km/wind"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_wind_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_wind_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("wind", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/wind/wind", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_wind_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_wind_", i, ".tif"))
}
file.remove(tempFile)


# Solar Radiation
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_srad.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldClim/1km/srad"))) dir.create(paste0(c, "/WorldClim/1km/srad"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_srad_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_srad_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("srad", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/srad/srad", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_srad_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_srad_", i, ".tif"))
}
file.remove(tempFile)


# Vapor Pressure
tempFile <- tempfile()
file_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_vapr.zip"

# We can go month-by-month
# Download all files
download.file(file_url, tempFile)

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldClim/1km/vapr"))) dir.create(paste0(c, "/WorldClim/1km/vapr"), recursive = TRUE)
}

for(i in 1:12){
  if(i<10) newfile <- unzip(tempFile, paste0("wc2.1_30s_vapr_0", i, ".tif"))
  else newfile <- unzip(tempFile, paste0("wc2.1_30s_vapr_", i, ".tif"))
  uncropped <- raster(newfile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- paste0("vapr", month[i])
    writeRaster(layer, paste0(c, "/WorldClim/1km/vapr/vapr", month[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  if(i<10) file.remove(paste0("wc2.1_30s_vapr_0", i, ".tif"))
  else file.remove(paste0("wc2.1_30s_vapr_", i, ".tif"))
}
file.remove(tempFile)


#################################### 250m resolution ####################################


##################################################################################
# ISRIC Data
##################################################################################

for(c in countries){
  if(!dir.exists(paste0(c, "/ISRIC/250m"))) dir.create(paste0(c, "/ISRIC/250m"), recursive = TRUE)
}


# Extractable Nutrients

nutrients <- c("aluminium", "boron", "calcium", "copper", "iron", "potassium", "magnesium", "manganese", "nitrogen", "sodium", "phosphorus", "zinc")
nutri <- c("al", "b", "ca", "cu", "fe", "k", "mg", "mn", "n", "na", "p", "zn")

for(i in 1:length(nutrients)){
  tempFile <- tempfile()
  file_url <- paste0("https://files.isric.org/public/af250m_nutrient/af250m_nutrient_", nutri[i], "_m_agg30cm.tif")
  download.file(file_url, tempFile, mode = "wb")
  uncropped <- raster(tempFile)
  for(c in countries){
    layer <- crop(uncropped, pop2020[[c]])
    names(layer) <- nutrients[i]
    writeRaster(layer, paste0(c, "/ISRIC/250m/", nutrients[i], ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
  }
  rm(layer)
  rm(uncropped)
  file.remove(tempFile)
}


##################################################################################
# GLAM Data (NDVI)
##################################################################################

#cur_year <- as.numeric(substr(Sys.Date(), 0, 4))
#cur_day <- as.numeric(Sys.Date() - as.Date(paste0(cur_year, "-01-01")))

for(c in countries){
  if(!dir.exists(paste0(c, "/GLAM/250m/ndvi"))) dir.create(paste0(c, "/GLAM/250m/ndvi"), recursive = TRUE)
  if(!dir.exists(paste0(c, "/GLAM/250m/ndviano"))) dir.create(paste0(c, "/GLAM/250m/ndviano"), recursive = TRUE)
}

years <- seq(2020, 2020)
days <- c('001', '017', '033', '049', '065', '081', '097',
          '113', '129', '145', '161', '177', '193', '209',
          '225', '241', '257', '273', '289', '305', '321', '337', '353')


#if no Ethiopia in countries
for(y in years){
  for(d in days){
    tempFile <- tempfile()
    file_url <- paste0("http://pekko.geog.umd.edu/usda/apps/delivery/africa_east2/", y, d, "/africa_east2.", y, d,".MOD44CQ.250m.tif")
    download.file(file_url, tempFile, mode = "wb")
    uncropped <- raster(tempFile)
    for(c in countries){
      layer <- resample(uncropped, copper[[c]])
      writeRaster(layer, paste0(c, "/GLAM/250m/ndvi/ndvi", y, d, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
    }
    rm(layer)
    rm(uncropped)
    file.remove(tempFile)
    tempFile <- tempfile()
    file_url <- paste0("http://pekko.geog.umd.edu/usda/apps/delivery/africa_east2/", y, d, "/africa_east2.", y, d,".MOD44CQ.250m.anomaly.tif")
    download.file(file_url, tempFile, mode = "wb")
    uncropped <- raster(tempFile)
    for(c in countries){
      layer <- resample(uncropped, copper[[c]])
      writeRaster(layer, paste0(c, "/GLAM/250m/ndviano/ndviano", y, d, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
    }
    rm(layer)
    rm(uncropped)
    file.remove(tempFile)
  }
}



#If Ethiopia in countries
for(y in years){
  for(d in days){
    tempFile <- tempfile()
    file_url <- paste0("http://pekko.geog.umd.edu/usda/apps/delivery/africa_icpac/", y, d, "/africa_icpac.", y, d,".MOD44CQ.250m.tif")
    download.file(file_url, tempFile, mode = "wb")
    uncropped <- raster(tempFile)
    for(c in countries){
      layer <- resample(uncropped, copper[[c]])
      writeRaster(layer, paste0(c, "/GLAM/250m/ndvi/ndvi", y, d, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
    }
    rm(layer)
    rm(uncropped)
    file.remove(tempFile)
    tempFile <- tempfile()
    file_url <- paste0("http://pekko.geog.umd.edu/usda/apps/delivery/africa_icpac/", y, d, "/africa_icpac.", y, d,".MOD44CQ.250m.anomaly.tif")
    download.file(file_url, tempFile, mode = "wb")
    uncropped <- raster(tempFile)
    for(c in countries){
      layer <- resample(uncropped, copper[[c]])
      writeRaster(layer, paste0(c, "/GLAM/250m/ndviano/ndviano", y, d, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
    }
    rm(layer)
    rm(uncropped)
    file.remove(tempFile)
  }
}


#################################### 100m resolution ####################################


##################################################################################
# WorldPop data
##################################################################################
tic()
# Annual population

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldPop/100m/pop"))) dir.create(paste0(c, "/WorldPop/100m/pop"), recursive = TRUE)
}

for(y in 2000:2020){
  for(c in countries){
    tempFile <- tempfile()
    file_url <- paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/", y, "/", toupper(substr(c, 0, 3)), "/", tolower(substr(c, 0, 3)), "_ppp_", y, ".tif")
    download.file(file_url, tempFile, mode = "wb")
    layer <- raster(tempFile)
    names(layer) <- paste0("pop", y)
    writeRaster(layer, paste0(c, "/WorldPop/100m/pop/pop", y, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
    rm(layer)
    file.remove(tempFile)
  }
}


# Distance to edges of ESA-CCI-LC classes

classes <- c("cultivated", "woodytree", "shrub", "herb", "sparse", "aquaticveg", "artificial", "barearea")
cla <- c("011", "040", "130", "140", "150", "160", "190", "200")

for(c in countries){
  for(cl in classes){
    if(!dir.exists(paste0(c, "/WorldPop/100m/", cl))) dir.create(paste0(c, "/WorldPop/100m/", cl), recursive = TRUE)
  }
}

for(y in 2000:2015){
  for(i in 1:8){
    for(c in countries){
      tempFile <- tempfile()
      file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/ESA_CCI_Annual/", y, "/", tolower(substr(c, 0, 3)), "_esaccilc_dst", cla[i], "_100m_", y, ".tif")
      download.file(file_url, tempFile, mode = "wb")
      layer <- raster(tempFile)
      names(layer) <- paste0(classes[i], y, ".tif")
      writeRaster(layer, paste0(c, "/WorldPop/100m/", classes[i], "/", classes[i], y, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
      rm(layer)
      file.remove(tempFile)
    }
  }
}


for(c in countries){
  tempFile <- tempfile()
  file_url <-  paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/ESA_CCI_Water/DST/", tolower(substr(c, 0, 3)), "_esaccilc_dst_water_100m_2000_2012.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "inlandwater2012"
  writeRaster(layer, paste0(c, "/WorldPop/100m/inlandwater2012.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}


for(c in countries){
  tempFile <- tempfile()
  file_url <-  tempFile <- tempfile()
  file_url <-  paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/Coastline/DST/", tolower(substr(c, 0, 3)), "_dst_coastline_100m_2000_2020.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "coastline2020"
  writeRaster(layer, paste0(c, "/WorldPop/100m/coastline2020.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}


# Population Age Structure
tic()
ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
age_labels <- c('infant', '1to4', '5to9', '10to14',
                '15to19', '20to24', '25to29', '30to34',
                '35to39', '40to44', '45to49', '50to54',
                '55to59', '60to64', '65to69', '70to74',
                '75to79', '80plus')



for(c in countries){
  for(g in c("m", "f")){
    for(a in age_labels){
      if(!dir.exists(paste0(c, "/WorldPop/100m/", g, a))) dir.create(paste0(c, "/WorldPop/100m/", g, a), recursive = TRUE)
    }
  }
}


for(g in c("m", "f")){
  for(y in 2015:2020){
    for(i in 1:length(ages)){
      for(c in countries){
        tempFile <- tempfile()
        file_url <- paste0("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/", y, "/", toupper(substr(c, 0, 3)), "/", tolower(substr(c, 0, 3)), "_", g, "_", ages[i], "_", y,".tif")
        download.file(file_url, tempFile, mode = "wb")
        layer <- raster(tempFile)
        names(layer) <- paste0(g, age_labels[i], y)
        writeRaster(layer, paste0(c, "/WorldPop/100m/", g, age_labels[i], "/", g, age_labels[i], y, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
        rm(layer)
        file.remove(tempFile)
      }
    }
  }
}

# Settlements

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldPop/100m/settlement"))) dir.create(paste0(c, "/WorldPop/100m/settlement"), recursive = TRUE)
}

for(y in c(2001:2011, 2013, 2015:2020)){
  for(c in countries){
    tempFile <- tempfile()
    if(y < 2015) file_url <- paste0("https://data.worldpop.org/GIS/Global_Settlement_Growth/Individual_countries/", toupper(substr(c, 0, 3)), "/v0a/", tolower(substr(c, 0, 3)), "_bsgmi_v0a_100m_", y, ".tif")
    else file_url <- paste0("https://data.worldpop.org/GIS/Global_Settlement_Growth/Individual_countries/", toupper(substr(c, 0, 3)), "/v0a/", tolower(substr(c, 0, 3)), "_bsgme_v0a_100m_", y, ".tif")
    download.file(file_url, tempFile, mode = "wb")
    layer <- raster(tempFile)
    names(layer) <- paste0("settlement", y)
    writeRaster(layer, paste0(c, "/WorldPop/100m/settlement/settlement", y, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
    rm(layer)
    file.remove(tempFile)
  }
}


# Distance to major intersections

for(c in countries){
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/OSM/DST/", tolower(substr(c, 0, 3)),"_osm_dst_roadintersec_100m_2016.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "intersect2016"
  writeRaster(layer, paste0(c, "/WorldPop/100m/intersect2016.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}

# Distance to major roads

for(c in countries){
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/OSM/DST/", tolower(substr(c, 0, 3)), "_osm_dst_road_100m_2016.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "road2016"
  writeRaster(layer, paste0(c, "/WorldPop/100m/road2016.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}

# Distance to major waterways

for(c in countries){
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/OSM/DST/", tolower(substr(c, 0, 3)), "_osm_dst_waterway_100m_2016.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "waterway2016"
  writeRaster(layer, paste0(c, "/WorldPop/100m/waterway2016.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}

# Elevation

for(c in countries){
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/Topo/", tolower(substr(c, 0, 3)), "_srtm_topo_100m.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "elevation2000"
  writeRaster(layer, paste0(c, "/WorldPop/100m/elevation2000.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}

# Slope

for(c in countries){
  tempFile <- tempfile()
  file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)), "/Slope/", tolower(substr(c, 0, 3)), "_srtm_slope_100m.tif")
  download.file(file_url, tempFile, mode = "wb")
  layer <- raster(tempFile)
  names(layer) <- "slope2000"
  writeRaster(layer, paste0(c, "/WorldPop/100m/slope2000.tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
  rm(layer)
  file.remove(tempFile)
}

# Nightime lights

for(c in countries){
  if(!dir.exists(paste0(c, "/WorldPop/100m/nightlight"))) dir.create(paste0(c, "/WorldPop/100m/nightlight"), recursive = TRUE)
}

for(y in 2012:2016){
  for(c in countries){
    tempFile <- tempfile()
    file_url <- paste0("https://data.worldpop.org/GIS/Covariates/Global_2000_2020/", toupper(substr(c, 0, 3)) ,"/VIIRS//", tolower(substr(c, 0, 3)),"_viirs_100m_", y, ".tif")
    download.file(file_url, tempFile, mode = "wb")
    layer <- raster(tempFile)
    names(layer) <- paste0("nightlight", y)
    writeRaster(layer, paste0(c, "/WorldPop/100m/nightlight/nightlight", y, ".tif"), options = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
    rm(layer)
    file.remove(tempFile)
  }
}


### RWANDA SPECIFIC


### UGANDA SPECIFIC


### KENYA SPECIFIC


### ETHIOPIA SPECIFIC