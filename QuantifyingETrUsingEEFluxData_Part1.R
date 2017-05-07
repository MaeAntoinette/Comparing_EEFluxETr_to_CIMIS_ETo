#Quantifying ETr Using EEFlux Data. 
#Date: 09 April 2017 

#------------- Background and Objective ------------
#Background: Quantifying the amount of ET in agricultural fields can be costly and time-intensive. FAO standards that allow farmers
#to quantify ET without in-situ ET measurements include using a "reference ET" (ETr/ETo) of a well-watered, thriving crop. ETr = alfalfa
#ETo = short-grass (commonly used in California). The ET of a given crop is then estimated using a "crop coefficient" (Kc) which accocunts 
#for biophysical parameters of the given crop type. The EEFlux model uses Landsat imagery to calculate the surface-energy balance and actual ET 
#and incorporates the crop coefficient method, where 24-hr ETa = ETr * ETrF (ETrF is known as the "fraction of reference ET) and is synonymous 
#with the Kc. EEFlux provides a daily ETrF map which is useful to understand ET relative to the reference across a landscape. 

#Objective: EEFlux SEB estimates rely on the amount of incoming Rn and consequently the ETr. I will determine ETr, using 24-hr ETrF 
#and ET maps. Then, I will compare the pixel values to ground-observed CIMIS ETo values recorded across the study area.


#------------- Script to Create ETr maps ------------------------------

#Identify map year
map.year <- "2010"

#List files of interest 
ETrF.fold <- paste0("F:/EEFlux/P39R37/",map.year,"/ETrF/Clipped/")
ETrF.files <- list.files(ETrF.fold, patt=".tif$|.img$", all.files = TRUE, full.names = TRUE)

ET.fold <- paste0("F:/EEFlux/P39R37/",map.year,"/ET/InterpDaily_Maps/") #Similarly do the same for your ET images.
ET.files <- list.files(ET.fold, patt=".tif$|.img$", all.files = TRUE, full.names = TRUE)
ET.files <- ET.files[grep("_me",ET.files)] #for some years this will be in the extension.



#Create ETr files (outfile names)
ETr.files <- paste0("F:/EEFlux/P39R37/",map.year,"/","ETr/",substr(ET.files,43,52)) #Set your output folder location
extension(ETr.files) <- ".img"

#Clipping Loop. This will save in your output directory. 
for (f in 1:length(ET.files)) {
  ETrF.r <- raster(ETrF.files[f])
  ET.r <- raster(ET.files[f])
  ETr.r <- ET.r/ETrF.r
  rw <- writeRaster(ETr.r, ETr.files[f], overwrite=TRUE)
  
  print(paste(f, "of" ,length(ET.files)))
  flush.console()
  
}


#------------ ETr Analysis ------------

ETr.path <- "F:/EEFlux/P39R37/"
ETr.fold <- "/ETr/"

#Bring in your ETr maps.
ETr.2010 <- list.files(paste0(ETr.path,"2010",ETr.fold),pattern = ".tif$|.img$",full.names = TRUE)
ETr.2011 <- list.files(paste0(ETr.path,"2011",ETr.fold),pattern = ".tif$|.img$",full.names = TRUE)
ETr.2012 <- list.files(paste0(ETr.path,"2012",ETr.fold),pattern = ".tif$|.img$",full.names = TRUE)
ETr.2013 <- list.files(paste0(ETr.path,"2013",ETr.fold),pattern = ".tif$|.img$",full.names = TRUE)
ETr.2014 <- list.files(paste0(ETr.path,"2014",ETr.fold),pattern = ".tif$|.img$",full.names = TRUE)
ETr.2015 <- list.files(paste0(ETr.path,"2015",ETr.fold),pattern = ".tif$|.img$",full.names = TRUE)

ETr.stack <- stack(c(ETr.2010,ETr.2011,ETr.2012,ETr.2013,ETr.2014,ETr.2015))

#Bring in your shp of weather stations. I used CIMIS. 
CIMIS.pn <- "F:\\CIMIS"
CIMIS.fn <- "CIMIS_IV_Stations"
CIMIS.shp <- readOGR(CIMIS.pn,CIMIS.fn)
CIMIS.shp@data #check the data.

#Extract ETr data using the CIMIS shp.
EEFlux.ETr <- extract(ETr.stack,CIMIS.shp)

#Transpose your ETr data and place in dataframe.
EEFlux.ETr.t <- t(EEFlux.ETr)
Image.Date <- as.character(substr(colnames(EEFlux.ETr),2,11))
EEFlux.ETr.df <- data.frame(Image.Date,EEFlux.ETr.t)
names(EEFlux.ETr.df) <- c("Date","xWestmorland","xCalipatriaMulberry","xMeloland","xSeeley")
EEFlux.ETr.df$Date <- gsub("_","-",EEFlux.ETr.df$Date)
EEFlux.ETr.df$Date <- as.Date(EEFlux.ETr.df$Date,format = "%Y-%m-%d") #convert to date format.


#Attach ETr values to a timeline with consecutive dates. 
ETr.Dates <- seq.Date(as.Date.character("2010-01-01"),as.Date.character("2015-12-31"), by="day")
ETr.Dates
ETr.Timeline <- data.frame(ETr.Dates)
names(ETr.Timeline) <- "Date"
class(ETr.Timeline)

ETr.Timeline <- merge(ETr.Timeline,EEFlux.ETr.df, all = TRUE)
ETr.Timeline[871,4] <- 7.801134


