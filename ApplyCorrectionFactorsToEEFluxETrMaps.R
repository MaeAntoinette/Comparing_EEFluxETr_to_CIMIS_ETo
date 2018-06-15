#Applying correction factors to each EEFlux ETr map. 

#Bring in ETr maps. 
ETr.mapfolder <- dir(path = "F:/EEFlux/P39R37",patt = "ETr$",all.files = TRUE,full.names = TRUE,recursive = TRUE,
                include.dirs = TRUE)
ETr.maps <- list.files(ETr.mapfolder,pattern = "_me.img$",full.names = TRUE)
ETr.maps

#Bring in daily CF values. Calculated as the average value (ETr divided by ETo) of all CIMIS stations.
CF.file <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_CFValues_By_AcquisitionDate.csv")
CF.file$Date <- as.Date.character(CF.file$Date)
CF.values <- c(CF.file$CF.daily)

#Make sure the dates of ETr maps match the dates of the CF file.
match.df <- setNames(data.frame(as.Date.character(gsub("_","-",substr(ETr.maps,27,36))),CF.file$Date),
                     c("MapDate","CFDate"))
match.df$matching <- all(sapply(match.df, identical, match.df[,1]))


#Create outfile names. 
outfile.pn <- paste0(substr(ETr.maps,0,26),"ETr_Adj/")
outfile.fn <- substr(ETr.maps,27,36)
outfile.names <- paste0(outfile.pn,outfile.fn)
extension(outfile.names) <- ".img"

#Loop, there it is. 
for (i in 1:length(CF.values)){
  ETr <- raster(ETr.maps[i])
  ETr.adj <- ETr * CF.values[i]
  writeRaster(ETr.adj, outfile.names[i],overwrite = TRUE)
  
  print(paste(i, "of" ,length(CF.values)))
  flush.console()
}


#Make ET_Adj maps using the adjusted ETr and ETrF maps. 

#Bring in your ETr_Adj files
AdjETr.mapfolder <- dir(path = "F:/EEFlux/P39R37",patt = "ETr_Adj$",all.files = TRUE,full.names = TRUE,recursive = TRUE,
                     include.dirs = TRUE)
AdjETr.maps <- list.files(AdjETr.mapfolder,pattern = ".img$",full.names = TRUE)
AdjETr.maps

#Bring in your ETrF files
ETrF.mapfolder <- dir(path = "F:/EEFlux/P39R37",patt = "ETrF$",all.files = TRUE, full.names = TRUE, recursive = TRUE,
                      include.dirs = TRUE)
ETrF.mapfolder <- dir(path = ETrF.mapfolder, patt = "InterpDaily_Maps",all.files = TRUE, full.names = TRUE, recursive = TRUE,
                      include.dirs = TRUE)

ETrF.maps <- list.files(ETrF.mapfolder,patt = "me.img$|me.tif$",full.names = TRUE)
ETrF.maps


#Match the date names. 
#Make sure the dates of ETr maps match the dates of the CF file.
match.df <- setNames(data.frame(as.Date.character(gsub("_","-",substr(AdjETr.maps,35,44))),as.Date.character(gsub("_","-",substr(ETrF.maps,45,54)))),
                     c("ETrDate","ETrFDate"))
match.df$matching <- all(sapply(match.df, identical, match.df[,1]))

#make an output directory 
#Create outfile names. 
outfile.pn <- paste0(substr(ETrF.maps,0,24),"/ET_Adj/")
outfile.fn <- substr(ETrF.maps,45,54)
outfile.names <- paste0(outfile.pn,outfile.fn)
extension(outfile.names) <- ".img"

#Multiply them. This will save in your output directory. 
for (f in 1:length(outfile.names)) {
  ETrF.r <- raster(ETrF.maps[f])
  ETr.r <- raster(AdjETr.maps[f])
  ETadj.r <-  ETrF.r * ETr.r
  rw <- writeRaster(ETadj.r, outfile.names[f], overwrite=TRUE)
  
  print(paste(f, "of" ,length(outfile.names)))
  flush.console()
  
}



