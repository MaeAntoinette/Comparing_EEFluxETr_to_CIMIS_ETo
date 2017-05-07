#Objective: After calculating annual point data for EEFlux PET. You want to compare it to the annual CIMIS PET data.
#This requires an interpolation of your data. 
#This will save annual CIMIS-EEFlux ETr data in your ETr_Comparisons folder in the EEFlux P39R37 pn. 

load("F:/RStudio/Scripts/QuantifyingETrUsingEEFluxData_Part1_workenv.RData")


#-------------- Compare to weather data. ----------------
CIMIS.fold <- "F:/CIMIS/"
CIMIS.west.pn <- "IV_Westmorland/"
CIMIS.cali.pn <- "IV_Calipatria/"
CIMIS.mel.pn <- "IV_Meloland/"
CIMIS.seel.pn <- "IV_Seeley/"

CIMIS.west <- read.csv(paste0(CIMIS.fold,CIMIS.west.pn,"eto_daily.csv"),header=TRUE,sep=",")
CIMIS.west <- setNames(CIMIS.west[,c(4,8)],c("Date","Westmorland"))
CIMIS.west$Date <- as.Date(CIMIS.west$Date,format = "%m/%d/%Y")

CIMIS.cali <- read.csv(paste0(CIMIS.fold,CIMIS.cali.pn,"eto_daily.csv"),header=TRUE,sep=",")
CIMIS.cali <- setNames(CIMIS.cali[,c(4,8)],c("Date","CalipatriaMulberry"))
CIMIS.cali$Date <- as.Date(CIMIS.cali$Date,format = "%m/%d/%Y")

CIMIS.mel <- read.csv(paste0(CIMIS.fold,CIMIS.mel.pn,"eto_daily.csv"),header=TRUE,sep=",")
CIMIS.mel <- setNames(CIMIS.mel[,c(4,8)],c("Date","Meloland"))
CIMIS.mel$Date <- as.Date(CIMIS.mel$Date,format = "%m/%d/%Y")

CIMIS.seel <- read.csv(paste0(CIMIS.fold,CIMIS.seel.pn,"eto_daily.csv"),header=TRUE,sep=",")
CIMIS.seel <- setNames(CIMIS.seel[,c(4,8)],c("Date","Seeley"))
CIMIS.seel$Date <- as.Date(CIMIS.seel$Date,format = "%m/%d/%Y")

#Merge CIMIS data with EEFlux ETr data in timeline.
ETr.Timeline <- merge(ETr.Timeline,CIMIS.west,all = TRUE)
ETr.Timeline <- merge(ETr.Timeline,CIMIS.cali,all = TRUE)
ETr.Timeline <- merge(ETr.Timeline,CIMIS.mel,all = TRUE)
ETr.Timeline <- merge(ETr.Timeline,CIMIS.seel,all=TRUE)

#Interpolate EEFlux Data to derive annual sums 
ETr.dates <- ETr.Timeline$Date
Ann.ETr.EE.CM <- approx(ETr.Timeline$Date,ETr.Timeline$xCalipatriaMulberry,xout=ETr.dates)
Ann.ETr.EE.CM <- data.frame(Ann.ETr.EE.CM)
names(Ann.ETr.EE.CM) <- c("Date","xCM_ETr_mm")

Ann.ETr.EE.Mel <- approx(ETr.Timeline$Date,ETr.Timeline$xMeloland,xout=ETr.dates)
Ann.ETr.EE.Mel <- data.frame(Ann.ETr.EE.Mel)
names(Ann.ETr.EE.Mel) <- c("Date","xMel_ETr_mm")

Ann.ETr.EE.Seel <- approx(ETr.Timeline$Date,ETr.Timeline$xSeeley,xout=ETr.dates)
Ann.ETr.EE.Seel <- data.frame(Ann.ETr.EE.Seel)
names(Ann.ETr.EE.Seel) <- c("Date","xSeel_ETr_mm")





#--------- Create daily interpolated annual EEFlux PET df --------
Ann.ETr.EE <- merge(Ann.ETr.EE.CM,Ann.ETr.EE.Mel)
Ann.ETr.EE <- merge(Ann.ETr.EE,Ann.ETr.EE.Seel)
Ann.ETr.EE$Year <- as.numeric(format(strptime(Ann.ETr.EE$Date,format="%Y-%m-%d"),"%Y"))

#Beginning and end values will show up as NA. 
#If there are NA values, fill them in with the value of the closest date.
Ann.ETr.NonNA.index.2010.cm <- min(which(!is.na(Ann.ETr.EE$xCM_ETr_mm)))
Ann.ETr.NonNA.value.2010.cm <- Ann.ETr.EE$xCM_ETr_mm[Ann.ETr.NonNA.index.2010.cm]

Ann.ETr.NonNA.index.2015.cm <- max(which(!is.na(Ann.ETr.EE$xCM_ETr_mm)))
Ann.ETr.NonNA.value.2015.cm <- Ann.ETr.EE$xCM_ETr_mm[Ann.ETr.NonNA.index.2015.cm]

Ann.ETr.EE$xCM_ETr_mm[(Ann.ETr.EE$Year == 2010) & (is.na(Ann.ETr.EE$xCM_ETr_mm))] <- Ann.ETr.NonNA.value.2010.cm
Ann.ETr.EE$xCM_ETr_mm[(Ann.ETr.EE$Year == 2015) & (is.na(Ann.ETr.EE$xCM_ETr_mm))] <- Ann.ETr.NonNA.value.2015.cm

#Meloland
Ann.ETr.NonNA.index.2010.mel <- min(which(!is.na(Ann.ETr.EE$xMel_ETr_mm)))
Ann.ETr.NonNA.value.2010.mel <- Ann.ETr.EE$xMel_ETr_mm[Ann.ETr.NonNA.index.2010.mel]

Ann.ETr.NonNA.index.2015.mel <- max(which(!is.na(Ann.ETr.EE$xMel_ETr_mm)))
Ann.ETr.NonNA.value.2015.mel <- Ann.ETr.EE$xMel_ETr_mm[Ann.ETr.NonNA.index.2015.mel]

Ann.ETr.EE$xMel_ETr_mm[(Ann.ETr.EE$Year == 2010) & (is.na(Ann.ETr.EE$xMel_ETr_mm))] <- Ann.ETr.NonNA.value.2010.mel
Ann.ETr.EE$xMel_ETr_mm[(Ann.ETr.EE$Year == 2015) & (is.na(Ann.ETr.EE$xMel_ETr_mm))] <- Ann.ETr.NonNA.value.2015.mel

#Seeley
Ann.ETr.NonNA.index.2010.seel <- min(which(!is.na(Ann.ETr.EE$xSeel_ETr_mm)))
Ann.ETr.NonNA.value.2010.seel <- Ann.ETr.EE$xSeel_ETr_mm[Ann.ETr.NonNA.index.2010.seel]

Ann.ETr.NonNA.index.2015.seel <- max(which(!is.na(Ann.ETr.EE$xSeel_ETr_mm)))
Ann.ETr.NonNA.value.2015.seel <- Ann.ETr.EE$xSeel_ETr_mm[Ann.ETr.NonNA.index.2015.seel]

Ann.ETr.EE$xSeel_ETr_mm[(Ann.ETr.EE$Year == 2010) & (is.na(Ann.ETr.EE$xSeel_ETr_mm))] <- Ann.ETr.NonNA.value.2010.seel
Ann.ETr.EE$xSeel_ETr_mm[(Ann.ETr.EE$Year == 2015) & (is.na(Ann.ETr.EE$xSeel_ETr_mm))] <- Ann.ETr.NonNA.value.2015.seel



#-------- Aggregate CIMIS ETo daily to annual. ---------
yearPK = as.numeric(format(strptime(ETr.Timeline$Date,format="%Y-%m-%d"),"%Y"))
CaliMul.ann.Q <- setNames(aggregate(as.numeric(ETr.Timeline$CalipatriaMulberry),by=list(yearPK),FUN="sum",na.rm = TRUE),
                          c("Year","CM_ETo_Total"))
Meloland.ann.Q <- setNames(aggregate(as.numeric(ETr.Timeline$Meloland),by=list(yearPK),FUN="sum",na.rm = TRUE),
                           c("Year","Mel_ETo_Total"))
Seeley.ann.Q <- setNames(aggregate(as.numeric(ETr.Timeline$Seeley),by=list(yearPK),FUN="sum",na.rm = TRUE),
                         c("Year","Seel_ETo_Total"))


#------ Aggregate EEFlux ETr daily to annual. ---------
CaliMul.EE.ann.Q <- setNames(aggregate(as.numeric(Ann.ETr.EE$xCM_ETr_mm),by=list(yearPK),FUN="sum",na.rm = TRUE),
                             c("Year","xCM_ETr_Total"))
Meloland.EE.ann.Q <- setNames(aggregate(as.numeric(Ann.ETr.EE$xMel_ETr_mm),by=list(yearPK),FUN="sum",na.rm = TRUE),
                              c("Year","xMel_ETr_Total"))
Seeley.EE.ann.Q <- setNames(aggregate(as.numeric(Ann.ETr.EE$xSeel_ETr_mm),by=list(yearPK),FUN="sum",na.rm = TRUE),
                            c("Year","xSeel_ETr_Total"))



#----------- Create annual dataframe of CIMIS and EEFlux reference ET values ----------
Ann.ETr.df <- merge(CaliMul.ann.Q,Meloland.ann.Q)
Ann.ETr.df <- merge(Ann.ETr.df,Seeley.ann.Q)
Ann.ETr.df <- merge(Ann.ETr.df,CaliMul.EE.ann.Q)
Ann.ETr.df <- merge(Ann.ETr.df,Meloland.EE.ann.Q)
Ann.ETr.df <- merge(Ann.ETr.df,Seeley.EE.ann.Q)

#Create min and max values. 
ETr.min <- min(ETr.Timeline$xCalipatriaMulberry,ETr.Timeline$xMeloland,ETr.Timeline$xSeeley,
               ETr.Timeline$CalipatriaMulberry,ETr.Timeline$Meloland,ETr.Timeline$Seeley,na.rm = TRUE)
ETr.max <- max(ETr.Timeline$xCalipatriaMulberry,ETr.Timeline$xMeloland,ETr.Timeline$xSeeley,na.rm = TRUE)

write.csv(ETr.Timeline,"F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Values_in_Timeline.csv",row.names = FALSE)
write.csv(Ann.ETr.df, "F:/EEFlux/P39R37/ETr_Comparisons/Annual_PET_Comparison.csv",row.names = FALSE)






#--------- Daily EEFlux ETr plot overlay to CIMIS timeseries --------

plot.ImageAcqEEFluxETr.CIMIStimeseries <- function(){
  windows(width = 8, height = 10) 
  
  par(oma=c(1,2,2,2))
  par(mar=c(4,4,2,3)) 
  
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE))
  
  
# ----- Seeley 
plot(ETr.Timeline$Date,ETr.Timeline$Seeley,type = "l", col = "black",
     ylim = c(ETr.min,ETr.max),xlab = "Date", ylab = "Reference ET (mm/day)",main = "Seeley")
points(ETr.Timeline$Date,ETr.Timeline$xSeeley,pch = 20, col = "grey")
# ---- Calipatria 
plot(ETr.Timeline$Date,ETr.Timeline$CalipatriaMulberry,type = "l", col = "black",
     ylim = c(ETr.min,ETr.max),xlab = "Date", ylab = "Reference ET (mm/day)",main = "CalipatriaMulberry")
points(ETr.Timeline$Date,ETr.Timeline$xCalipatriaMulberry,pch = 20, col = "grey")
# ----- Meloland
plot(ETr.Timeline$Date,ETr.Timeline$Meloland,type = "l", col = "black",
     ylim = c(ETr.min,ETr.max),xlab = "Date", ylab = "Reference ET (mm/day)", main = "Meloland")
points(ETr.Timeline$Date,ETr.Timeline$xMeloland,pch = 20, col = "grey")

}


legend.ImageAcqEEFluxETr.CIMIStimeseries <- function(){
legend("topright",c("EEFlux","CIMIS"),pch = c(20,NA),lty = c(NA,1),col = c("grey","black"))
}







# ------------- Plot Compared Data: Annual EEFlux ETr to Annual ETo --------------
#Plot Annual CIMIS ETo to Annual EEFlux ETr, by year 
col.vec <- ifelse(Ann.ETr.df$Year == 2010, "black",
                  ifelse(Ann.ETr.df$Year == 2011, "grey",
                         ifelse(Ann.ETr.df$Year == 2012, "grey40",
                                ifelse(Ann.ETr.df$Year== 2013, "red",
                                       ifelse(Ann.ETr.df$Year == 2014, "blue","gold")))))
windows(width = 8, height = 10)
plot(Ann.ETr.df$CM_ETo_Total,Ann.ETr.df$xCM_ETr_Total,type = "p", pch = 17, col = col.vec,
     xlab = "Annual CIMIS ETo (mm)", ylab = "Annual EEFlux ETr (mm)",
     xlim = c(1800,2100),ylim = c(1800,3700))
abline(lm(xCM_ETr_Total ~ CM_ETo_Total,Ann.ETr.df), lty = 2, col = "grey")

points(Ann.ETr.df$Mel_ETo_Total,Ann.ETr.df$xMel_ETr_Total,pch = 8, col = col.vec)
abline(lm(xMel_ETr_Total ~ Mel_ETo_Total,Ann.ETr.df), lty = 4, col = "grey30")

points(Ann.ETr.df$Seel_ETo_Total,Ann.ETr.df$xSeel_ETr_Total,pch = 15, col = col.vec)
abline(lm(xSeel_ETr_Total ~ Seel_ETo_Total,Ann.ETr.df), lty = 3, col = "black")

legend("bottomright", c("2010","2011","2012","2013","2014","2015","CM","CM lm","Mel","Mel lm","Seel","Seel lm"),
       col = c("black","grey","grey40","red","blue","gold","black","grey","black","grey30","black","black"), 
       pch = c(20, 20, 20, 20, 20, 20, 17, NA,8,NA, 15,NA),
       lty = c(NA, NA, NA, NA, NA, NA, NA, 2, NA, 4, NA, 3),
       ncol = 3, cex = 0.75)


#----- flipped the axes. 


plot(Ann.ETr.df$xCM_ETr_Total,Ann.ETr.df$CM_ETo_Total,type = "p", pch = 17, col = col.vec,
     ylab = "Annual CIMIS ETo (mm)", xlab = "Annual EEFlux ETr (mm)",
     ylim = c(1800,2100),xlim = c(1800,3700))

abline(lm(CM_ETo_Total ~ xCM_ETr_Total,Ann.ETr.df), lty = 2, col = "grey")
Ann.ETr.res.CM <- lm(CM_ETo_Total ~ xCM_ETr_Total,Ann.ETr.df)
Ann.ETr.res.CM$residuals

points(Ann.ETr.df$xMel_ETr_Total,Ann.ETr.df$Mel_ETo_Total,pch = 8, col = col.vec)
abline(lm(Mel_ETo_Total ~ xMel_ETr_Total,Ann.ETr.df), lty = 4, col = "grey30")
Ann.ETr.res.Mel <- lm(Mel_ETo_Total ~ xMel_ETr_Total,Ann.ETr.df)
Ann.ETr.res.Mel$residuals

points(Ann.ETr.df$xSeel_ETr_Total,Ann.ETr.df$Seel_ETo_Total,pch = 15, col = col.vec)
abline(lm(Seel_ETo_Total ~ xSeel_ETr_Total,Ann.ETr.df), lty = 3, col = "black")
Ann.ETr.res.Seel <- lm(Seel_ETo_Total ~ xSeel_ETr_Total,Ann.ETr.df)
Ann.ETr.res.Seel$residuals

legend("bottomleft", c("2010","2011","2012","2013","2014","2015","CM","CM lm","Mel","Mel lm","Seel","Seel lm"),
       col = c("black","grey","grey40","red","blue","gold","black","grey","black","grey30","black","black"), 
       pch = c(20, 20, 20, 20, 20, 20, 17, NA,8,NA, 15,NA),
       lty = c(NA, NA, NA, NA, NA, NA, NA, 2, NA, 4, NA, 3),
       ncol = 3, cex = 0.75)



#---------- Create a matrix of your Ann ref ET data ----------------
Ann.ETr.mat <- as.matrix(round(t(Ann.ETr.df[,2:7])),digits = 2)
require(htmlTable)
htmlTable(Ann.ETr.mat,
          align="c c c c c c",
          header = paste(seq(2010,2015,by=1)),
          css.cell = "padding-top: .2em; padding-left: .5em; padding-bottom: .2em; padding-right: .2em;",
          rnames = c("CalMul","Mel","Seel","xCalMul","xMel","xSeel"), #lists all the rows 
          rgroup = c("CIMIS","EEFlux"),  #defines row sections
          n.rgroup = c(3,3) #defines how many rows are within each define row section
) 



