#Objective: To create a dataframe that compares PET per image acquisition date. for future regression plots.
#The finished csv will have image date, EEFlux value, CIMIS value, sesaon, and residual from the regression line. 

#Bring in your Timeline csv that shows CIMIS values and EEFlux ETr values on their acquisition dates. 
ETr.Timeline <- read.csv("F:\\EEFlux\\P39R37\\ETr_Comparisons\\EECIM_Values_in_Timeline.csv",header=TRUE)


#Note that you have Westmorland in your df. We don't have complete CIMIS data for that so we will remove. 
#Also, we will truncate the data to the days that we have EEFlux images. This is for our regression. 
ETr.Timeline <- ETr.Timeline[ , -which(names(ETr.Timeline) %in% c("Westmorland","xWestmorland"))]
ETr.Timeline <- na.omit(ETr.Timeline) 


write.csv(ETr.Timeline, "F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Values_By_AcquisitionDate.csv",row.names = FALSE)
#make sure your date column is read in as a date. 
ETr.Timeline$Date <- as.Date.character(ETr.Timeline$Date)




#Separate the timeline: eeflux data per station.
Ann.ETr.EE.CM <- setNames(ETr.Timeline[,which(names(ETr.Timeline) %in% c("Date","xCalipatriaMulberry"))],c("Date","xETr"))
Ann.ETr.EE.CM$Station <- "CM"

Ann.ETr.EE.Mel<- setNames(ETr.Timeline[,which(names(ETr.Timeline) %in% c("Date","xMeloland"))],c("Date","xETr"))
Ann.ETr.EE.Mel$Station <- "Mel"

Ann.ETr.EE.Seel<- setNames(ETr.Timeline[,which(names(ETr.Timeline) %in% c("Date","xSeeley"))],c("Date","xETr"))
Ann.ETr.EE.Seel$Station <- "Seel"

#Similarly, separate the timeline: cimis data per station.
Ann.ETr.CIM.CM<- setNames(ETr.Timeline[,which(names(ETr.Timeline) %in% c("Date","CalipatriaMulberry"))],c("Date","ETr"))
Ann.ETr.CIM.CM$Station <- "CM"

Ann.ETr.CIM.Mel<- setNames(ETr.Timeline[,which(names(ETr.Timeline) %in% c("Date","Meloland"))],c("Date","ETr"))
Ann.ETr.CIM.Mel$Station <- "Mel"

Ann.ETr.CIM.Seel<- setNames(ETr.Timeline[,which(names(ETr.Timeline) %in% c("Date","Seeley"))],c("Date","ETr"))
Ann.ETr.CIM.Seel$Station <- "Seel"


#-------- Create df of CIMIS and EEFlux PET data at all sites. One col (OC) for CIM, OC for EEFlux (EE) ---------
Ann.ETr.EE.OC <- rbind(Ann.ETr.EE.CM,Ann.ETr.EE.Mel,Ann.ETr.EE.Seel) #315 days each. 
Ann.ETr.CIM.OC <- rbind(Ann.ETr.CIM.CM,Ann.ETr.CIM.Mel,Ann.ETr.CIM.Seel)

#Combine the rbinds together into one df
Ann.ETr.EE.CIM.OC <- cbind(Ann.ETr.EE.OC,Ann.ETr.CIM.OC)
row.names(Ann.ETr.EE.CIM.OC) <- seq(1:nrow(Ann.ETr.EE.CIM.OC))
#remove additional dates and station cols. 
Ann.ETr.EE.CIM.OC <- Ann.ETr.EE.CIM.OC[-c(3,4)] 

rm(Ann.ETr.EE.OC,Ann.ETr.CIM.OC) #we don't need these anymore... 




#------- Create additional columns in Ann.ETr.EE.CIM.OC to describe Y/Mo/Season/residuals for one reg. eq --------
#Identify groups by month and by season
Ann.ETr.month <- as.numeric(format(strptime(Ann.ETr.EE.CIM.OC$Date,format="%Y-%m-%d"),"%m"))
Ann.ETr.year <- as.numeric(format(strptime(Ann.ETr.EE.CIM.OC$Date,format = "%Y-%m-%d"),"%Y"))
Ann.ETr.seas <- ifelse(Ann.ETr.month == "12", "Winter", 
                       ifelse(Ann.ETr.month == "1","Winter",
                              ifelse(Ann.ETr.month == "2", "Winter",
                                     ifelse(Ann.ETr.month == "3", "Spring",
                                            ifelse(Ann.ETr.month == "4", "Spring",
                                                   ifelse(Ann.ETr.month == "5", "Spring",
                                                          ifelse(Ann.ETr.month == "6", "Summer",
                                                                 ifelse(Ann.ETr.month == "7", "Summer",
                                                                        ifelse(Ann.ETr.month == "8", "Summer",
                                                                               ifelse(Ann.ETr.month == "9", "Fall",
                                                                                      ifelse(Ann.ETr.month == "10", "Fall","Fall")))))))))))
Ann.ETr.EE.CIM.OC$Year <- Ann.ETr.year
Ann.ETr.EE.CIM.OC$Month <- Ann.ETr.month
Ann.ETr.EE.CIM.OC$Season <- Ann.ETr.seas
Ann.ETr.EE.CIM.OC$Y.Seas <- paste(Ann.ETr.EE.CIM.OC$Year,Ann.ETr.EE.CIM.OC$Season,sep="-")

#get regression equation and residuals to the fitted value
Ann.ETr.Reg <- lm(ETr ~ xETr, Ann.ETr.EE.CIM.OC)
Ann.ETr.EE.CIM.all.reg <- lm(ETr~xETr, Ann.ETr.EE.CIM.OC)
Ann.ETr.EE.CIM.OC$Residuals <- Ann.ETr.EE.CIM.all.reg$residuals



#------- Save your df ------- 
write.csv(Ann.ETr.EE.CIM.OC, "F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals.csv",
          row.names = FALSE)



#Regressed Data
#Create min and max values. 
ETr.min <- min(ETr.Timeline$xCalipatriaMulberry,ETr.Timeline$xMeloland,ETr.Timeline$xSeeley,
               ETr.Timeline$CalipatriaMulberry,ETr.Timeline$Meloland,ETr.Timeline$Seeley,na.rm = TRUE)
ETr.max <- max(ETr.Timeline$xCalipatriaMulberry,ETr.Timeline$xMeloland,ETr.Timeline$xSeeley,na.rm = TRUE)



#--------- Plot all EEFlux ETr and CIMIS regression with one equation ------------------

#Create scatter plot with mutli regressed data
plot.ScatterPlot.SingleRegress <- function() {
windows(width = 8, height = 10) 
par(oma=c(1,2,2,2))
par(mar=c(4,4,2,3)) 

ETr.col.vec <- ifelse(Ann.ETr.EE.CIM.OC$Station == "CM", "grey",
                      ifelse(Ann.ETr.EE.CIM.OC$Station == "Mel", "grey30","black"))
ETr.pch.vec <- ifelse(Ann.ETr.EE.CIM.OC$Station == "CM", 17,
                      ifelse(Ann.ETr.EE.CIM.OC$Station == "Mel", 8, 20))

windows(width = 8, height = 10)
par(oma=c(1,2,2,2))

plot(Ann.ETr.EE.CIM.OC$xETr,Ann.ETr.EE.CIM.OC$ETr,pch = ETr.pch.vec, col = ETr.col.vec,
     xlab = expression("EEFlux ETr (mm day"^-1~")"), ylab = expression("CIMIS ETo (mm day"^-1~")"), ylim = c(0,20), xlim = c(0,20),
     xaxs = "i", yaxs = "i")
abline(Ann.ETr.Reg$coefficients[1],Ann.ETr.Reg$coefficients[2],lty = 2)

}

legend.ScatterPlot.SingleRegress <- function() {
text.default(x = 5, y = 7, "y = 0.23 + 0.60x", cex = 0.75)
legend("bottomright", c("Seeley","CalipatriaMulberry", "Meloland"),pch = c(20, 17, 8),
       col = c("black","grey","grey30"),cex = 0.65)
}


# ------------- Plot Compared Data: Scatter plot of EEFlux ETr to CIMIS ETo by acquisition date --------------
#Create scatter plot with mutli regressed data
plot.ScatterPlot.MultiRegress <- function() {
windows(width = 8, height = 10) 
par(oma=c(1,2,2,2))
par(mar=c(4,4,2,3)) 

plot(ETr.Timeline$Seeley,ETr.Timeline$xSeeley,pch = 20,col = "black",
     xlim = c(ETr.min,ETr.max), xlab = "CIMIS ETo (mm/day)", ylab = "EEFlux ETr (mm/day)")
abline(lm(xSeeley ~ Seeley,ETr.Timeline), lty = 2, col = "black")
points(ETr.Timeline$CalipatriaMulberry,ETr.Timeline$xCalipatriaMulberry,pch = 17,col = "grey")
abline(lm(xCalipatriaMulberry ~ CalipatriaMulberry,ETr.Timeline), col = "grey", lty = 2)
points(ETr.Timeline$Meloland,ETr.Timeline$xMeloland,pch = 8, col = "grey30")
abline(lm(xMeloland ~ Meloland,ETr.Timeline),col = "grey30",lty = 2)
abline(0,1)
}

legend.ScatterPlot.MultiRegress <- function(){
legend("bottomright",c("Seeley","CalipatriaMulberry",
                       "Meloland","Absolute Line"), pch = c(20,17,8,NA), lty = c(NA,NA,NA,1),
       col = c("black","grey","grey30","black"))
}

