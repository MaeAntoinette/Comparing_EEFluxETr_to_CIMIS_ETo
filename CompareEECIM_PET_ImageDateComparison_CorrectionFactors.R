#Background: Time series residuals of EEFlux ETr to CIMIS ETo show that there is spatial uniformity amongst the stations 
#and thus across the valley. Seasonal series does not show uniformity. Perhaps each image will need to be corrected. 

#Objective: Get ratio of CIMIS PET to EEFlux ET for each image at each station. Plots of each "correction factor" 
#to see how close to 1:1 they are. If they are close, we will just apply an individual CF to each image. 
#Output: Three scatter plots (CF1 - CF2, CF2 - CF3, CF3 - CF1). Get lm eq for each CF-CF plot. 

ETr.timeline <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Values_By_AcquisitionDate.csv")

ETr.timeline$CF.CM <- ETr.timeline$CalipatriaMulberry/ETr.timeline$xCalipatriaMulberry
ETr.timeline$CF.Mel <- ETr.timeline$Meloland/ETr.timeline$xMeloland
ETr.timeline$CF.Seel <- ETr.timeline$Seeley/ETr.timeline$xSeeley

ETr.timeline$Date <- as.Date.character(ETr.timeline$Date)

#------- Create additional columns in Ann.ETr.EE.CIM.OC to describe Y/Mo/Season/residuals for one reg. eq --------
#Identify groups by month and by season
Ann.ETr.month <- as.numeric(format(strptime(ETr.timeline$Date,format="%Y-%m-%d"),"%m"))
Ann.ETr.year <- as.numeric(format(strptime(ETr.timeline$Date,format = "%Y-%m-%d"),"%Y"))
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


ETr.timeline$Season <- Ann.ETr.seas


#-------- Get regression eq. for each CF comparison plot ----------
CFMEL.CFCM <- lm(CF.Mel ~ CF.CM, ETr.timeline)
CFSEEL.CFMEL <- lm(CF.Seel ~ CF.Mel, ETr.timeline)
CFCM.CFSEEL <- lm(CF.CM~CF.Seel, ETr.timeline)




#-------- Plot CF plots, regularly --------------- 

windows(width = 8, height = 10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))
par(oma = c(1,0.5,1,1))
par(mar = c(4,4,2,2))

plot(ETr.timeline$CF.CM,ETr.timeline$CF.Mel,pch = 20,
     xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "CF CalMul", ylab = "CF Mel",
     xaxs = "i", yaxs = "i")
abline(0,1,lty = 2, col = "grey")
text(0.3, 1.0, "y = 0.61x + 0.22",cex = 0.8)


plot(ETr.timeline$CF.Mel,ETr.timeline$CF.Seel,pch = 20,
     xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "CF Mel", ylab = "CF Seel",
     xaxs = "i", yaxs = "i")
abline(0,1,lty = 2, col = "grey")
text(0.3, 1.0, "y = 0.89x + 0.12",cex = 0.8)

plot(ETr.timeline$CF.Seel,ETr.timeline$CF.CM, pch = 20,
     xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "CF Seel", ylab = "CF CalMul",
     xaxs = "i", yaxs = "i")
abline(0,1,lty = 2, col = "grey")
text(0.3, 1.0, "y = 0.64x + 0.21",cex = 0.8)

#-------- Plot CF plots, by season -------- 
seas.col.vec <- ifelse(ETr.timeline$Season == "Winter", "blue",
                       ifelse(ETr.timeline$Season == "Spring", "hotpink",
                              ifelse(ETr.timeline$Season == "Summer", "forestgreen","goldenrod")))


windows(width = 8, height = 10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))
par(oma = c(1,0.5,1,1))
par(mar = c(4,4,2,2))

plot(ETr.timeline$CF.CM,ETr.timeline$CF.Mel,pch = 20,
     xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "CF CalMul", ylab = "CF Mel",
     xaxs = "i", yaxs = "i", col = seas.col.vec)
abline(0,1,lty = 2, col = "grey")
legend("topleft", c("W","SP","S","F"),
       col = c("blue","hotpink","forestgreen","goldenrod"),pch = 20)

plot(ETr.timeline$CF.Mel,ETr.timeline$CF.Seel,pch = 20,
     xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "CF Mel", ylab = "CF Seel",
     xaxs = "i", yaxs = "i", col = seas.col.vec)
abline(0,1,lty = 2, col = "grey")

plot(ETr.timeline$CF.Seel,ETr.timeline$CF.CM, pch = 20,
     xlim = c(0,1.5), ylim = c(0,1.5),
     xlab = "CF Seel", ylab = "CF CalMul",
     xaxs = "i", yaxs = "i", col = seas.col.vec)
abline(0,1,lty = 2, col = "grey")


