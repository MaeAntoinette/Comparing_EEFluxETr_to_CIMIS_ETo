#Objective: To separate data by season. This will be used to analyze how data varies seasonally and
#answer the question: Do we need a time-varying correction factor by season? 
#We will create a scatter plot for each season

Ann.ETr.EE.CIM.OC <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals.csv")
Ann.ETr.EE.CIM.OC$Date <- as.Date.character(Ann.ETr.EE.CIM.OC$Date)


#-------- Seasonal regression (for scatter plots) ----------

#Separate by season 
Ann.ETr.Wint <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Season == "Winter",]
Ann.ETr.Spri <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Season == "Spring",]
Ann.ETr.Summ <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Season == "Summer",]
Ann.ETr.Fall <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Season == "Fall",]

write.csv(Ann.ETr.Wint, "F:/EEFlux/P39R37/ETr_Comparisons/Seasonal_EECIM_Winter.csv",row.names = FALSE)
write.csv(Ann.ETr.Spri, "F:/EEFlux/P39R37/ETr_Comparisons/Seasonal_EECIM_Spring.csv",row.names = FALSE)
write.csv(Ann.ETr.Summ, "F:/EEFlux/P39R37/ETr_Comparisons/Seasonal_EECIM_Summer.csv",row.names = FALSE)
write.csv(Ann.ETr.Fall, "F:/EEFlux/P39R37/ETr_Comparisons/Seasonal_EECIM_Fall.csv",row.names = FALSE)

#residuals by season
Ann.ETr.Reg.Wint <- lm(ETr ~ xETr, Ann.ETr.Wint)
Ann.ETr.Reg.Spri <- lm(ETr ~ xETr, Ann.ETr.Spri)
Ann.ETr.Reg.Summ <- lm(ETr ~ xETr, Ann.ETr.Summ)
Ann.ETr.Reg.Fall <- lm(ETr ~ xETr, Ann.ETr.Fall)


#---------- Seasonal regressions for each station  ---------------
#Separate by station
Ann.ETr.cm <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Station == "CM",]
Ann.ETr.mel <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Station == "Mel",]
Ann.ETr.seel <- Ann.ETr.EE.CIM.OC[Ann.ETr.EE.CIM.OC$Station == "Seel",]

Ann.ETr.cm.reg <- lm(ETr~xETr, Ann.ETr.cm)
Ann.ETr.mel.reg <- lm(ETr~xETr, Ann.ETr.mel)
Ann.ETr.seel.reg <- lm(ETr~xETr, Ann.ETr.seel)

Ann.ETr.cm$stationXresid <- Ann.ETr.cm.reg$residuals
Ann.ETr.mel$stationXresid <- Ann.ETr.mel.reg$residuals
Ann.ETr.seel$stationXresid <- Ann.ETr.seel.reg$residuals

write.csv(Ann.ETr.cm, "F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_CM.csv",row.names = FALSE)
write.csv(Ann.ETr.mel, "F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_Mel.csv",row.names = FALSE)
write.csv(Ann.ETr.seel, "F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_Seel.csv",row.names = FALSE)






#-------------- Plot one regression eq. for all stations by season (four plots) -------
plot.ScatterPlot.Seasons <- function() {
windows(width = 8, height = 10) 
par(oma = c(1,1,1,1))
par(mar = c(4,4,2,2))
layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))


  #winter
  col.vec.wint <- ifelse(Ann.ETr.Wint$Station == "CM","grey",
                       ifelse(Ann.ETr.Wint$Station == "Mel", "grey30", "black"))
  pch.vec.wint <- ifelse(Ann.ETr.Wint$Station == "CM", 17,
                       ifelse(Ann.ETr.Wint$Station == "Mel", 8, 20))
  plot(Ann.ETr.Wint$xETr,Ann.ETr.Wint$ETr,type = "p",
     col = col.vec.wint, pch = pch.vec.wint,
     xlab = " ", ylab = "CIMIS ETo (mm/day)", xlim = c(0,20),main = "Winter")
  abline(Ann.ETr.Reg.Wint$coefficients[1],Ann.ETr.Reg.Wint$coefficients[2],lty = 2, col = "grey")

  #spring
  col.vec.spri <- ifelse(Ann.ETr.Spri$Station == "CM","grey",
                       ifelse(Ann.ETr.Spri$Station == "Mel", "grey30", "black"))
  pch.vec.spri <- ifelse(Ann.ETr.Spri$Station == "CM", 17,
                       ifelse(Ann.ETr.Spri$Station == "Mel", 8, 20))
  plot(Ann.ETr.Spri$xETr,Ann.ETr.Spri$ETr,type = "p",
     col = col.vec.spri, pch = pch.vec.spri,
     xlab = " ", ylab = " ", xlim = c(0,20),main = "Spring")
  abline(Ann.ETr.Reg.Spri$coefficients[1],Ann.ETr.Reg.Spri$coefficients[2],lty = 2, col = "grey")

  #summer
  col.vec.summ <- ifelse(Ann.ETr.Summ$Station == "CM","grey",
                       ifelse(Ann.ETr.Summ$Station == "Mel", "grey30", "black"))
  pch.vec.summ <- ifelse(Ann.ETr.Summ$Station == "CM", 17,
                       ifelse(Ann.ETr.Summ$Station == "Mel", 8, 20))
  plot(Ann.ETr.Summ$xETr,Ann.ETr.Summ$ETr,type = "p",
     col = col.vec.summ, pch = pch.vec.summ,
     xlab = "EEFlux ETr (mm/day)", ylab = "CIMIS ETo (mm/day)", xlim = c(0,20),main = "Summer")
  abline(Ann.ETr.Reg.Summ$coefficients[1],Ann.ETr.Reg.Summ$coefficients[2],lty = 2, col = "grey")

  #fall
  col.vec.fall <- ifelse(Ann.ETr.Fall$Station == "CM","grey",
                       ifelse(Ann.ETr.Fall$Station == "Mel", "grey30", "black"))
  pch.vec.fall <- ifelse(Ann.ETr.Fall$Station == "CM", 17,
                       ifelse(Ann.ETr.Fall$Station == "Mel", 8, 20))
  plot(Ann.ETr.Fall$xETr,Ann.ETr.Fall$ETr,type = "p",
     col = col.vec.fall, pch = pch.vec.fall,
     xlab = "EEFlux ETr (mm/day)", ylab = " ", xlim = c(0,20),main = "Fall")
  abline(Ann.ETr.Reg.Fall$coefficients[1],Ann.ETr.Reg.Fall$coefficients[2],lty = 2, col = "grey")

}

legend.ScatterPlot.Seasons <- function(){
  legend("bottomright", c("CalMul","Mel","Seel"),pch = c(17,8,20), col = c("grey","grey30","black"))
} 







#----------- Plot residuals to fitted ------------
plot.ResidualsFitted.Seasons <- function(){
  windows(width = 8, height = 10) 
  par(oma=c(1,2,2,2))
  par(mar=c(4,4,2,3)) 
  layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))
  
  plot(Ann.ETr.Reg.Wint$fitted.values, Ann.ETr.Reg.Wint$residuals, type = "p",
     pch = pch.vec.wint,col = col.vec.wint, xlab = "Fitted", ylab = "Residual",main="Winter")
  abline(0,0,lty=2,col="grey")
  
  plot(Ann.ETr.Reg.Spri$fitted.values, Ann.ETr.Reg.Spri$residuals, type = "p",
     pch = pch.vec.spri,col = col.vec.spri, xlab = "Fitted", ylab = "Residual",main = "Spring")
  abline(0,0,lty=2,col="grey")
  
  plot(Ann.ETr.Reg.Summ$fitted.values, Ann.ETr.Reg.Summ$residuals, type = "p",
     pch = pch.vec.summ,col = col.vec.summ, xlab = "Fitted", ylab = "Residual",main = "Summer")
  abline(0,0,lty=2,col="grey")
  
  plot(Ann.ETr.Reg.Fall$fitted.values, Ann.ETr.Reg.Fall$residuals, type = "p",
     pch = pch.vec.fall,col = col.vec.fall, xlab = "Fitted", ylab = "Residual",main = "Fall")
  abline(0,0,lty=2,col="grey")
}


