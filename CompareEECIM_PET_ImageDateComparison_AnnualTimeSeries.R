#Objective: Separate your station data to create a timeseries of residuals 

Ann.ETr.cm <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_CM.csv")
Ann.ETr.mel <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_Mel.csv")
Ann.ETr.seel <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_Seel.csv")


#Set date cols as date. 
Ann.ETr.cm$Date <- as.Date.character(Ann.ETr.cm$Date)
Ann.ETr.mel$Date <- as.Date.character(Ann.ETr.mel$Date)
Ann.ETr.seel$Date <- as.Date.character(Ann.ETr.seel$Date)



#-------- Time series of EEFlux residuals all dates ---------
plot.timeseries.annual <- function(){
windows(width = 8, height = 10) 
par(oma=c(1,2,2,2))
par(mar=c(4,4,2,3)) 

layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))

par(mai=c(0,0.5,0.1,0))

plot(Ann.ETr.cm$Date,Ann.ETr.cm$stationXresid,type = "l",
     xaxt = "n", xlab = " ", ylab = "CM ET Residual (mm)",ylim = c(-4,4))

plot(Ann.ETr.cm$Date,Ann.ETr.mel$stationXresid,type = "l",
     xaxt = "n",xlab = " ", ylab = "Mel ET Residual (mm)",ylim = c(-4,4))

par(mai=c(0.2,0.5,0.1,0))
plot(Ann.ETr.cm$Date,Ann.ETr.seel$stationXresid,type = "l",
     xlab = " ", ylab = "Seel ET Residual (mm)",ylim = c(-4,4))
}
