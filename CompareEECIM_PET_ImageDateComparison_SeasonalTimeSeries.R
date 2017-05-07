#Objective: Separate your station data by season to create a timeseries. 

Ann.ETr.cm <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_CM.csv")
Ann.ETr.mel <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_Mel.csv")
Ann.ETr.seel <- read.csv("F:/EEFlux/P39R37/ETr_Comparisons/EECIM_Comparison_LMRegression_Residuals_Seel.csv")

Ann.ETr.cm$Date <- as.Date.character(Ann.ETr.cm$Date)
Ann.ETr.mel$Date <- as.Date.character(Ann.ETr.cm$Date)
Ann.ETr.seel$Date <- as.Date.character(Ann.ETr.cm$Date)


#Seasonal separation per station
Ann.ETr.Wint.cm <- Ann.ETr.cm[Ann.ETr.cm$Season == "Winter",]
Ann.ETr.Wint.mel <- Ann.ETr.mel[Ann.ETr.mel$Season == "Winter",]
Ann.ETr.Wint.seel <- Ann.ETr.seel[Ann.ETr.seel$Season == "Winter",]

Ann.ETr.Spri.cm <- Ann.ETr.cm[Ann.ETr.cm$Season == "Spring",]
Ann.ETr.Spri.mel <- Ann.ETr.mel[Ann.ETr.mel$Season == "Spring",]
Ann.ETr.Spri.seel <- Ann.ETr.seel[Ann.ETr.seel$Season == "Spring",]

Ann.ETr.Summ.cm <- Ann.ETr.cm[Ann.ETr.cm$Season == "Summer",]
Ann.ETr.Summ.mel <- Ann.ETr.mel[Ann.ETr.mel$Season == "Summer",]
Ann.ETr.Summ.seel <- Ann.ETr.seel[Ann.ETr.seel$Season == "Summer",]

Ann.ETr.Fall.cm <- Ann.ETr.cm[Ann.ETr.cm$Season == "Fall",]
Ann.ETr.Fall.mel <- Ann.ETr.mel[Ann.ETr.mel$Season == "Fall",]
Ann.ETr.Fall.seel <- Ann.ETr.seel[Ann.ETr.seel$Season == "Fall",]


#-------------- Plot time series residuals for each station by season (3 x 4 = 12 plots)--------

plot.timeseries.winter <- function(){
  windows(width = 8, height = 10) 
  par(oma=c(1,2,2,2))
  layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))
  par(mar=c(4,4,2,3)) 
  #Winter time series
  plot(Ann.ETr.Wint.cm$Date,Ann.ETr.Wint.cm$stationXresid,type = "l",xlab = "",ylab = "CM ET Residual")
  plot(Ann.ETr.Wint.mel$Date,Ann.ETr.Wint.mel$stationXresid,type = "l",xlab = "",ylab = "Mel ET Residual")
  plot(Ann.ETr.Wint.seel$Date,Ann.ETr.Wint.seel$stationXresid,type = "l",xlab = "",ylab = "Seel ET Residual")
}


plot.timeseries.spring <- function(){
  windows(width = 8, height = 10) 
  par(oma=c(1,2,2,2))
  layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))
  par(mar=c(4,4,2,3)) 
  #Spring time series
  plot(Ann.ETr.Spri.cm$Date,Ann.ETr.Spri.cm$stationXresid,type = "l",xlab = "",ylab = "CM ET Residual")
  plot(Ann.ETr.Spri.mel$Date,Ann.ETr.Spri.mel$stationXresid,type = "l",xlab = "",ylab = "Mel ET Residual")
  plot(Ann.ETr.Spri.seel$Date,Ann.ETr.Spri.seel$stationXresid,type = "l",xlab = "",ylab = "Seel ET Residual")
}


plot.timeseries.summer <- function(){
  windows(width = 8, height = 10) 
  par(oma=c(1,2,2,2))
  layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))
  par(mar=c(4,4,2,3)) 
  #Summer time series
  plot(Ann.ETr.Summ.cm$Date,Ann.ETr.Summ.cm$stationXresid,type = "l",xlab = "",ylab = "CM ET Residual")
  plot(Ann.ETr.Summ.mel$Date,Ann.ETr.Summ.mel$stationXresid,type = "l",xlab = "",ylab = "Mel ET Residual")
  plot(Ann.ETr.Summ.seel$Date,Ann.ETr.Summ.seel$stationXresid,type = "l",xlab = "",ylab = "Seel ET Residual")
}

plot.timeseries.fall <- function(){
  windows(width = 8, height = 10) 
  par(oma=c(1,2,2,2))
  layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE))
  par(mar=c(4,4,2,3)) 
  #Fall time series 
  plot(Ann.ETr.Fall.cm$Date,Ann.ETr.Fall.cm$stationXresid,type = "l",xlab = "",ylab = "CM ET Residual")
  plot(Ann.ETr.Fall.mel$Date,Ann.ETr.Fall.mel$stationXresid,type = "l",xlab = "",ylab = "Mel ET Residual")
  plot(Ann.ETr.Fall.seel$Date,Ann.ETr.Fall.seel$stationXresid,type = "l",xlab = "",ylab = "Seel ET Residual")
} 