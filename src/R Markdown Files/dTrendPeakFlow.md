Detrend Peak Flow Data with Nonparameteric Linear Slope Estimator
========================================================

# Initialize environmet   

```r
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew/")
```

# Read in peak flow data from 

```r
# Identify streamgages where trends may be present
ndxTrnd <- which(PeakFlowStats$tauP_value < 0.02)
```

```
## Error: object 'PeakFlowStats' not found
```

```r
# Number of streamgages that may have a trend
numTrnd <- length(ndxTrnd)
```

```
## Error: object 'ndxTrnd' not found
```

```r
# PeakFlowStats$Station[ndxTrnd]
for (i in 1:numTrnd){
  # ID streamgage for analysis
  stationNo <- PeakFlowStats$Station[ndxTrnd[i]]
  # Read in peak flow data from 
  peakFile  <- paste("./data/Raw/pk",stationNo,".txt",sep="")
  #
  # Read in the Peak flow (txt) file 
  peakData <- readLines(peakFile)
  # Determine the length of the data file
  nData <- length(peakData)
  #
  # Extract the station number and name from H card
  if (substr(peakData[3],1,1)=="N"){
    staNo    <- substr(peakData[3], 2,16)
    staNo    <- substr(peakData[3], 2,regexpr(" ",staNo))
    staName  <- substr(peakData[3],17,64)
  }
  # Extract the character string for flow
  flowVal  <- as.numeric(substr(peakData[5:nData],25,31))
  # Extract the flow codes for the record
  flowCode <- substr(peakData[5:nData],32,43) 
  # Find indices of peaks with partial date information
  ndxPartDate  <- grep("B",flowCode)
  # Create date string from peak file data
  dateStr  <- substr(peakData[5:nData],17,24)
  # Convert the character string to date
  dateVal  <- as.Date(dateStr, "%Y%m%d")
  plot(dateVal,log10(flowVal),pch=16, col="blue",cex=0.9,
       xlab="Year",ylab="log10 Peak Flow, in cfs",
       main = paste("Peak flow time series at",staNo,staName))
  #
  # Plot the trend through the time series plot
  
  ndxSta <- which(PeakFlowStats$Station == staNo)
  cat(paste("Station",PeakFlowStats$Station[ndxSta],"has a median slope of",
            PeakFlowStats$tauMedSlope[ndxSta]),"and a p-value of",
      PeakFlowStats$tauP_value[ndxSta])
  cat("The end date is",as.character(median(dateVal)),
      "and the median flow is",median(flowVal),"\n")
  # points(median(dateVal),median(log10(flowVal)),pch=4,col="red")
  # Compute time vector from median time
  relMedDate <- (dateVal-median(dateVal))/365.25
  relMedFlow <- as.numeric(median(flowVal) + relMedDate * PeakFlowStats$tauMedSlope[ndxSta])
  lines(dateVal,log10(relMedFlow),lty="dashed",col="red")
  # Detrend flowVal 
  flowValdTrd <- as.numeric(flowVal - relMedDate * PeakFlowStats$tauMedSlope[ndxSta])
  points(dateVal,log10(flowValdTrd),pch=17,col="darkgreen",cex=0.75)
  legend("topleft",legend=c("Measured Flows","Trend Line","Detrended Flows"),
         pch=c(16,NA,17),lty=c(NA,"dashed",NA),col=c("blue","red","darkgreen"),
         cex = 0.75)
}
```

```
## Error: object 'numTrnd' not found
```


