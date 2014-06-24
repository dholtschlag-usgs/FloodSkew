# Run extractPeakStatsFromPrt.R prior to running this script to 
# Set working directory
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew/")
# 
# Run extractPeakStatsFromPrt.R
# source("./src/Final Scripts/extractPeakStatsFromPrt.R")
#
# Identify streamgages where trends may be present
ndxTrnd <- which(PeakFlowStats$tauP_value < 0.02)
# Number of streamgages that may have a trend
numTrnd <- length(ndxTrnd)
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
  
  # (4) Detrend flows without changing most recent flow
  
  scratch <- peakData
  for (i in 1:length(dateVal)){
    substr(scratch[4+i],26,31) <- format(round(flowValdTrd[i], digits=0),
                                         justify="right", width=6)
  }
  # writeLines to output file
  outFile  <- paste("./data/raw/pk",staNo,"d.txt",sep="")
  fileConn <- file(outFile, open="w+") 
  for (i in 1:length(scratch)){
    writeLines(scratch[i], con = fileConn, sep = "\n")
  }
  close(fileConn)
}
# Set up the corresponding psf files
for (i in 1:numTrnd){
  # Read in the standard psf file
  stationNo <- PeakFlowStats$Station[ndxTrnd[i]]
  # Read in peak flow data from 
  psfFile   <- paste("./data/Raw/PK",stationNo,".psf",sep="")
  #
  # Read in the Peak flow (txt) file 
  psfData <- readLines(psfFile)
  # Determine the length of the data file
  nData <- length(peakData)
  # Replace to add the d for detrended data
  psfDataD <- gsub(paste(stationNo,".",sep=""),paste(stationNo,"d.",sep=""),psfData)
  # Open a file for output
  outFile  <- paste("./data/raw/pk",stationNo,"d.psf",sep="")
  fileConn <- file(outFile, open="w+") 
  for (j in 1:length(psfDataD)){
    writeLines(psfDataD[j], con = fileConn, sep = "\n")
  }
  close(fileConn)
}
###
# 
ndxTrnd   <- which(PeakFlowStats$tauP_value < 0.02)
gagesTrnd <- PeakFlowStats$Station[ndxTrnd]
# Subset gages that are not in the set of regulated gages
gages    <- subset(gages, (gages[,1] %in% gagesTrnd))
numGages <- nrow(gages)
#
# Initialize date frame df2 for peak flow statistics
df2 <- data.frame("Station"       = character(numGages),  #  1
                  "State"         = character(numGages),  #  2
                  #  3 Beginning Year
                  "begYear"       = numeric(numGages),   
                  #  4 Ending Year
                  "endYear"       = numeric(numGages), 
                  #  5 Historical Period Length
                  "histPerLen"    = numeric(numGages),    
                  #  6 Number of peaks in record
                  "numPeakRec"    = numeric(numGages),
                  #  7 Peaks not used in analysis
                  "peaksNotUsed"  = numeric(numGages), 
                  #  8 Systematic peaks in analysis
                  "numSysPeaks"   = numeric(numGages), 
                  #  9 Historic peaks in analysis
                  "numHistPeaks"  = numeric(numGages),  
                  # 10 PILFS (LOS) Threshold USING MULTIPLE GRUBBS-BECK TEST
                  "mgbLOThres"    = numeric(numGages),    
                  # 11 PILFS (LOS) low outliers detected USING MULTIPLE GRUBBS-BECK TEST
                  "numLoOutlier"  = numeric(numGages), 
                  # 12 EMA W/O REG. INFO: Mean
                  "emaMean"       = numeric(numGages),    
                  # 13 EMA W/O REG. INFO: Standard deviation
                  "emaStd"        = numeric(numGages),    
                  # 14 EMA W/O REG. INFO: Skew
                  "emaSkew"       = numeric(numGages),    
                  # 15 EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)
                  "mseSkewWOReg"  = numeric(numGages), 
                  # 16 EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE)
                  "mseSkewWSysO"  = numeric(numGages),
                  # 17 Kendall's Tau Parameters: Tau
                  "tauParameter"  = numeric(numGages),
                  # 18 Kendall's Tau Parameters: P-Value
                  "tauP_value"    = numeric(numGages),
                  # 19 Kendall's Tau Parameters: Median Slope
                  "tauMedSlope"   = numeric(numGages),
                  # 20 Kendall's Tau Parameters: No of Peaks
                  "tauNoPeaks"    = numeric(numGages),
                  stringsAsFactors = FALSE)
#
for (i in 1:numGages){
  # for (i in 1:5)  {
  df2[i,"Station"] <- gages[i,1]
  df2[i,"State"]   <- "Michigan"
  cat(i,gages[i,1],gages[i,2],"\n")
  prtFile          <- file(paste("./data/raw/pk",gages[i,1],"d.prt",sep=""),"r")
  fileContents     <- readLines(con = prtFile)
  close(prtFile)
  # 
  # Read: Number of peaks in record
  strPeakRec  <- fileContents[grep("Number of peaks in record            =",fileContents)]
  df2[i,"numPeakRec"]  <- as.numeric(substr(strPeakRec,60,63))
  if (icat) cat("Number of peaks in record:",df2[i,"numPeakRec"],"\n")
  # Read: Peaks not used in analysis
  strPeaksNot  <- fileContents[grep("Peaks not used in analysis           =",fileContents)]
  df2[i,"peaksNotUsed"] <- as.numeric(substr(strPeaksNot,60,64))
  if (icat) cat("Peaks not used in analysis:",df2[i,"peaksNotUsed"],"\n")
  
  # Read: Systematic peaks in analysis
  strSysPeaks <- fileContents[grep("Systematic peaks in analysis         =",fileContents)]
  df2[i,"numSysPeaks"] <- as.numeric(substr(strSysPeaks,60,64))
  if (icat) cat("Systematic peaks in analysis:",df2[i,"numSysPeaks"],"\n")
  
  # Read: Historic peaks in analysis
  strHistPeaks <- fileContents[grep("Historic peaks in analysis           =",fileContents)]
  df2[i,"numHistPeaks"] <- as.numeric(substr(strHistPeaks,60,64))
  if (icat) cat("Historic peaks in analysis:",df2[i,"numHistPeaks"],"\n")
  
  
  # Read: Beginning Year
  strBegYear   <- fileContents[grep("Beginning Year                       =",fileContents)]
  df2[i,"begYear"]     <- as.numeric(substr(strBegYear,60,63))
  if (icat) cat("Beginning Year          :",df2[i,"begYear"],"\n")
  # Read: Ending Year
  strEndYear   <- fileContents[grep("Ending Year                          =",fileContents)]
  df2[i,"endYear"]     <- as.numeric(substr(strEndYear,60,63))
  if (icat) cat("Ending Year             :",df2[i,"endYear"],"\n")
  # Read: Historical Period Length
  strHistLen   <- fileContents[grep("Historical Period Length             =",fileContents)]
  df2[i,"histPerLen"]   <- as.numeric(substr(strHistLen,60,63))
  if (icat) cat("Historical Period Length:",df2[i,"HistRecLen"],"\n")
  # 
  # Read: EMA W/O REG. INFO        
  strEmaStats  <- fileContents[grep("EMA W/O REG. INFO        ",fileContents)]
  df2[i,"emaMean"]      <- as.numeric(substr(strEmaStats,27,34))
  if (icat) cat("EMA W/O REG. Mean:",df2[i,"emaMean"])
  df2[i,"emaStd"]       <- as.numeric(substr(strEmaStats,38,46))
  if (icat) cat(",  Std:",df2[i,"emaStd"])
  df2[i,"emaSkew"]      <- as.numeric(substr(strEmaStats,50,57))
  if (icat) cat(", Skew:",df2[i,"emaSkew"],"\n")
  #
  # Read: EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)    
  strMseSkewReg  <- fileContents[grep("EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)",
                                      fixed = TRUE, fileContents)]
  df2[i,"mseSkewWOReg"]     <- as.numeric(substr(strMseSkewReg,58,64))
  if (icat) cat("EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE): ",df2[i,"mseSkewReg"],"\n")
  
  # Read: EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE) 
  strMseSkewSys  <- fileContents[grep("EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE) ",
                                      fixed = TRUE, fileContents)]
  df2[i,"mseSkewWSys"]     <- as.numeric(substr(strMseSkewSys,58,64))
  if (icat) cat("EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE): ",df2[i,"mseSkewSys"],"\n")
  
  # Read: EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST 
  strLoOutlier   <- fileContents[grep("EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST ",
                                      fileContents)]    
  if (length(strLoOutlier)==0){
    df2[i,"numLoOutlier"]   <- 0
    df2[i,"mgbLOThres"]     <- NaN
  } else {
    df2[i,"numLoOutlier"]   <- as.numeric(substr(strLoOutlier,71,73))
    df2[i,"mgbLOThres"]     <- as.numeric(substr(strLoOutlier,76,83)) 
  }
  # List results to console
  if (icat) cat("Number of low outliers: ",df2[i,"numLoOutlier"],"\n")
  if (icat) cat("MGB low outlier threshold: ",df2[i,"mgbLOThres"],"\n")
  # 
  # Define string for tau values
  str <- "TAU    P-VALUE    SLOPE   PEAKS"
  tauDataLine               <- grep(str,fileContents) + 2
  df2[i,"tauParameter"]     <- as.numeric(substr(fileContents[tauDataLine],34,42))
  df2[i,"tauP_value"]       <- as.numeric(substr(fileContents[tauDataLine],46,54))
  df2[i,"tauMedSlope"]      <- as.numeric(substr(fileContents[tauDataLine],56,65))
  df2[i,"tauNoPeaks"]       <- as.numeric(substr(fileContents[tauDataLine],66,69))
}
# Copy dataframe to better name
PeakFlowStatsTrnd           <- df2
# Remove old dataframe
rm(df2)
#
# Compare skews in trend and detrended sets
# Find indice of streamgages with trend in annual peaks
PeakFlowStatsOrig <- subset(PeakFlowStats, PeakFlowStats$Station %in% gagesTrnd)
%
par(mfrow=c(3,1))
plot(PeakFlowStatsOrig$emaMean, PeakFlowStatsTrnd$emaMean, pch=16, col="blue",
     xlab = "EMA Mean With Trend in Annual Peak Flows", 
     ylab = "EMA Mean With DeTrended Peak Flow Data",
     main = paste("Relation between Means of Logarithms from EMA Estimates\n",
     "W/O REG. INFO with Trend and Detrended Peak Flows"), cex=0.9)
abline(0,1,col="red",lty="dashed")

plot(PeakFlowStatsOrig$emaStd, PeakFlowStatsTrnd$emaStd, pch=16, col="blue",
     xlab = "EMA Std With Trend in Annual Peak Flows", 
     ylab = "EMA Std With DeTrended Peak Flow Data",
     main = paste("Relation between Std of Logarithms from EMA Estimates\n",
                  "W/O REG. INFO with Trend and Detrended Peak Flows"), cex=0.9)
abline(0,1,col="red",lty="dashed")

plot(PeakFlowStatsOrig$emaSkew, PeakFlowStatsTrnd$emaSkew, pch=16, col="blue",
     xlab = "EMA Skew With Trend in Annual Peak Flows", 
     ylab = "EMA Skew With DeTrended Peak Flow Data",
     main = paste("Relation between Skew of Logarithms from EMA Estimates\n",
                  "W/O REG. INFO with Trend and Detrended Peak Flows"), cex=0.9)
abline(0,1,col="red",lty="dashed")




