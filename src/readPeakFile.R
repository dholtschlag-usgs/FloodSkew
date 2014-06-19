# Generate a PSF file for peak flow analysis
# Name the file of interest
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
# Read set of MI gages derived from the Gages II data file
gages = read.table("./data/miGagesIIa.txt", sep="\t",header=TRUE,
                   colClasses=c("character","character"))
# 
# Streamgages with regulated flow
gagesReg = c("04034500","04035500","04036000","04044400","04058100","04058200",
             "04059000","04061500","04062000","04062500","04063000","04066800",
             "04067000","04108801","04114500","04128000","04129000","04133500",
             "04133501")
#
# Subset gages that are not in the set of regulated gages
gages <- subset(gages, !(gages[,1] %in% gagesReg))
#
for (i in 1:nrow(gages)){
# for (i in 61:61){
  cat(paste("Generating PSF",i,"for",gages[i,1],gages[i,2],"\n"))
  stationNo <- gages[i,1]
  peakFile  <- paste("./data/Raw/pk",stationNo,".txt",sep="")
  #
  # cat(paste("Working directory is ",getwd(),sep=""))
  # Read in the file 
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
  # Replace missing partial date info with "1001"
  if (length(ndxPartDate)>0){
    dateStr[ndxPartDate] <- paste(substr(dateStr[ndxPartDate],1,4),"1001",sep="")
  }
  #
  # Convert the character string to date
  dateVal  <- as.Date(dateStr, "%Y%m%d")
  # Find indices of historical floods
  ndxHistFlood <- grep("7",flowCode)
  # Find indices of peaks affected by dam failure
  ndxDamFail   <- grep("3",flowCode)
  # Convert calendar year to water year
  wYear    <- as.numeric(format(dateVal + 92, "%Y"))
  # Find water year(s) of historical floods
  wyrHistFlood <- wYear[ndxHistFlood]
  # Find magnitudes of historical floods
  magHistFlood <- flowVal[ndxHistFlood]
  # 
  par(cex.main=0.8)
  plot(dateVal,flowVal,xlab="Date of Peak Flow", ylab="Peak Flow, in cfs",
       main=paste("Annual Peak Flows at",staNo,staName),pch=16,
       col="blue",cex=0.7,log="y")
  # Add the historical peaks as red dots
  points(dateVal[ndxHistFlood],flowVal[ndxHistFlood],
         pch=16,col="red",cex=0.8)
  points(dateVal[ndxDamFail  ],flowVal[ndxDamFail],
         pch=16,col="green",cex=0.8)
  legend("bottomleft",legend=c("Systematic","Historical","Dam Failure"),
         col=c("blue","red","green"),
         pch=c(16,16,16),cex=0.5)
  #
  # Find lag1 differences between consecutive water years
  diffWY   <- diff(wYear, lag=1)
  # Length of ndxMisRec corresponds to the number of periods of missing record
  ndxMisRec <- which(diffWY > 1)
  # Open file for output
  fileConn <-file(paste("./data/Raw/pk",staNo,".psf",sep=""),open="wt")
  # Write psf file output
  writeLines(paste("I ASCI PK",staNo,".TXT",sep=""), fileConn, sep="\n")
  writeLines(paste("O File PK",staNo,".PRT",sep=""), fileConn, sep="\n")
  writeLines(paste("O Additional Watstore PK",staNo,".bcd",sep=""), fileConn, sep="\n")
  writeLines(paste("O Export PK",staNo,".exp",sep=""),fileConn, sep="\n")
  writeLines(paste("O Empirical PK",staNo,".emp",sep=""),fileConn, sep="\n")
  writeLines(paste("Station ",staNo,sep=""),fileConn, sep="\n")
  writeLines(paste("     Analyze EMA"),fileConn, sep="\n")
  writeLines(paste("     PCPT_Thresh ",min(wYear)," ",max(wYear),
                   " 0 1E+20 Default",sep=""),fileConn, sep="\n")
  # Determine if there is any missing record: length(ndxMisRec) > 0
  if (length(ndxMisRec > 0)) {
    for (j in 1:length(ndxMisRec)){
      # Populates lower threshold with min preceding historical flood
      # Find any historical floods before wYear[ndxMisRes[j]]
      if (any(wyrHistFlood < wYear[ndxMisRec[j]]+1)) {
        # Indices of wyrHistFlood that satify criteria
        ndxSelHistFlood <- which(wyrHistFlood < wYear[ndxMisRec[j]]+1)
        # Use the selected Historical floods to find min for preception threshold
        loPercThres <- as.character(min(magHistFlood[ndxSelHistFlood])) 
      }  else {
        loPercThres <- "1E+20"
      }
      writeLines(paste("     PCPT_Thresh ",wYear[ndxMisRec[j]] + 1," ",
                       wYear[ndxMisRec[j]] + diffWY[ndxMisRec[j]] -1,
                       " ",loPercThres," 1E+20 Missing record in historic period ",j,sep=""),
                 fileConn, sep="\n")
    }
  }
  # Determine if there are any peak flows affected by dam failures reset threshold
  if (length(ndxDamFail)>0) {
    for (j in 1:length(ndxDamFail)) {
      writeLines(paste("     PCPT_Thresh ",wYear[ndxDamFail[j]]," ",
                       wYear[ndxDamFail[j]],
                       " 1E+20 1E+20 Dam Break Affected Peak",j,sep=""),
                 fileConn, sep="\n")
    }
  }
  writeLines(paste("     BegYear ",min(wYear),sep=""),   fileConn, sep = "\n")
  writeLines(paste("     EndYear ",max(wYear),sep=""),   fileConn, sep = "\n")
  writeLines(paste("     HistPeriod ",nData + 1,sep=""),fileConn, sep = "\n")
  writeLines(paste("     SkewOpt Station",sep=""),      fileConn, sep = "\n")
  writeLines(paste("     LOType MGBT",sep=""),          fileConn, sep = "\n")
  close(fileConn)
}
