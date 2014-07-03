# Compute skew and kurtosis to identify log Pearson family
# Load needed libraries
library(moments)
library(PearsonDS)
pearsonDiagram()
# readInPeakflow 
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
             "04133501","04157000","04161000","04162900")
#
# Subset gages that are not in the set of regulated gages
gages <- subset(gages, !(gages[,1] %in% gagesReg))
# Number of gages in analysis
nGages <- nrow(gages)
# Allocate a data frame to contain skew, kurtosis info
dfMoments <- data.frame(Streamgage = vector("character",nGages),
                        lnMean     = vector("numeric",nGages),
                        lnStd      = vector("numeric",nGages),
                        lnSkew     = vector("numeric",nGages),
                        lnKurt     = vector("numeric",nGages),
                        stringsAsFactors = FALSE)
#
# Output filename for station, wYear, flowVal
fileOut <- "./data/Raw/pearsonAnal.txt"
# Delete existing file
if (file.exists(fileOut)) file.remove(fileOut)
writeLines("staNum,wYear,annPeak", con=fileOut, sep="\n")
#
for (i in 1:nrow(gages)){
  # for (i in 1:10){
  cat(paste("Generating output for",i,"for",gages[i,1],gages[i,2],"\n"))
  stationNo <- gages[i,1]
  peakFile  <- paste("./data/Processed/pk",stationNo,".txt",sep="")
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
  # Extract info on largest peak since
  histInfo <- substr(peakData[5:nData],60,63)
  # Find nonblank string with dates of earlier historic data
  ndxInfo  <- which(nchar(gsub(" ","",histInfo)) > 0)
  # List the station and historical years
  #  cat("staNo",histInfo[ndxInfo],"\n")
  # Find indices of peaks with partial date information
  ndxPartDate  <- grep("B",flowCode)
  # Create date string from peak file data
  dateStr  <- substr(peakData[5:nData],17,24)
  # Replace missing partial date info with "1001"
  if (length(ndxPartDate)>0){
    for (j in 1:length(ndxPartDate)){
      ndxSpace <- regexpr(" ",dateStr[ndxPartDate[j]])
      if (ndxSpace[1]==5){
        # If only a four-digit year is given, assume that this is the water year
        dateStr[ndxPartDate[j]] <- paste(substr(dateStr[ndxPartDate[j]],1,4),"1001",sep="")
        # If the year and month is given, assume the peak occurred on the 15th day
      } else if (ndxSpace[1]==7) {
        dateStr[ndxPartDate[j]] <- paste(substr(dateStr[ndxPartDate[j]],1,6),"15",sep="")
        # If the year, month, and day is given, make no substitues
      } else if (ndxSpace[1]==-1) cat("No change to uncertain date",dateStr[ndxPartDate[j]])
    }
  }
  #
  # Convert the character string to date
  dateVal  <- as.Date(dateStr, "%Y%m%d")
  # Find indices of historical floods
  ndxHistFlood <- grep("7",flowCode)
  # Find indices of peaks affected by dam failure
  ndxDamFail   <- grep("3",flowCode)
  # Convert calendar year to water year: 92 adds days to convert yYear -> wYear
  wYear    <- as.numeric(format(dateVal + 92, "%Y"))
  # Find water year(s) of historical floods
  wyrHistFlood <- wYear[ndxHistFlood]
  # Find magnitudes of historical floods
  magHistFlood <- flowVal[ndxHistFlood]
  # 
  if (length(ndxHistFlood)>0) {
    # Append the station number, water year, and peak flow w/o historical data
    tmp <- cbind(rep(as.numeric(stationNo),length(wYear)-length(ndxHistFlood)),
                 as.matrix(wYear[-c(ndxHistFlood)],   ncol=1),
                 as.matrix(flowVal[-c(ndxHistFlood)], ncol=1))
  } else {
    # Append all station number, water year, and peak flow
    tmp <- cbind(rep(as.numeric(stationNo),length(wYear)),
                 as.matrix(wYear,   ncol=1),
                 as.matrix(flowVal, ncol=1))  
  }
  write.table(tmp, file = fileOut, append = TRUE, sep=",", col.name = FALSE,
              row.name = FALSE)
  # 
  # Compute some moments
  dfMoments[i,"Streamgage"] <- stationNo
  dfMoments[i,"lnMean"    ] <- mean(    log(tmp[,3]))
  dfMoments[i,"lnStd"     ] <- sd(      log(tmp[,3]))
  dfMoments[i,"lnSkew"    ] <- skewness(log(tmp[,3]))
  dfMoments[i,"lnKurt"    ] <- kurtosis(log(tmp[,3]))
  #   
}
beta1 <- dfMoments[,"lnSkew"]^2
beta2 <- dfMoments[,"lnKurt"] + 3

plot(beta1,beta2)

points(beta1,beta2)
