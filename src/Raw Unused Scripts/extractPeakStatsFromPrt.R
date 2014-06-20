# This script reads the pk[stationNo].txt file and extracts needed statistics
#
# Set working directory
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
# 
prtFile      <- file("./data/raw/pk04039500.prt","r")
fileContents <- readLines(con = prtFile)
close(prtFile)

# Read: Number of peaks in record
strPeakRec  <- fileContents[grep("Number of peaks in record            =",fileContents)]
numPeakRec  <- as.numeric(substr(strPeakRec,60,63))
cat("Number of peaks in record:",numPeakRec,"\n")
# Read: Peaks not used in analysis
strPeaksNot  <- fileContents[grep("Peaks not used in analysis           =",fileContents)]
peaksNotUsed <- as.numeric(substr(strPeaksNot,60,64))
cat("Peaks not used in analysis:",peaksNotUsed,"\n")

# Read: Systematic peaks in analysis
strSysPeaks <- fileContents[grep("Systematic peaks in analysis         =",fileContents)]
numSysPeaks <- as.numeric(substr(strSysPeaks,60,64))
cat("Systematic peaks in analysis:",numSysPeaks,"\n")

# Read: Historic peaks in analysis
strHistPeaks <- fileContents[grep("Historic peaks in analysis           =",fileContents)]
numHistPeaks <- as.numeric(substr(strHistPeaks,60,64))
cat("Historic peaks in analysis:",numHistPeaks,"\n")


# Read: Beginning Year
strBegYear  <- fileContents[grep("Beginning Year                       =",fileContents)]
begYear     <- as.numeric(substr(strBegYear,60,63))
cat("Beginning Year          :",begYear,"\n")
# Read: Ending Year
strEndYear  <- fileContents[grep("Ending Year                          =",fileContents)]
endYear     <- as.numeric(substr(strEndYear,60,63))
cat("Ending Year             :",endYear,"\n")
# Read: Historical Period Length
strHistLen  <- fileContents[grep("Historical Period Length             =",fileContents)]
HistRecLen  <- as.numeric(substr(strHistLen,60,63))
cat("Historical Period Length:",HistRecLen,"\n")
# 
# Read: EMA W/O REG. INFO        
strEmaStats  <- fileContents[grep("EMA W/O REG. INFO        ",fileContents)]
EmaMean      <- as.numeric(substr(strEmaStats,27,34))
cat("EMA W/O REG. Mean:",EmaMean)
EmaStd       <- as.numeric(substr(strEmaStats,38,46))
cat(",  Std:",EmaStd)
EmaSkew      <- as.numeric(substr(strEmaStats,50,57))
cat(", Skew:",EmaSkew,"\n")
#
# Read: EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)    
strMseSkewReg  <- fileContents[grep("EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)",
                                    fixed = TRUE, fileContents)]
MseSkewReg     <- as.numeric(substr(strMseSkewReg,58,64))
cat("EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE): ",MseSkewReg,"\n")

# Read: EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE) 
strMseSkewSys  <- fileContents[grep("EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE) ",
                                    fixed = TRUE, fileContents)]
MseSkewSys     <- as.numeric(substr(strMseSkewSys,58,64))
cat("EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE): ",MseSkewSys,"\n")

# Read: EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST 
strLoOutlier   <- fileContents[grep("EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST ",
                                    fileContents)]    
if (length(strLoOutlier)==0){
  numLoOutlier   <- 0
  mgbLOThres     <- NaN
} else {
  numLoOutlier   <- as.numeric(substr(strLoOutlier,71,73))
  mgbLOThres     <- as.numeric(substr(strLoOutlier,76,83)) 
}
# List results to console
cat("Number of low outliers: ",numLoOutlier,"\n")
cat("MGB low outlier threshold: ",mgbLOThres)




