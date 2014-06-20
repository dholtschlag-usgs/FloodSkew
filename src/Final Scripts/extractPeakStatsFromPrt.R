# This script reads the pk[stationNo].txt file and extracts needed statistics
# (1) 

# Dave Holtschlag, USGS MI-WSC, June 2014
#
# Turn print (cat) data off/on
icat <- 0  # 0 is don't print
#
# Set working directory
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
# 
gages = read.table("./data/miGagesIIa.txt", sep="\t",header=TRUE,
                   colClasses=c("character","character"))
# 
# Streamgages with regulated flow or superseded (more record at nearby site) or trend
gagesReg = c("04034500","04035500","04036000","04044400","04058100","04058200",
             "04059000","04061500","04062000","04062500","04063000","04066800",
             "04067000","04108801","04114500","04128000","04129000","04133500",
             "04133501","04157000","04161000","04162900","04170500",
             # Stations to re-analyze
             "04097500","04105000","04121500","04154000")  
#
# Subset gages that are not in the set of regulated gages
gages    <- subset(gages, !(gages[,1] %in% gagesReg))
numGages <- nrow(gages)
#
# Initialize date frame df1 for peak flow statistics
df1 <- data.frame("Station"       = character(numGages),  #  1
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
  df1[i,"Station"] <- gages[i,1]
  df1[i,"State"]   <- "Michigan"
  cat(i,gages[i,1],gages[i,2],"\n")
  prtFile          <- file(paste("./data/raw/pk",gages[i,1],".prt",sep=""),"r")
  fileContents     <- readLines(con = prtFile)
  close(prtFile)
  # 
  # Read: Number of peaks in record
  strPeakRec  <- fileContents[grep("Number of peaks in record            =",fileContents)]
  df1[i,"numPeakRec"]  <- as.numeric(substr(strPeakRec,60,63))
  if (icat) cat("Number of peaks in record:",df1[i,"numPeakRec"],"\n")
  # Read: Peaks not used in analysis
  strPeaksNot  <- fileContents[grep("Peaks not used in analysis           =",fileContents)]
  df1[i,"peaksNotUsed"] <- as.numeric(substr(strPeaksNot,60,64))
  if (icat) cat("Peaks not used in analysis:",df1[i,"peaksNotUsed"],"\n")
  
  # Read: Systematic peaks in analysis
  strSysPeaks <- fileContents[grep("Systematic peaks in analysis         =",fileContents)]
  df1[i,"numSysPeaks"] <- as.numeric(substr(strSysPeaks,60,64))
  if (icat) cat("Systematic peaks in analysis:",df1[i,"numSysPeaks"],"\n")
  
  # Read: Historic peaks in analysis
  strHistPeaks <- fileContents[grep("Historic peaks in analysis           =",fileContents)]
  df1[i,"numHistPeaks"] <- as.numeric(substr(strHistPeaks,60,64))
  if (icat) cat("Historic peaks in analysis:",df1[i,"numHistPeaks"],"\n")
  
  
  # Read: Beginning Year
  strBegYear   <- fileContents[grep("Beginning Year                       =",fileContents)]
  df1[i,"begYear"]     <- as.numeric(substr(strBegYear,60,63))
  if (icat) cat("Beginning Year          :",df1[i,"begYear"],"\n")
  # Read: Ending Year
  strEndYear   <- fileContents[grep("Ending Year                          =",fileContents)]
  df1[i,"endYear"]     <- as.numeric(substr(strEndYear,60,63))
  if (icat) cat("Ending Year             :",df1[i,"endYear"],"\n")
  # Read: Historical Period Length
  strHistLen   <- fileContents[grep("Historical Period Length             =",fileContents)]
  df1[i,"histPerLen"]   <- as.numeric(substr(strHistLen,60,63))
  if (icat) cat("Historical Period Length:",df1[i,"HistRecLen"],"\n")
  # 
  # Read: EMA W/O REG. INFO        
  strEmaStats  <- fileContents[grep("EMA W/O REG. INFO        ",fileContents)]
  df1[i,"emaMean"]      <- as.numeric(substr(strEmaStats,27,34))
  if (icat) cat("EMA W/O REG. Mean:",df1[i,"emaMean"])
  df1[i,"emaStd"]       <- as.numeric(substr(strEmaStats,38,46))
  if (icat) cat(",  Std:",df1[i,"emaStd"])
  df1[i,"emaSkew"]      <- as.numeric(substr(strEmaStats,50,57))
  if (icat) cat(", Skew:",df1[i,"emaSkew"],"\n")
  #
  # Read: EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)    
  strMseSkewReg  <- fileContents[grep("EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE)",
                                      fixed = TRUE, fileContents)]
  df1[i,"mseSkewWOReg"]     <- as.numeric(substr(strMseSkewReg,58,64))
  if (icat) cat("EMA ESTIMATE OF MSE OF SKEW W/O REG. INFO (AT-SITE): ",df1[i,"mseSkewReg"],"\n")
  
  # Read: EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE) 
  strMseSkewSys  <- fileContents[grep("EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE) ",
                                      fixed = TRUE, fileContents)]
  df1[i,"mseSkewWSys"]     <- as.numeric(substr(strMseSkewSys,58,64))
  if (icat) cat("EMA ESTIMATE OF MSE OF SKEW W/SYSTEMATIC ONLY (AT-SITE): ",df1[i,"mseSkewSys"],"\n")
  
  # Read: EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST 
  strLoOutlier   <- fileContents[grep("EMA003I-PILFS (LOS) WERE DETECTED USING MULTIPLE GRUBBS-BECK TEST ",
                                      fileContents)]    
  if (length(strLoOutlier)==0){
    df1[i,"numLoOutlier"]   <- 0
    df1[i,"mgbLOThres"]     <- NaN
  } else {
    df1[i,"numLoOutlier"]   <- as.numeric(substr(strLoOutlier,71,73))
    df1[i,"mgbLOThres"]     <- as.numeric(substr(strLoOutlier,76,83)) 
  }
  # List results to console
  if (icat) cat("Number of low outliers: ",df1[i,"numLoOutlier"],"\n")
  if (icat) cat("MGB low outlier threshold: ",df1[i,"mgbLOThres"],"\n")
  # 
  # Define string for tau values
  str <- "TAU    P-VALUE    SLOPE   PEAKS"
  tauDataLine               <- grep(str,fileContents) + 2
  df1[i,"tauParameter"]     <- as.numeric(substr(fileContents[tauDataLine],34,42))
  df1[i,"tauP_value"]       <- as.numeric(substr(fileContents[tauDataLine],46,54))
  df1[i,"tauMedSlope"]      <- as.numeric(substr(fileContents[tauDataLine],56,65))
  df1[i,"tauNoPeaks"]       <- as.numeric(substr(fileContents[tauDataLine],66,69))
}
# Copy dataframe to better name
PeakFlowStats             <- df1
# Remove old dataframe
rm(df1)