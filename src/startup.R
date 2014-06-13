# Initiate Flood skew analysis
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
library("ProjectTemplate")
load.project()
# Put miGagesIIa.txt into easy format
gages = read.table("./data/miGagesIIa.txt", sep="\t",header=TRUE,
                   colClasses=c("character","character"))
gages[1:3,]
# Count the number of gages
nGages = nrow(gages)
# Set up prefix and suffix for URL retriving peak flow data
myUrla <- "http://nwis.waterdata.usgs.gov/nwis/peak?site_no="
myUrlb <- "&agency_cd=USGS&format=hn2"
# 
for (i in 1:nGages){
  cat(gages[i,1],"\n")
  peakTable <- readLines(paste(myUrla,gages[i,1],myUrlb,sep=""))
  writeLines(peakTable, con = paste("./data/Raw/pk",gages[i,1],".txt",sep=""))
}
setwd("./data/Raw/")
getwd()
system(paste("PeakfqSA0998 TestCase02.spc"))
