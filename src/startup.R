# Initiate Flood skew analysis
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
library("ProjectTemplate")
load.project()
# Put miGagesIIa.txt into easy format
gages = read.table("./data/miGagesIIa.txt", sep="\t",header=TRUE,
                   colClasses=c("character","character"))
# Gages that are regulated
# Streamgages with regulated flow
gagesReg = c("04034500","04035500","04036000","04044400","04058100","04058200",
             "04059000","04061500","04062000","04062500","04063000","04066800",
             "04067000","04108801","04114500","04128000","04129000","04133500",
             "04133501","04157000","04161000","04162900")
#
# Take set difference between gages and gagesReg for gagesUse
gagesUse   = setdiff(gages[,1],gagesReg)
# Count the number of gages
nGagesUse = length(gagesUse)
# Set up prefix and suffix for URL retriving peak flow data
myUrla <- "http://nwis.waterdata.usgs.gov/nwis/peak?site_no="
myUrlb <- "&agency_cd=USGS&format=hn2"
# 
for (i in 1:nGagesUse){
  cat(gagesUse[i],"\n")
  peakTable <- readLines(paste(myUrla,gagesUse[i],myUrlb,sep=""))
  writeLines(peakTable, con = paste("./data/Raw/pk",gagesUse[i],".txt",sep=""))
}
setwd("./data/Raw/")
getwd()
system(paste("PeakfqSA0998 TestCase02.spc"))
