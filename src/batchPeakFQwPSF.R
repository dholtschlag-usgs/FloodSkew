# Run PeakFQ w/ psf
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
#
gages = read.table("./data/miGagesIIa.txt", sep="\t",header=TRUE,
                   colClasses=c("character","character"))
#
# Streamgages with regulated flow
gagesReg = c("04034500","04035500","04036000","04044400","04058100","04058200",
             "04059000","04061500","04062000","04062500","04063000","04066800",
             "04067000","04108801","04114500","04128000","04129000","04133500",
             "04133501","04157000","04161000","04162900")
#
# Find set of gages that are not in the set of regulated gages
gages <- subset(gages, !(gages[,1] %in% gagesReg))
#
# Batch PeakFQ
system("C:/Program Files (x86)/PeakFQ/bin/PKFQWin.exe") # pk04033000.psf")

system("c:\\Program Files (x86)\\PeakFQ\\bin\\PKFQWin.exe,pk04033000.psf")

