# Initiate Flood skew analysis
setwd("C:/Home/Projects/FloodSkew/Analysis/R/FloodSkew")
library("ProjectTemplate")
load.project()
# Put miGagesIIa.txt into easy format
gages = read.table("./data/miGagesIIa.txt", sep="\t",header=TRUE,
                   colClasses=c("character","character"))
gages[1:3,]
