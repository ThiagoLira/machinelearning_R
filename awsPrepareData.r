
#recieve input zones from command line
args = commandArgs(trailingOnly=TRUE)
#default values
if (length(args)==0){
  args <- c("112","113","114")
}

#convert to numbers
zones <- as.numeric(args)

source("serverFunctions.r")


dataPath <- "raw-data"


datasets <- prepareUCIdata(dataPath,zones)


saveRDS(datasets,"prepared-data/UCIdata.rds")

