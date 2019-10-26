#'@title fixtables
#'@description Reformat input tables to create readable data.frame objects. Format dateTime columns as.POSIXct.  \\cr \\cr
#'@param data object from `readLines()` import to convert to readable data.frame.
#'@return `data` data.frame with correct headers and dateTime class columns.
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'#readBG
#'data<-read.csv(filePath, header = FALSE)
#'
#'#get pump table
#'pumpStartRow<-which(data[,1]=="Index")[1]
#'pumpEndRow<-which(data[,1]=="-------" & as.numeric(rownames(data))>pumpStartRow)-1
#'pumpData<-data[pumpStartRow:pumpEndRow,]
#'
#'#fix tables
#'pumpData<-fixtables(pumpData,libraryPath)


fixtables<-function(data,libraryPath){
  #replace column names
  for (i in 1:length(data)){
    names(data)[i]<-as.character(data[1,i])
  }
  data<-data[-c(1),]
  
  #write to csv and then read to get colclasses
  tempfile = paste0(libraryPath,"data/temp.csv")
  write.csv(file = tempfile,data, row.names = FALSE)
  
  data<-read.csv(tempfile)
  data[,names(data) %in% c("Sensor.Glucose..mg.dL.","ISIG.Value")]<-sapply(data[,names(data) %in% c("Sensor.Glucose..mg.dL.","ISIG.Value")], function(x) as.numeric(x))
  
  
  #remove index
  data<-data[,-c(1)]
  #convert date/times
  data<-data[as.character(data[,1])!="",]
  dataOrig<-data
  data$Date<-format(as.POSIXct(as.character(data[,1]),tx = "EDT", origin = "1899-12-30", format = "%m/%d/%Y"),"%Y-%m-%d")
  data$Time<-format(as.POSIXlt(as.character(data[,2]),format="%H:%M"), format="%H:%M")
  
  tryIt<-try({
data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"))
  }, silent =TRUE)

  if (class(tryIt)[1]=="try-error"){
    data$Date<-format(as.POSIXct(as.character(dataOrig[,1]),tx = "EDT", origin = "1899-12-30"),"%Y-%m-%d")
    data$Time<-format(as.POSIXlt(as.character(dataOrig[,2]),format="%H:%M"), format="%H:%M")
    data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"))
  }else{
    data$Date<-format(as.POSIXct(as.character(dataOrig[,1]),tx = "EDT", origin = "1899-12-30", format = "%m/%d/%Y"),"%Y-%m-%d")
    data$Time<-format(as.POSIXlt(as.character(dataOrig[,2]),format="%H:%M"), format="%H:%M")
    data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"))
  }

  return(data)
}