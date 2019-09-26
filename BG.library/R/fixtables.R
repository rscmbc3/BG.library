#'@title fixtables
#'@description Reformat input tables to create readable data.frame objects. Format dateTime columns as.POSIXct.  \\cr \\cr
#'@param data object from `readLines()` import to convert to readable data.frame.
#'@return `data` data.frame with correct headers and dateTime class columns.

fixtables<-function(data){
  #replace column names
  for (i in 1:length(data)){
    names(data)[i]<-as.character(data[1,i])
  }
  data<-data[-c(1),]
  
  #write to csv and then read to get colclasses
  tempfile = paste0(path,"temp.csv")
  write.csv(file = tempfile,data, row.names = FALSE)
  
  data<-read.csv(tempfile)
  data[,names(data) %in% c("Sensor.Glucose..mg.dL.","ISIG.Value")]<-sapply(data[,names(data) %in% c("Sensor.Glucose..mg.dL.","ISIG.Value")], function(x) as.numeric(x))
  
  
  #remove index
  data<-data[,-c(1)]
  #convert date/times
  data$Date<-format(as.POSIXct(as.character(data[,1]),tx = "EDT", origin = "1899-12-30"),"%Y-%m-%d")
  data$Time<-format(as.POSIXlt(as.character(data[,2]),format="%H:%M"), format="%H:%M")
  data$dateTime<-as.POSIXct(paste(data$Date, data$Time,"EDT"))
  return(data)
}