#'@title convertClass
#'@description Converts column classes \\cr \\cr
#'@param cls character string for the desired class conversion
#'@param col charcter string for the column to convert
#'@param sensorData data.frame of sensor BG values
#'@param pumpData data.frame of pumpData
#'@return `convert` vector of converted values with class equal to `cls`


convertClass<-function(cls,col,sensorData,pumpData){
  if (cls=="factor"){
    convert<-eval(parse(text=paste("factor(sensorData$`",col,"`, levels = as.character(levels(pumpData$`",col,"`)))",sep="")))
  }else if (cls=="character"){
    convert<-eval(parse(text=paste("as.character(sensorData$`",col,"`)",sep="")))
  }else if (cls=="integer"){
    convert<-eval(parse(text=paste("as.integer(as.character(sensorData$`",col,"`))",sep="")))
  }else if (cls=="numeric"){
    convert<-eval(parse(text=paste("as.numeric(as.character(sensorData$`",col,"`))",sep="")))
  }else if (cls=="logical"){
    convert<-eval(parse(text=paste("ifelse(as.character(sensorData$`",col,"`),FALSE,TRUE)",sep="")))
  }else if (cls=="Date"){
    convert<-eval(parse(text=paste("as.Date(as.character(sensorData$`",col,"`),format='%Y-%m-%d')",sep="")))
  }
  return(convert)
}