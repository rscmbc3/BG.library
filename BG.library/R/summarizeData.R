summarizeData<-function(data, colName, sumFuncs = "min, mean, max, sd", 
                        numberDays, filterCond = "",
                        timeStep = "hour", period = 1){
  #subset for numberDays
  data<-data[data$Date2>=max(data$Date2)-numberDays+1,]
  
  
  
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  if(nrow(data)!=0){
  #get unique values
  NAMES<-c("dateTime","Date2","time2","hour",colName)
  data<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,timeStep = timeStep, period = period)
  
  data<-data[c("time2",colName)]
  data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
  data<-data[c("time3",colName)]


  
  if (sumFuncs!="length"){
    sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                      sumFuncs,"),na.rm = TRUE))")
  }else{#length
    sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                      sumFuncs,")))")
    
  } 
  data<-eval(parse(text = sumString))
  data$time3<-format(data$time3, "%H:%M")
  return(data)
  
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}