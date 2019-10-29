summarizeData<-function(data, colName, sumFuncs = "min, mean, max, sd", 
                        numberDays = NA, filterCond = "",
                        startTime = "00:00", endTime = "23:00",timeStep = "hour", period = 1, 
                        startDate = NA, endDate = NA,fromChange=TRUE,removeDates = NA,libraryPath){
  #get dateRange
  data<-fromChangeDateRange(data,fromChange,numberDays,libraryPath = libraryPath,
                            startDate = startDate,endDate = endDate,removeDates = removeDates)
  
  
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  if(nrow(data)!=0){
  #get unique values
  NAMES<-c("dateTime","Date2","time2","hour",colName)
  data<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,startTime = startTime,endTime = endTime,timeStep = timeStep, period = period)
  
  #set time series column to aggregate on
  if (timeStep=="hour"){
     data<-data[c("time2",colName)]
  data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
  }else if (timeStep=="day"){
    data$time3<-data$Date2
  }

data<-data[c("time3",colName)] 

  
  if (sumFuncs!="length"){
    sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                      sumFuncs,"),na.rm = TRUE))")
  }else{#length
    sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                      sumFuncs,")))")
    
  } 
  data<-eval(parse(text = sumString))
  
  #format output data timestep column
  if (timeStep=="hour"){
  data$time3<-format(data$time3, "%H:%M")
  }else if (timeStep=="day"){
  names(data)[names(data)=="time3"]<-"Date2"
  data$Date2<-as.Date(data$Date2, format = "%Y-%m-%d")
  }
  
  return(data)
  
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}