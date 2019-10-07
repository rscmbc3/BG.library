fromChangeDateRange<-function(data,numberDays,fromChange,startDate = NA, endDate = NA, libraryPath){
  if (!fromChange){
    #set date range, numberDays overrules start/endDates
    if (!is.na(numberDays)){
      data<-data[data$Date2>=max(data$Date2)-numberDays+1,]
    }else if (!is.na(startDate) | !is.na(endDate)){
      if (!is.na(startDate)){
        startDate<-as.Date(startDate, format = "%Y-%m-%d" )
        data<-data[data$Date2>=startDate,]
      }
      if (!is.na(endDate)){
        endDate<-as.Date(endDate, format = "%Y-%m-%d" )
        data<-data[data$Date2<=endDate,]
      }
    }#end set date range 
    
}else{
  basal<-makePumpSettings(libraryPath)$basal
  lastChange<-names(basal)[length(basal)]
  lastChange<-gsub("X","",lastChange)
  lastChange<-gsub("\\.","-",lastChange)
  lastChange<-as.Date(lastChange,format = "%m-%d-%Y",origin = "1970-01-01")
  lastChange<-as.Date(lastChange, format = "%Y-%m-%d" )
  prevChange<-names(basal)[length(basal)-1]
  prevChange<-gsub("X","",prevChange)
  prevChange<-gsub("\\.","-",prevChange)
  prevChange<-as.Date(prevChange,format = "%m-%d-%Y",origin = "1970-01-01")
  prevChange<-as.Date(prevChange, format = "%Y-%m-%d" )
  if (nrow(data[data$Date2>=lastChange,])!=0){
    data<-data[data$Date2>=lastChange,]
  }else{
    data<-data[data$Date2>=prevChange,]
  }
}
  return(data)
}