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
    
    #get changes as dates
    allChanges<-character(0)
    class(allChanges)<-"Date"
    for (d in 2:length(basal)){
      change<-names(basal)[d]
      change<-gsub("X","",change)
      change<-gsub("\\.","-",change)
      change<-as.Date(change,format = "%m-%d-%Y",origin = "1970-01-01")
      change<-as.Date(change, format = "%Y-%m-%d" )
      allChanges<-c(allChanges,change)
    }
    
    #get max dates in data
    maxDate<-max(data$Date2)
    lastChange<-allChanges[allChanges<=maxDate]
    lastChange<-lastChange[length(lastChange)]
    
    #subset data
    data<-data[data$Date2>=lastChange,]

}
return(data)
}