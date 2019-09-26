subsetData<-function(data,numberDays,startDate,endDate,filterCond){
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
  
  
  
  #apply filter condition
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
   return(data)
}