subsetData<-function(data,numberDays,startDate,endDate,filterCond,
                     timeStep,period, fromChange = TRUE,libraryPath){
  
  
  data<-fromChangeDateRange(data,numberDays,fromChange,startDate, endDate, libraryPath)
    
  
  #apply filter condition
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  
  #regenerate new time/date columns based on timeStep and period
  data<-setTimeStep(data, timeStep, period)
  
  return(data)
}