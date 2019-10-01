setTimeStep<-function(data, timeStep, period){
  #regenerate time2, hour, minute based on timeStep and period
  if (timeStep=="hour"){
    seqTime<-seq.POSIXt(as.POSIXct("00:00",format="%H:%M"),as.POSIXct("23:00",format="%H:%M"),by = paste0(period," hour"))
    data$timeNew<-as.POSIXct(data$time2,format="%H:%M")
    data$timeNew2<-as.numeric(rep(NA,nrow(data)))
    for (s in 2:length(seqTime)){
      if (s==1){
        data<-transform(data, timeNew2 = ifelse(is.na(data$timeNew2) & timeNew<=seqTime[s],seqTime[s],timeNew2))
      }else{
        data<-transform(data, timeNew2 = ifelse(is.na(data$timeNew2) & timeNew<seqTime[s] & timeNew>=seqTime[s-1],seqTime[s-1],timeNew2))
      }
    }
    #add last in sequence
    data<-transform(data, timeNew2 = ifelse(is.na(data$timeNew2) & timeNew>=seqTime[length(seqTime)],seqTime[length(seqTime)],timeNew2))
    
    #replace core columns
    data$time2<-as.POSIXlt(data$timeNew2, origin = "1970-01-01")
    data$hour<-data$time2$hour
    data$minute<-minute(data$time2)
    data$time2<-format(data$time2,"%H:%M")
    data<-data[,regexpr("timeNew",names(data))<0]
  }
  return(data)
}