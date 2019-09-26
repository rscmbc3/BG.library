prettyTime<-function(data,timeCol){
  tempTime<-eval(parse(text = paste0("data$",timeCol)))
  data$Date2<-as.Date(tempTime, format = "%Y-%m-%d" )
  data$time2<-format(tempTime, "%H:%M")
  data$hour<-hour(tempTime)
  data$minute<-minute(tempTime)
  return(data)
}