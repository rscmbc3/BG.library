summaryLinePlot_ly<-function(data, plotSummary, p){
  
  
  sumdata<-data[c("time2",plotSummary)]

  sumdata$time3<-as.POSIXct(round(as.POSIXct(sumdata$time2,format="%H:%M"),"hours"))

  sumdata<-sumdata[c("time3",plotSummary)]
  sumdata<-as.data.frame(sumdata %>% group_by(time3) %>% summarise_all(funs(min, mean, max),na.rm = TRUE))

  #round time to 0:00 that rounds to next day (i.e. closer to 24hrs and 23hrs)
  if (unique(sumdata$time3)[length(unique(sumdata$time3))]-unique(sumdata$time3)[1]==1){
    for (i in 1:nrow(sumdata)){
      if (sumdata$time3[i]==unique(sumdata$time3)[length(unique(sumdata$time3))]){
        sumdata$time3[i]<-unique(sumdata$time3)[1]
      }
    }
}

  sumdata$time3<-as.POSIXlt(sumdata$time3,format="%H:%M")
  sumdata$hours<- sumdata$time3$hour + sumdata$time3$min/60 
  sumdata<-sumdata[order(sumdata$time3),]

  
  p <- p %>% add_trace(data=sumdata, 
                       x=~hours, y=~min, mode='lines',
                       type="scatter", color=I("gray"), 
                       hoverinfo = 'text',
                       text = ~paste(
                                     '</br> Time: ',format(time3,"%H:%M"),
                                     paste0("'</br> ",plotSummary," min value : '"),min),
                       name = paste0("min_",plotSummary))
  p <- p %>% add_trace(data=sumdata, 
                       x=~hours, y=~mean, mode='lines',
                       type="scatter", color=I("black"), 
                       hoverinfo = 'text',
                       text = ~paste(
                         '</br> Time: ',format(time3,"%H:%M"),
                         paste0("'</br> ",plotSummary," mean value : '"),round(mean)),
                       name = paste0("mean_",plotSummary))
  p <- p %>% add_trace(data=sumdata, 
                       x=~hours, y=~max, mode='lines',
                       type="scatter", color=I("gray"), 
                       hoverinfo = 'text',
                       text = ~paste(
                         '</br> Time: ',format(time3,"%H:%M"),
                         paste0("'</br> ",plotSummary," max value : '"),max),
                       name = paste0("max_",plotSummary))
  return(p)
  
  
}#end function
  