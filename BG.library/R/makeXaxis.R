makeXaxis<-function(addSetting, settingOverlay,xDomain){

 xAxis<- paste("xaxis  = list(tickmode = 'array',
               tickvals = seq(startTime,endTime,1),
               ticks = 'outside',
               ticktext = xticks,
               tick0 = xticks[1],",
                    xDomain,"      
               #domain = c(0,0.80),
               zeroline = FALSE,
               showline = TRUE,
               tickangle = 60
               
  )")

  return(xAxis)

}