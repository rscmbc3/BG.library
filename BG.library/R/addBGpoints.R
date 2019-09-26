addBGpoints<-function(data,axisLevel,legendChar,legendCol,legendLty,legendpch,legendFill,legendBoxCol,
                      scatterOnly){
  par(new=TRUE)
  plot(y = data$BG.Reading..mg.dL.,x = as.POSIXct(data$time2,format="%H:%M"), 
       xaxt = "n",yaxt = "n", pch = 21, bg = "black", xlab = "",ylab= "",ylim = c(0,400))
if (!is.na(axisLevel)){
  axis(side = 4,line=0,tck=-0.05,
       at = seq(0,400,100), 
       col = "black", col.ticks = "black",las = 2)
  axis(side = 4,line=0,labels = FALSE,tck=-0.01,
       at = seq(0,400,50), 
       col = "black", col.ticks = "black",las = 2)
}
 
  if (scatterOnly){
    r <- as.POSIXct(round(range(as.POSIXct(data$time2,format="%H:%M")), "hours"))
    axis.POSIXct(1, as.POSIXct(data$time2,format="%H:%M"), at = seq(r[1], r[2], by = "hour"), format="%H:%M", las = 2)
    axis(side = 2, las = 2) 
  } 
  legendChar<-c(legendChar,"meterBG")
  legendCol<-c(legendCol,"black")
  legendLty<-c(legendLty,NA)
  legendpch<-c(legendpch,16)
  legendFill<-c(legendFill,"black")
  legendBoxCol<-c(legendBoxCol,NA)
  
  plotTracker<-named.list(legendChar, legendCol, legendFill,legendLty,legendpch,legendBoxCol,axisLevel)
  return(plotTracker)
}