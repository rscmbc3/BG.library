#'@title summaryPlot
#'@description Function to plot min, mean, max per hour lines  \\cr \\cr
#'@param data data to be plotted
#'@param plotSummary character string indicating the column to be summarised
#'@param legendChar character vector of all legend text
#'@param legendCol character vector of all legend colors
#'@param legendFill character vector of all legend point colors
#'@param legendLty character vector of all legend line types 
#'@param legendpch character vector of all legend point types 
#'@returns `plotTracker` list of new legend parameters


summaryPlot<-function(data, plotSummary,margins,
                      legendChar, legendCol, legendFill,legendLty,legendpch){
  
  
sumSensor<-data[c("time2",plotSummary)]
sumSensor$time3<-as.POSIXct(round(as.POSIXct(sumSensor$time2,format="%H:%M"),"hours"))
sumSensor<-sumSensor[c("time3",plotSummary)]
sumSensor<-as.data.frame(sumSensor %>% group_by(time3) %>% summarise_all(funs(min, mean, max),na.rm = TRUE))
par(xpd = FALSE,mar = margins)
plot(round(as.POSIXct(sumSensor$time3,format="%H:%M"),"hours"),sumSensor$min,xaxt="n",
     type = "n", ylim = c(0,400), xlab = "",ylab="BG", yaxt = "n",col = "gray",
     main = paste0(min(data$Date2)," -to- ",max(data$Date2)))
r <- as.POSIXct(round(range(as.POSIXct(sumSensor$time3,format="%H:%M")), "hours"))
axis.POSIXct(1, as.POSIXct(sumSensor$time3,format="%H:%M"), at = seq(r[1], r[2], by = "hour"), format="%H:%M", las = 2)
axis(side = 2, las = 2)
lines( y =sumSensor$min,x = round(as.POSIXct(sumSensor$time3,format="%H:%M"),"hours"),col = "gray", lwd = 2)
lines( y =sumSensor$max,x = round(as.POSIXct(sumSensor$time3,format="%H:%M"),"hours"),col = "gray", lwd = 2)
lines( y =sumSensor$mean,x = round(as.POSIXct(sumSensor$time3,format="%H:%M"),"hours"),col = "black", lwd = 2)

legendChar<-c(legendChar,paste0(plotSummary,"_",c("min","mean","max")))
legendCol<-c(legendCol,c("gray","black","gray"))
legendLty<-c(legendLty,rep(1,3))
legendpch<-c(legendpch,rep(NA,3))
legendFill<-c(legendFill,rep(NA,3))

plotTracker<-named.list(legendChar, legendCol, legendFill,legendLty,legendpch)

return(plotTracker)
}