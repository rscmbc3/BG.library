#'@title addPumpSetting
#'@description Function to plot pump settings as lines  \\cr \\cr
#'@param data data to be plotted 
#'@param color color of line
#'@param legendName character string given to pump setting line in the legend
#'@param bigTick number indicating major breaks in axis labels
#'@param littleTick number indicating minor breaks in axis labels
#'@param axisLevel number indicating how far to the right the new axis should be placed (number of previous axes)
#'@param legendChar character vector of all legend text
#'@param legendCol character vector of all legend colors
#'@param legendFill character vector of all legend point colors
#'@param legendLty character vector of all legend line types 
#'@param legendpch character vector of all legend point types 
#'@returns `plotTracker` list of new legend parameters and axis position


addPumpSetting<-function(data, color, legendName,
                         bigTick, littleTick,
                         axisLevel,
                         legendChar, legendCol, legendFill,legendLty,legendpch,legendBoxCol){

  #initialize plot
  par(new = TRUE)
  plot(y =data[[length(data)]],x = round(as.POSIXct(data[[1]],format="%H:%M"),"hours"),
       col = color, lwd = 2, lty = 4, ylim = c(0,max(data[[length(data)]])), type = "l",
       xaxt = "n",yaxt = "n",xlab = "",ylab= "")
  
  #set location of current and next axis
  axisLevel<-axisLevel+3
  
  axis(side = 4,line=axisLevel, tck =-0.05,
       at = seq(0,max(data[[length(data)]]),bigTick),
       col = color, col.ticks = color,las = 2)
  axis(side = 4,line=axisLevel,labels = FALSE,tck=-0.02,
      at = seq(0,max(data[[length(data)]]),littleTick), 
       col = color, col.ticks = color,las = 2)
  
  #plot tracker data for legend
  legendChar<-c(legendChar,legendName)
  legendCol<-c(legendCol,color)
  legendFill<-c(legendFill,NA)
  legendLty<-c(legendLty,4)
  legendpch<-c(legendpch,NA)
  legendBoxCol<-c(legendBoxCol,NA)

  plotTracker<-named.list(legendChar, legendCol, legendFill,legendLty,legendpch,axisLevel,legendBoxCol)
  return(plotTracker)
}