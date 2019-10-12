#'@title addBGpoints_ly
#'@description Adds BG values as scatter trace to plot_ly interactive plot \\cr \\cr
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param p current plot_ly plot
#'@param addBG TRUE/FALSE whether BG values should be added to current plot
#'@param pointSize scatter point size will be reduced by pointSize/1.5 for BG values
#'@return `p` plot_ly interactive plot with BG values added if addBG


addBGpoints_ly<-function(data, p, yAxis = 'y', addBG, pointSize,startTime,endTime){
  if (addBG){#add bG values
    NAMES<-c("dateTime","Date2","time2","hours","hour","BG.Reading..mg.dL.")
    data<-uniqueDateTime(data, NAMES, replaceNAs = FALSE,startTime = startTime,endTime = endTime, timeStep = "hour", period = 1)

    p <- p %>% add_trace( data = data, x = ~hours, y = ~BG.Reading..mg.dL., 
                          type = "scatter", 
                          mode = "markers",
                          marker = list(
                            color = "black",
                            size = pointSize/1.5),
                          hoverinfo = 'text',
                          text = ~paste('</br> Date: ',Date2,
                                        '</br> Time: ',time2,
                                        '</br> BG value :',BG.Reading..mg.dL.),
                          name = "BG.Reading..mg.dL.",
                          yaxis = yAxis)
    
    
  }#if addBG
  return(p)
}#end function