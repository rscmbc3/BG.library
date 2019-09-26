makeXaxis<-function(addSetting, settingOverlay,xDomain){
#if (addSetting[1]=="" | settingOverlay){ 
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
#}else if (addSetting[1]!="" & !settingOverlay){
 # xAxis<- paste("xaxis  = list(showtickLabels = FALSE,
#showline = FALSE,"
#                ,xDomain,"##

#showTitle = FALSE),#
#
#                xaxis2 = list(tickmode = 'array',
#               tickvals = seq(startTime,endTime,1),
#               ticks = 'outside',
##               ticktext = xticks,
#               tick0 = xticks[1],",
#                xDomain,"      
#               #domain = c(0,0.80),
#               zeroline = FALSE,
#               showline = TRUE,
#               tickangle = 60,
#side = 'bottom'
#               
#  )")
#  print(xAxis)
  
#}
  return(xAxis)

}