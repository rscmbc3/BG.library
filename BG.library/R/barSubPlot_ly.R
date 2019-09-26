#'@title barSubPlot_ly
#'@description Add subplot of mean carb intake per hour as barplot  \\cr \\cr
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param p current plot_ly plot
#'@param barSubPlot TRUE/FALSE whether subplot should be added to main plot
#'@param ayCarb list of y axis specifications for carb barplot
#'@return `p` plot_ly interactive plot


barSubPlot_ly<-function(p, data, barSubPlot,ayCarb){
  if (barSubPlot){
    
    #format data
      data<-data[,names(data) %in% c("hours","BWZ.Carb.Input..grams.")]
      data$hours<-round(data$hours)
      data$BWZ.Carb.Input..grams.<-as.numeric(ifelse(data$BWZ.Carb.Input..grams.==0,NA,data$BWZ.Carb.Input..grams.))
      data<-as.data.frame(data %>% group_by(hours) %>% summarise_all(funs(mean),na.rm = TRUE))

      #create plot
      p <- p %>% add_bars(data = data, x = ~hours, y = ~BWZ.Carb.Input..grams., 
                          name = "mean BWZ.Carb.Input..grams.",
                          yaxis = "y6")

  }#if barsubplot
  return(p)
}