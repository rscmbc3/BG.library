#'@title summaryPlot_ly
#'@description generate bar and box summary plots and carb subbarplot  \\cr \\cr
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param p current plot_ly plot
#'@param barSubPlot TRUE/FALSE whether subplot should be added to main plot
#'@param ayCarb list of y axis specifications for carb barplot
#'@return `p` plot_ly interactive plot


summaryPlot_ly<-function(p, data, barSubPlot = FALSE,ayCarb,
                     addBarSub,
                     boxBar = "bar",
                     numberDays, filterCond = "",
                     startDate = NA, endDate = NA,
                     startTime = "00:00", endTime = "23:00",
                     timeStep = "hour",period = 1,
                     plotSummary, sumFunc = "length", stackedBar = "",
                     uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = FALSE,
                     addBG = TRUE, pointSize = 10,
                     addSetting = "",settingOverlay = FALSE,percentSetting = 30,
                     legendInset = -0.2){
  #get basal
  basal<-makePumpSettings(libraryPath)$basal
  
  if (barSubPlot){#barplot is not main plot and add subplot
    
    p <- barSubPlot_ly(data, p, addBarSub,timeStep, period)
    return(p)
    
  }else if (timeStep=="hour"){#barplot is main plot timestep hour
    p <- summaryPlotHour_ly(data,basal,barSubPlot,boxBar,
                        numberDays, filterCond,
                        startDate, endDate,
                        startTime, endTime,
                        timeStep,period,
                        plotSummary, sumFunc, stackedBar,
                        uniqueDT,replaceNAs,ignoreNAs,
                        addBG, pointSize,
                        addSetting,settingOverlay,percentSetting,
                        legendInset)
      p
  }else if (timeStep=="day"){#barplot is main plot timestep day
    p <- summaryPlotDay_ly(data,basal,barSubPlot,boxBar,
                            numberDays, filterCond,
                            startDate, endDate,
                            startTime, endTime,
                            timeStep,period,
                            plotSummary, sumFunc, stackedBar,
                            uniqueDT,replaceNAs,ignoreNAs,
                            legendInset)
      p
    }
}#end function