createSavedPlot<-function(libraryPath, plotName,plotType, 
                          description, paramList, default = TRUE){
  #file path to plotList
  plotListFile<-paste0(libraryPath,"/data/plotList")
  
  #load plotList
  load(file = plotListFile)
  
  #create new plot for list
  plotListSub<-list(plotType = plotType,
                    description = description,
                    paramList = paramList,
                    default = default)
  
  
  eval(parse(text = paste0(plotName,"<-plotListSub")))
  eval(parse(text = paste0("plotList$",plotName,"<-",plotName)))
  
  #save updated plotList
  save(file = plotListFile,plotList) 
}

