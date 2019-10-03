executeSavedPlot<-function(data, numberDays, plotName, libraryPath){
  #load saved plotList
  load(paste0(libraryPath,"/data/plotList"))
  
  #make plotName top level in plotList
  eval(parse(text = paste0("plotList<-plotList$",plotName)))
  unPackList(lists = list(plotList = plotList),
             parentObj = list(NA))
  #unpack params
  unPackList(lists = list(paramList = paramList),
             parentObj = list(NA))
  
  #format plot params
  paramStr<-character(0)
for (p in 1:length(paramList)){
  pName<-names(paramList)[p]
  pStr<-paste0(pName,"=",pName,",")
  paramStr<-paste0(paramStr,pStr)
}
  paramStr<-paste0(paramStr,"data = data,numberDays = numberDays")
  
  #execute plot
  execStr<-paste0(plotType,"(",paramStr,")")
  eval(parse(text = execStr))

}