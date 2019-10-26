setupShinyParams<-function(input, output, session,
                           libraryPath, filePath,
                           data){
compiledInput<-compileInput(input, output, session)
unPackList(lists = list(compiledInput = compiledInput),
           parentObj = list(NA)) 

#format parameters
if (shPlotType=="scatter"){
  timeStep<-'hour'
  period<-1
}else if (shPlotType %in% c("bar","box")){
  ayCarb<-NA
  addBarSub<-FALSE
  boxBar<-shPlotType
  plotType<-"summaryPlot_ly"
  plotSummary<-plotSummary2
}else if (shPlotType=="heatmap"){
  brks<-paste0("c(",brks,")")
  brks<-eval(parse(text = brks))
  tcol <- "time2"
  dcol <- "Date2"
  includeTotals<-TRUE
  naRemove<-ignoreNAs
  plotType<-"heatMap_ly"
  valueVar<-plotSummary2
  timeStep<-"hour"
  period<-1
}
startDate<-as.character(daterange[1])
endDate<-as.character(daterange[2])

if (shPlotType!='heatmap'){
startTime<-ifelse(nchar(as.character(timeRange[1]))==1,paste0("0",timeRange[1],":00"),paste0(timeRange[1],":00"))
endTime<-ifelse(nchar(as.character(timeRange[2]))==1,paste0("0",timeRange[2],":00"),paste0(timeRange[2],":00"))
}

if (!exists("addBarSub")){
  addBarSub<-FALSE
}

if (!exists("addSetting")){
  addSetting<-""
}

if (shPlotType=="scatter"){
  plotType<-"plotLine_ly"
}
if (shPlotType!="Saved Plot"){
    allArgs<-findCodeStr(libraryPath,plotType,"args")$arguments

  #format plot params
  paramStr<-character(0)
  for (p in 2:length(allArgs)-1){
    pName<-allArgs[p]
    pStr<-paste0(pName,"=",pName,",")
    paramStr<-paste0(paramStr,pStr)
    
  }
  
  paramStr<-paste0(paramStr,allArgs[length(allArgs)],"=",allArgs[length(allArgs)])



#return workspace
workSpace.list<-named.list(paramStr,plotType)
}else{
  allArgs<-c("startDate","endDate","fromChange","numberDays","plotName")
  workSpace.list<-list() 
}
for (n in allArgs){
  eval(parse(text = paste0("workSpace.list$",n,"<-",n)))
}

#add history seq params
for (n in c("reportTitle","outPath","outFileName","seqType","seqLength")){
  eval(parse(text = paste0("workSpace.list$",n,"<-",n)))
}


  return(workSpace.list)
  
  }