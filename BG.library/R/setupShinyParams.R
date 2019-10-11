setupShinyParams<-function(input, output, session,
                           libraryPath, path, fileName,
                           data){
compiledInput<-compileInput(input, output, session)
unPackList(lists = list(compiledInput = compiledInput),
           parentObj = list(NA)) 

#format parameters
if (shPlotType=="scatter"){
  timeStep<-'hour'
  period<-1
}
startDate<-as.character(daterange[1])
endDate<-as.character(daterange[2])
xticks<-seq.POSIXt(as.POSIXct("00:00",format="%H:%M"),as.POSIXct("23:00",format="%H:%M"),by = paste0(period," hour"))
xticks<-format(xticks,"%H:%M")
startTime<-xticks[which(seq(0,23,1)==timeRange[1])]
endTime<-xticks[which(seq(0,23,1)==timeRange[2])]
if (!exists("addBarSub")){
  addBarSub<-FALSE
}
if (!exists("barSubPlot")){
  barSubPlot<-FALSE
}
if (!exists("addSetting")){
  addSetting<-""
}

if (shPlotType=="scatter"){
  plotType<-"plotLine_ly"
  barSubPlot<-TRUE
}
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
for (n in allArgs){
  eval(parse(text = paste0("workSpace.list$",n,"<-",n)))
}

#add history seq params
for (n in c("reportTitle","outPath","outFileName","seqType","seqLength")){
  eval(parse(text = paste0("workSpace.list$",n,"<-",n)))
}


  return(workSpace.list)
  
  }