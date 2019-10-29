shinyReport<-function(input, output, session,
                          libraryPath, filePath,
                          data, reportType){
  shinySetup.list<-setupShinyParams(input, output, session,
                                    libraryPath, filePath,
                                    data)
  unPackList(lists = list(shinySetup.list = shinySetup.list),
             parentObj = list(NA)) 
  if (input$shPlotType!="Saved Plot"){
    
    
    #build parmList
    execStr<-paste0("parmList<-list(",paramStr,")")
    eval(parse(text = execStr))
    
    #render RMD
    plotName<-NA
    paramList<-parmList
    
    paramList<-paramList[!names(paramList) %in% c("data","numberDays")]
  }else{
    
    plotName<- input$plotName
    paramList<-NA
    plotType<-NA
    
  }
  
  if (reportType=="history"){
    period<-input$periodHist
  historySeqOut(data = NA,plotName, paramList = paramList, plotType,
                seqType, seqLength, period = period,
                reportTitle,outPath, outFileName,
                libraryPath,filePath, removeDates = removeDates)
  
  }else{#BG report
    generateBGreport(libraryPath, filePath, data = data,numberDays = numberDays,
                     fromChange = fromChange,startDate = startDate, 
                     endDate=endDate,removeDates = removeDates)
    }
  
  
  
}