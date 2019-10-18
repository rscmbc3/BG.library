shinyReport<-function(input, output, session,
                          libraryPath, path, fileName,
                          data, reportType){
  shinySetup.list<-setupShinyParams(input, output, session,
                                    libraryPath, path, fileName,
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
  historySeqOut(data = NA,libraryPath,path,fileName,reportTitle, 
                outPath, outFileName,
                plotName, paramList = paramList, plotType,
                seqType, seqLength, period = period)
  
  }else{#BG report
    generateBGreport(libraryPath, path, fileName, data = data,numberDays = numberDays,
                     fromChange = fromChange,startDate = startDate, endDate=endDate)
    }
  
  
  
}