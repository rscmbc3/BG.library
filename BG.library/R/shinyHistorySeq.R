shinyHistorySeq<-function(input, output, session,
                          libraryPath, path, fileName,
                          data){
  if (input$shPlotType!="Saved Plot"){
  shinySetup.list<-setupShinyParams(input, output, session,
                                    libraryPath, path, fileName,
                                    data)
  unPackList(lists = list(shinySetup.list = shinySetup.list),
             parentObj = list(NA)) 
  
  #build parmList
    execStr<-paste0("parmList<-list(",paramStr,")")
    eval(parse(text = execStr))
    
    #render RMD
    plotName<-NA
    paramList<-parmList
  
    paramList<-paramList[!names(paramList) %in% c("data","numberDays")]
  }else{
    compiledInput<-compileInput(input, output, session)
    unPackList(lists = list(compiledInput = compiledInput),
               parentObj = list(NA)) 
    
    plotName<- input$plotName
    paramList<-NA
    plotType<-NA

  }
    historySeqOut(data = NA,libraryPath,path,fileName,reportTitle, 
                           outPath, outFileName,
                            plotName, paramList = paramList, plotType,
                            seqType, seqLength, period = NA)
    

  
}