goShinyPlot<-function(input, output, session,
                      libraryPath, path, fileName,
                      data){    
  
  shinySetup.list<-setupShinyParams(input, output, session,
                             libraryPath, path, fileName,
                             data)
    unPackList(lists = list(shinySetup.list = shinySetup.list),
               parentObj = list(NA)) 
  if (input$shPlotType!="Saved Plot"){

    
    #execute plot
    execStr<-paste0("suppressWarnings(",plotType,"(",paramStr,"))")    
    eval(parse(text = execStr))

  }else{

    changeParam.list = list(fromChange = fromChange,
                            startDate = startDate,
                            endDate = endDate)
    executeSavedPlot(data = data, plotName = plotName, libraryPath = libraryPath,numberDays = numberDays,
                     changeParam.list = changeParam.list)
  }
 
  
}