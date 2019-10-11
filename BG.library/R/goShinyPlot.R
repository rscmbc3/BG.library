goShinyPlot<-function(input, output, session,
                      libraryPath, path, fileName,
                      data){
  
  shinySetup.list<-setupShinyParams(input, output, session,
                             libraryPath, path, fileName,
                             data)
    unPackList(lists = list(shinySetup.list = shinySetup.list),
               parentObj = list(NA)) 
    
    #execute plot
    execStr<-paste0("suppressWarnings(",plotType,"(",paramStr,"))")
    
    eval(parse(text = execStr))
 
  
}