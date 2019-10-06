historySeqOut<-function(data = NA,libraryPath,path,fileName,reportTitle, 
                        outPath = NA, outFileName= NA,
                        plotName = NA, paramList = NA, plotType = NA,
                        seqType = "change", seqLength = 2, period = NA) {
  
  
  replaceHistoryTitle(libraryPath, reportTitle)
  
  reportPath<-paste0(libraryPath,"reports/historySeqOut.Rmd")
if (is.na(outPath) | is.na(outFileName)){
  outFileName<-gsub("\\.Rmd","\\.html",reportPath)
}else{
  outFileName<-paste0(outPath,"/",outFileName,".html")
}
  
  rmarkdown::render(
    reportPath, params = list(
      libraryPath = libraryPath,
      path = path,
      fileName = fileName,
      plotName = plotName,
      paramList = paramList,
      plotType = plotType,
      seqType = seqType,
      seqLength = seqLength
    ),
    output_file = outFileName
  )
  
  shell.exec(outFileName)
}



