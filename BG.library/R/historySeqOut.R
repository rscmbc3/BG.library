historySeqOut<-function(data = NA,libraryPath,path,fileName,reportTitle, plotName = NA, paramList = NA, plotType = NA,
                        seqType = "change", seqLength = 2, period = NA) {
  
  generatehistorySeqRMD(data,libraryPath,reportTitle, plotName, paramList, plotType,
                                  seqType, seqLength, period)
  
  reportPath<-paste0(libraryPath,"reports/historySeqOut.Rmd")

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
    output_file = gsub("\\.Rmd","\\.html",reportPath)
  )
  
  shell.exec(gsub("\\.Rmd","\\.html",reportPath))
}



