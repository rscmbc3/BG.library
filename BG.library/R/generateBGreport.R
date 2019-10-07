generateBGreport<-function(libraryPath, path, fileName,
                           outPath = NA, outFileName= NA,
                             numberDays = NA, fromChange = TRUE,
                           data){
  #get dateRange
  data<-fromChangeDateRange(data,numberDays,fromChange,libraryPath = libraryPath)
  
  reportTitle<-paste0("BG_report for Dates: ",min(data$Date2)," to ",max(data$Date2))
  
  replaceTitle(libraryPath, reportTitle,reportName = "BG_report")
  
  reportPath<-paste0(libraryPath,"reports/BG_report.Rmd")
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
      numberDays = numberDays,
      fromChange = fromChange
    ),
    output_file = outFileName
  )
  
  shell.exec(outFileName)
  
  }