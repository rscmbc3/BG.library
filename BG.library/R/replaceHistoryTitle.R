replaceHistoryTitle<-function(libraryPath, reportTitle){
  
  historyRMDpath<-paste0(libraryPath,"reports/historySeqOut.Rmd")
  #read Rmd file as text
  x <- readLines(historyRMDpath)
  #find where title is designated
  editthis<-x[which(regexpr("title:",gsub(" ","",x))>0)]
  #replace with current path_master
  y <- gsub( editthis, paste0("title: '",reportTitle,"'"), x )
  #overwrite the file
  cat(y, file=historyRMDpath, sep="\n") 
}
