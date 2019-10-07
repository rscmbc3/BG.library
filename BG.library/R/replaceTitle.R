replaceTitle<-function(libraryPath, reportTitle, reportName){
  
  RMDpath<-paste0(libraryPath,"reports/",reportName,".Rmd")
  #read Rmd file as text
  x <- readLines(RMDpath)
  #find where title is designated
  editthis<-x[which(regexpr("title:",gsub(" ","",x))>0)]
  #replace with current path_master
  y <- gsub( editthis, paste0("title: '",reportTitle,"'"), x )
  #overwrite the file
  cat(y, file=RMDpath, sep="\n") 
}
