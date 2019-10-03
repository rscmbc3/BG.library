breakStr<-function(longStr,nBreak,strBreak){ 
  if (nchar(longStr)>nBreak){
    spaces<-lapply(strsplit(longStr, ''), function(longStr) which(longStr == ' '))[[1]]
    seqSplit<-seq(1,max(spaces),nBreak)
    start<-2
    if (max(seqSplit)!=1){
      start<-2
      newstr<-character(0)
      for (s in seqSplit[2:length(seqSplit)]){
        splitSpace<-max(spaces[which(spaces<s)])
        newstr<-c(newstr,substr(longStr,start,splitSpace))
        start<-splitSpace+1
      }
      newstr[1]<-paste0(substr(longStr,1,1),newstr[1])
      newstr<-paste(c(newstr,substr(longStr,start,nchar(longStr))),collapse = strBreak)
      
    }else{
      newstr<-c(substr(longStr,1,max(spaces)),substr(longStr,max(spaces)+1,nchar(longStr)))
      newstr<-paste0(newstr, collapse = strBreak)
    }
  }else{
    newstr<-longStr
  }
  return(newstr)
}