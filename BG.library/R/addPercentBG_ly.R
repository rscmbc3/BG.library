addPercentBG_ly<-function(data, p,addPercentBG){
  if (addPercentBG[1]!=""){
    data<-data[!is.na(data$BG.Reading..mg.dL.),]
    yPos<-numeric(0)
    percentStr<-character(0)
    for (j in addPercentBG) {
      if (length(grep("very high",addPercentBG))!=0 | length(grep("high",addPercentBG))==0){
        subsetStr<-ifelse(j=="low","<80",
                        ifelse(j=="good",">=80 and <=150",
                               ifelse(j=="high",">150 and <=240",
                                      ifelse(j=="very high",">240",""))))
      }else if (length(grep("high",addPercentBG))!=0){
        subsetStr<-ifelse(j=="low","<80",
                          ifelse(j=="good",">=80 and <=150",
                                 ifelse(j=="high",">150")))
      }
      
      
      rangeStr<-gsub(">","",subsetStr)
      rangeStr<-gsub("<","",rangeStr)
      rangeStr<-gsub("=","",rangeStr)
      rangeStr<-gsub("and","<=BG<=",rangeStr)
      rangeStr<-ifelse(regexpr("BG",rangeStr)>0,rangeStr,paste0("BG ",rangeStr))
      rangeStr<-ifelse(regexpr("80<=BG<=150", rangeStr)>0,"80<=BG<150",rangeStr)
      rangeStr<-ifelse(regexpr("BG 80",rangeStr)>0,"BG<80",rangeStr)
      rangeStr<-ifelse(regexpr("BG 240",rangeStr)>0,"BG>240",rangeStr)
      rangeStr<-ifelse(regexpr("BG 150",rangeStr)>0,"BG>150",rangeStr)
      
      
      subsetStr<-strsplit(subsetStr," and ")[[1]]
      subsetStr<-paste0("data$BG.Reading..mg.dL.",subsetStr, collapse = " & ")
      subsetStr<-paste0("which(",subsetStr,")")
      subsetValues<-eval(parse(text = subsetStr))
      
      percent<-round(length(subsetValues)/nrow(data)*100)
      
      rangeStr<-paste0(percent,"% ",rangeStr)
      percentStr<-c(percentStr,rangeStr)
      
      i<-which(addPercentBG==j)
      yPos<-c(yPos,450-25*i)
    }#for each addpercentBG
    
    #get positional data
    posData<-data.frame(x = rep(17.5,length(addPercentBG)), y = yPos, percentStr = percentStr)
    
    p <- p %>% add_text(data = posData, x = ~x, y = ~y, text = ~percentStr, 
                        textposition = "right",hoverinfo='skip')
  }#if addPercentBG
  return(p)
}#end func