addPercentBG_ly<-function(data, p,addPercentBG,addPercentType){
  if (addPercentBG[1]!=""){
    
    if (addPercentType=="BG.Reading..mg.dL."){
     data<-data[!is.na(data$BG.Reading..mg.dL.),] 
     data$value<-data$BG.Reading..mg.dL.
     typeStr<-"BG"
    }else{
      data<-data[!is.na(data$Sensor.Glucose..mg.dL.),] 
      data$value<-data$Sensor.Glucose..mg.dL.
      typeStr<-"SG"
    }
    
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
     
      rangeStr<-gsub("and",paste0("<=",typeStr,"<="),rangeStr)
      rangeStr<-ifelse(regexpr(typeStr,rangeStr)>0,rangeStr,paste0(typeStr," ",rangeStr))
      rangeStr<-ifelse(regexpr(paste0("80<=",typeStr,"<=150"), rangeStr)>0,paste0("80<=",typeStr,"<150"),rangeStr)
      rangeStr<-ifelse(regexpr(paste0(typeStr," 80"),rangeStr)>0,paste0(typeStr,"<80"),rangeStr)
      rangeStr<-ifelse(regexpr(paste0(typeStr," 240"),rangeStr)>0,paste0(typeStr,">240"),rangeStr)
      rangeStr<-ifelse(regexpr(paste0(typeStr," 150"),rangeStr)>0,paste0(typeStr,">150"),rangeStr)

      
      subsetStr<-strsplit(subsetStr," and ")[[1]]
      subsetStr<-paste0("data$value",subsetStr, collapse = " & ")
      subsetStr<-paste0("which(",subsetStr,")")
      subsetValues<-eval(parse(text = subsetStr))
      
      percent<-round(length(subsetValues)/nrow(data)*100)
      
      rangeStr<-paste0(percent,"% ",rangeStr)
      percentStr<-c(percentStr,rangeStr)
      
      i<-which(addPercentBG==j)
      yPos<-c(yPos,450-25*i)
    }#for each addpercentBG
    
    #get positional data
    posData<-data.frame(x = rep(17,length(addPercentBG)), y = yPos, percentStr = percentStr)
    
    p <- p %>% add_text(data = posData, x = ~x, y = ~y, text = ~percentStr, 
                        textposition = "right",hoverinfo='skip')
  }#if addPercentBG
  return(p)
}#end func