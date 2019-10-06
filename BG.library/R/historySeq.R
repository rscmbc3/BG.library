historySeq<-function(data,libraryPath, plotName, parmList, plotType,
                     seqType, seqLength){

  
  if (seqType=="change"){
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  dateSeq<-.Date(0)
  dateSeq<-dateSeq[0]
  for (s in names(pumpSettings.list)){
    eval(parse(text = paste0("temp<-pumpSettings.list$",s)))
    dateStr<-gsub("X","",names(temp)[2:length(temp)])
  dateStr<-gsub("\\.","-",dateStr)
  dateStr<-as.Date(dateStr,format = "%m-%d-%Y",origin = "1970-01-01")
    dateSeq<-c(dateSeq,dateStr)
  }
  dateSeq<-unique(dateSeq)
  dateSeq<-dateSeq[order(dateSeq, decreasing = TRUE)]
  
  }#if seqType Change

  #trim dateSeq
  dateSeq<-dateSeq[1:(seqLength+1)]

  #plot each pair
    for (d in 1:(length(dateSeq)-1)){#for each pair
      endDate<-as.character(dateSeq[d])
    startDate<-as.character(dateSeq[d+1])
    changeParam.list<-list(startDate = startDate,
                           endDate = endDate)
    if (!is.na(plotName)){
         p<-executeSavedPlot(data, numberDays =NA, plotName,changeParam.list, libraryPath)
print(p)
    }else{#not saved plot
      
      #change parameters
      if (!is.na(changeParam.list)){
        for (c in names(changeParam.list)){
          eval(parse(text = paste0("paramList$",c,"<-changeParam.list$",c)))
        }
      }
      #unpack paramList
      unPackList(lists = list(paramList = paramList),
                 parentObj = list(NA))
      
      #format plot params
      paramStr<-character(0)
      for (p in 1:length(paramList)){
        pName<-names(paramList)[p]
        pStr<-paste0(pName,"=",pName,",")
        paramStr<-paste0(paramStr,pStr)
      }
      paramStr<-paste0(paramStr,"data = data,numberDays = NA")
      
      #execute plot
      execStr<-paste0(plotType,"(",paramStr,")")
      eval(parse(text = execStr))
    }
      
    }

}