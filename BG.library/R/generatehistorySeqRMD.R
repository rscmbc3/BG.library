generatehistorySeqRMD<-function(data,libraryPath, reportTitle, plotName, paramList, plotType,
                     seqType, seqLength, period){
  
reportPath<-paste0(libraryPath,"reports/historySeqOut.Rmd")

headStr<-paste0("---
title: '",reportTitle,"'
author: 'Lillian Gorman Sanisaca'
date: ",'\"',"`r format(Sys.time(), '%m-%d-%Y')`",'\"\n',
                "output: html_document
params:
  libraryPath: libraryPath
  path: path
  fileName: fileName
  plotName: plotName
  paramList: parmList
  plotType: plotType
  seqType: seqType
  seqLength: seqLength
---\n \n")

setStr<-"```{r setup, include=FALSE}\n
knitr::opts_chunk$set(echo = FALSE,fig.width=12, fig.height=8, warning = FALSE, message = FALSE)\n
```\n \n"
allStr<-"```{r, eval=TRUE, results='asis'}
#setPaths
libraryPath<-params$libraryPath
path<-params$path
fileName<-params$fileName
plotName<-params$plotName
parmList<-params$parmList
plotType<-params$plotType
seqType<-params$seqType
seqLength<-params$seqLength


#load functions
devtools::load_all(libraryPath,recompile = FALSE) 

#get pumpSettings
pumpSettings.list<-makePumpSettings(libraryPath)
unPackList(lists = list(pumpSettings.list = pumpSettings.list),
           parentObj = list(NA)) 

#metronic csv data import
dataImport.list<-dataImport(path,fileName)
unPackList(lists = list(dataImport.list = dataImport.list),
           parentObj = list(NA)) 
data<-allData

  if (seqType=='change'){
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  dateSeq<-.Date(0)
  dateSeq<-dateSeq[0]
  for (s in names(pumpSettings.list)){
    eval(parse(text = paste0('temp<-pumpSettings.list$',s)))
    dateStr<-gsub('X','',names(temp)[2:length(temp)])
  dateStr<-gsub('\\\\.','-',dateStr)
  dateStr<-as.Date(dateStr,format = '%m-%d-%Y',origin = '1970-01-01')
    dateSeq<-c(dateSeq,dateStr)
  }
  dateSeq<-unique(dateSeq)
  dateSeq<-dateSeq[order(dateSeq, decreasing = TRUE)]
  
  }else{#if seqType = days

dateSeq<-seq.Date(from = max(data$Date2),to = min(data$Date2),by=paste0('-',period,' day'))

}

  #trim dateSeq
if (seqLength!='all' & seqType=='change'){
  if (length(dateSeq)>seqLength+1){

    dateSeq<-dateSeq[1:(seqLength+1)]
  }
}else if (seqType!='change' & seqLength!='all'){
          if (length(dateSeq)<seqLength+1){
    dateSeq<-c(dateSeq,min(data$Date2))
          }
    }
 

  #plot each pair"
if (seqType=='change'){
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  dateSeq<-.Date(0)
  dateSeq<-dateSeq[0]
  for (s in names(pumpSettings.list)){
    eval(parse(text = paste0('temp<-pumpSettings.list$',s)))
    dateStr<-gsub('X','',names(temp)[2:length(temp)])
    dateStr<-gsub('\\.','-',dateStr)
    dateStr<-as.Date(dateStr,format = '%m-%d-%Y',origin = '1970-01-01')
    dateSeq<-c(dateSeq,dateStr)
  }
  dateSeq<-unique(dateSeq)
  dateSeq<-dateSeq[order(dateSeq, decreasing = TRUE)]
  
}else{#if seqType = days
  dateSeq<-seq.Date(from = max(data$Date2),to = min(data$Date2),by=paste0('-',period,' day'))


}

#trim dateSeq
if (seqLength!='all' & seqType=='change'){
  if (length(dateSeq)>seqLength+1){

    dateSeq<-dateSeq[1:(seqLength+1)]
  }
}else if (seqType!='change' & seqLength!='all'){
          if (length(dateSeq)<seqLength+1){
    dateSeq<-c(dateSeq,min(data$Date2))
          }
    }

    for (d in 1:(length(dateSeq)-1)){#for each pair
      endDate<-as.character(dateSeq[d])
    startDate<-as.character(dateSeq[d+1])
    changeParam.list<-list(startDate = startDate,
                           endDate = endDate)
    changeParam.listStr<-paste0("changeParam.list<-",paste(capture.output(dput(changeParam.list)),collapse = ""))
    if (!is.na(plotName)){
    plotStr<-"executeSavedPlot(data, numberDays =NA, plotName,changeParam.list, libraryPath)"
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
      allValuesStr<-character(0)
      for (p in 1:length(paramList)){
        pName<-names(paramList)[p]
        pStr<-paste0(pName,"=",pName,",")
        paramStr<-paste0(paramStr,pStr)
        pValueStr<-paste0(pName,"<-",paste(capture.output(dput(paramList[[p]])),collapse = ""))
        allValuesStr<-paste(allValuesStr,pValueStr, sep="\n")
      }
      paramStr<-paste0(paramStr,"data = data,numberDays = NA")
      
      #get plotstr
   # startDateStr<-paste0("startDate<-",paste(capture.output(dput(startDate)),collapse = ""))
  #  endDateStr<-paste0("endDate<-",paste(capture.output(dput(endDate)),collapse = ""))
      plotStr<-paste0(plotType,"(",paramStr,")")
      plotStr<-paste(allValuesStr,plotStr,sep="\n")
      }

    totalStr<-paste(changeParam.listStr,plotStr,sep="\n")
    allStr<-paste(allStr,totalStr, sep = "\n \n")
    }

allStr<-paste(allStr,"```",sep = "\n")
sink(file = reportPath)
cat(headStr, setStr, allStr,sep="")
sink()
}#end function