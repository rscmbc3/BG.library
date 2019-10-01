uniqueDateTime<-function(data, NAMES, replaceNAs, sumFunc = "mean",
                         timeStep, period){
  data<-data[,NAMES]
  
  if (replaceNAs){
      #replace NAs with zeros
  for (c in which(names(data)!="Date2")){
    data[,c]<-sapply(data[,c], function(x) as.numeric(ifelse(is.na(x),0,x)))
  }
  }

  dataReplaced<-data
  #set up summary function string
  if (sumFunc!="length"){
    #sumString<-paste0("as.data.frame(data %>% group_by(Date2,hours) %>% summarise_all(funs(",
    #                  sumFunc,"),na.rm = TRUE))")
    sumString<-paste0("as.data.frame(data[,!names(data) %in% c('Date2','hours')] %>% group_by(dateTime) %>% summarise_all(funs(",
                      sumFunc,"),na.rm = TRUE))")
  }else if (sumFunc=="length"){
    #sumString<-paste0("as.data.frame(data %>% group_by(Date2,hours) %>% summarise_all(funs(",
    #                  sumFunc,")))")
    sumString<-paste0("as.data.frame(data[,!names(data) %in% c('Date2','hours')] %>% group_by(dateTime) %>% summarise_all(funs(",
                      sumFunc,")))")
    
  }#end sumString
  
  #apply sumString
  data<-eval(parse(text = sumString))
  
  return(data)
}