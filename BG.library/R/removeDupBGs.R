removeDupBGs<-function(data){
  #get bg data
  bgData<-data[!is.na(data$BG.Reading..mg.dL.),]
  #remove bg data from data
  data<-data[is.na(data$BG.Reading..mg.dL.),]
 
  #flag duplicates <= 10 minutes
bgData$dup<-rep(0,nrow(bgData))
  for (b in 2:nrow(bgData)){
    if (abs(difftime(bgData$dateTime[(b-1)],bgData$dateTime[b], units = "mins"))<=10 |
        bgData$BG.Reading..mg.dL.[(b-1)]==bgData$BG.Reading..mg.dL.[b]){
      bgData$dup[b]<-1
    }
  }

#remove duplicates
bgData<-bgData[bgData$dup==0,]  
bgData<-bgData[,names(bgData)!="dup"]

#bind with non-BG data
  data<-rbind(data,bgData)
  data<-data[order(data$dateTime),]
return(data)
}
