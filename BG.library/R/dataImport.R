#'@title dataImport
#'@description import csv file and create allData and joinData objects.  
#'allData is pump and sensor data rbinded
#'joinData is a roll join of pump and sensor data on datetime.\\cr \\cr
#'@param filePath character path to csv import file
#'@return `dataImport.list` list containing 2 data.frames (allData and joinData)
#'@examples
#'libraryPath<-"F:/BG.library_github/BG.library/"
#'filePath<-"F:/BG.library_github/exampleData.csv"
#'dataImport.list<-dataImport(filePath,libraryPath)

dataImport<-function(filePath,libraryPath){
  #readBG
  data<-read.csv(filePath, header = FALSE)
  
  #get pump table
  pumpStartRow<-which(data[,1]=="Index")[1]
  pumpEndRow<-which(data[,1]=="-------" & as.numeric(rownames(data))>pumpStartRow)-1
  pumpData<-data[pumpStartRow:pumpEndRow,]
  
  #get sensor table
  sensorStartRow<-which(data[,1]=="Index")[2]
  sensorEndRow<-nrow(data)
  sensorData<-data[sensorStartRow:sensorEndRow,]
 
  
  #fix tables
  pumpData<-fixtables(pumpData,libraryPath)
  sensorData<-fixtables(sensorData,libraryPath)

  
  #conform colClasses
  classTable<-cbind(names(pumpData), sapply(pumpData, class))
  for (c in 1:(nrow(classTable)-1)){
    cls<-as.character(classTable[[c,2]])
    #t<-"sensorData"
    col<-as.character(classTable[[c,1]])
    sensorData[[c]]<-convertClass(cls,col,sensorData,pumpData)
  }
  #print(cbind(classTable, sapply(sensorData, class))  )
  
  
  #find smallest difference in time and join
  sensorTable<-as.data.table(sensorData)
  pumpTable<-as.data.table(pumpData)
  sensorTable$dateTime2<-sensorTable$dateTime
  
  setkey(pumpTable, dateTime)
  setkey(sensorTable, dateTime)
  joinData<-sensorTable[pumpTable, roll = T]
  
  joinData<-as.data.frame(joinData)
  
  
  #combine columns
  for (i in 4:(length(sensorTable)-2)){
    tempSensor<-joinData[,i]
    tempPump<-joinData[,(length(sensorTable)+i)]
    tempSensor[is.na(tempSensor)] <- tempPump[is.na(tempSensor)]
    
    joinData[,i]<-tempSensor
  }
  
  #get rid of extra columns
  joinData<-joinData[,!startsWith(names(joinData),"i")]
  badNames<-c("Date","Time","New.Device.Time","Linked.BG.Meter.ID","Prime.Type","Prime.Volume.Delivered..U.","Rewind","Scroll.Step.Size","Network.Device.Associated.Reason","Network.Device.Disassociated.Reason","Network.Device.Disconnected.Reason","Preset.Temp.Basal.Name")
  joinData<-joinData[,!names(joinData) %in% badNames]
  
  
  #make extra time date columns in joinData for plotting
  joinData<-prettyTime(joinData,"dateTime2")
  
  #all data
  allData<-rbind(pumpData,sensorData)
  allData<-allData[order(allData$dateTime),]
  
  #make extra time date columns in allData for plotting
  allData<-prettyTime(allData,"dateTime")
  
  #remove dup BGs
  allData<-removeDupBGs(allData)
  joinData<-removeDupBGs(joinData)
  
  dataImport.list<-named.list(allData,joinData)
  return(dataImport.list)
}