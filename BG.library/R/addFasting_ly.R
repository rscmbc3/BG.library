addFasting_ly<-function(p, data, addFasting, addFastingAnnot){
  data<-dataFasting
  
  if(addFasting){
    #subset out before 5am values
    data<-data[!is.na(data$BG.Reading..mg.dL.),]
    data<-data[which(data$hours>=5 & data$hours<=10),]
    
    #get fasting times
    fastingTimes<-as.data.frame(data %>% group_by(Date2) %>% summarize(hours = min(hours,na.rm = TRUE)))
    
    #merge with data
    data<-merge(data,fastingTimes, by= c("Date2","hours"))
    
    #calculate mean fasting value and replicate for length unique(hours)
    meanFasting<-data.frame(hours = unique(data$hours))
    meanFasting$value<-rep(mean(data$BG.Reading..mg.dL.),nrow(meanFasting))
    
    #add trace
    lineText<-paste0("Mean Fasting BG = ",round(unique(meanFasting$value)))
    p <- p %>% add_trace(data = meanFasting, x = ~hours, y = ~value,
                         type = "scatter", 
                         mode = "lines",
                         color = I("magenta"),
                         hoverinfo = 'text',
                         text = lineText,
                         name = "Mean Fasting BG")
    if(addFastingAnnot){
    #add_annotation
    p <- p %>% add_annotations(x = max(meanFasting$hours),
                    y = unique(meanFasting$value),
                    text = lineText,
                    xref = "x",
                    yref = "y",
                    showarrow = TRUE,
                    arrowhead = 2,
                    arrowsize = 1,
                    ax = 20,
                    ay = -40)
    }
    
  }#if addFasting
  return(p)
  
}