addStackbar_ly<-function(p,data,cls, timeStep){
  if (timeStep=="hour"){
    for (s in names(data)[2:length(data)]){
    data$temp<-eval(parse(text = paste0("data$`",s,"`")))
    p <- p %>% add_trace(data = data, x = ~hour, y = ~temp,type = "bar",
                         name = s, 
                         marker = list(color = cls[which(names(data)==s)-1],
                                       line = list(color = I("black"),
                                                   width = 1.5)))
    
    
  }#for each type of insulin 
  }else if (timeStep=="day"){
    for (s in names(data)[2:length(data)]){
      data$temp<-eval(parse(text = paste0("data$`",s,"`")))
      p <- p %>% add_trace(data = data, x = ~Date2, y = ~temp,type = "bar",
                           name = s, 
                           marker = list(color = cls[which(names(data)==s)-1],
                                         line = list(color = I("black"),
                                                     width = 1.5)))
      
      
    }#for each type of insulin 
  }
 
  return(p)
}