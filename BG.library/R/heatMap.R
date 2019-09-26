heatMap<-function(data, hasTotals = TRUE,
                  margins = c(6,15), brks = seq(0,450,50), 
                  brewerPallete = "RdBu", revPallete = TRUE,
                  textCol = "black"){
  
  #format data
  rownames(data)<-data$time
  if (hasTotals){
      data<-data[-c(nrow(data)),-c(1,length(data))]
  }
  
  data<-as.matrix(data, ncol = length(data))
  data<-t(apply(data,1,function(x) ifelse(is.nan(x),NA,as.numeric(x))))


  #set breaks and make legend character
  myBreaks<-brks
for (b in 1:(length(myBreaks)-1)){
  br<-paste0(myBreaks[b], " to ",myBreaks[b+1])
  if (b==1){
    legendChar<-br
  }else {
    legendChar<-c(legendChar,br)
  }
}
  legendChar<-c(legendChar,paste0(">",myBreaks[length(myBreaks)]))
 
  #set color palette
   myCol<-brewer.pal(length(myBreaks)-1,"RdBu")
   myColLegend<-brewer.pal(length(myBreaks),"RdBu")
   if (revPallete){
     myCol<-rev(myCol)
     myColLegend<-rev(myColLegend)
   }
  
   #heatmap
  hm <- heatmap.2(data, scale="none", Rowv=NA, Colv=NA,
                  col = myCol, ## using your colors
                  breaks = myBreaks, ## using your breaks
                  dendrogram = "none",  ## to suppress warnings
                  margins=margins, 
                  cexRow=1, cexCol=1.0,
                  #key=FALSE,
                  trace="none", cellnote = round(data),notecol=textCol,na.color=par("bg"),
                  sepwidth=c(0.005,0.001),
                  sepcolor="black",
                  colsep=1:ncol(data),
                  rowsep=1:nrow(data))
  legend("right", fill = myColLegend,
         legend = legendChar,cex = 1.5)
  
}#endFunc

