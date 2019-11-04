#3hour boxplots

heatMap_ly(brks = c(0,50,80,150,240,300,400,500), 
           brewerPallete = "RdBu", revPallete = TRUE,
           textCol = "black",
           #timeDayTable args
           data = allData, tcol = "time2", dcol = "Date2", 
           valueVar = "BG.Reading..mg.dL.", 
           sumFunc = "mean", naRemove = TRUE,
           includeTotals = TRUE,
           filterCond = "",
           libraryPath = libraryPath)
plotName<-"meanSGheat_hist"
plotType<-"heatMap_ly"
description<-"Heat map of mean hourly SG values per day with histogram of groups."
paramList<-list(brks = c(0,50,80,150,240,300,400,500),
                brewerPallete = "RdBu",
                revPallete = TRUE,
                textCol = "black",
                tcol = "time2",
                dcol = "Date2",
                valueVar = "Sensor.Glucose..mg.dL.",
                sumFunc = "mean",
                naRemove = TRUE,
                includeTotals = TRUE,
                filterCond = "")

createSavedPlot(libraryPath, plotName,plotType,description, paramList)

for (n in names(plotList)){
 #evalStr<-paste0("plotList$",n,"$paramList$corrFactor<-NA")
  evalStr<-paste0("plotList$",n,"$paramList<-plotList$",n,"$paramList[names(plotList$",n,"$paramList)!='barSubPlot']")
  eval(parse(text = evalStr))
}

plotList$meanBGheat_hist$paramList<-plotList$meanBGheat_hist$paramList[names(plotList$meanBGheat_hist$paramList)!="includeTotals"]

plotList$boxSGdaily$paramList$addGoodRange<-FALSE