#3hour boxplots

summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,plotType = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",timeStep = "day",
               plotSummary ="BG.Reading..mg.dL.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
               addSetting = "",settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)
plotName<-"boxBGdaily"
plotType<-"summaryPlot_ly"
description<-"Box plot of daily BG values."
paramList<-list(startDate = "", 
                endDate = "",
                plotSummary = "BG.Reading..mg.dL.",
                addSetting ="",
                addBarSub = FALSE,
                p = NA,
                barSubPlot = FALSE,
                ayCarb = NA,
                sumFunc = "", 
                stackedBar = "",
                addBG = FALSE, 
                uniqueDT = TRUE,
                replaceNAs = FALSE,
                ignoreNAs = TRUE,
                boxBar = "box",
                timeStep = "day")

createSavedPlot(libraryPath, plotName,plotType,description, paramList)

for (n in names(plotList)){
 #evalStr<-paste0("plotList$",n,"$paramList$corrFactor<-NA")
  evalStr<-paste0("plotList$",n,"$paramList<-plotList$",n,"$paramList[names(plotList$",n,"$paramList)!='carbRatio']")
  eval(parse(text = evalStr))
}