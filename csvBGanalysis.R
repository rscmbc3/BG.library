#setPaths
libraryPath<-"F:/BG.library_repos/BG.library/"
path<-"F:/BG.library_repos/"
fileName<-"CareLink-Export-1569716195944.csv"

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

#summarize data
BGvalue_Summary<-summarizeData(allData, colName = "BG.Reading..mg.dL.", numberDays = 7)
BGvalue_SummaryDaily<-summarizeData(allData, colName = "BG.Reading..mg.dL.", numberDays = 7, timeStep = "day")
BGpercent_Summary<-addPercentBG_ly(allData, p = NA,addPercentBG = c("very high","high","good","low"),
                                   addPercentType = "BG.Reading..mg.dL.",outputType = "table",
                                   numberDays = 5)
SGpercent_Summary<-addPercentBG_ly(allData, p = NA,addPercentBG = c("very high","high","good","low"),
                                   addPercentType = "Sensor.Glucose..mg.dL.",outputType = "table",
                                   numberDays = 5)
BGHigh_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", numberDays = 7,
                            sumFuncs = "length",
                            filterCond = "data[data$BG.Reading..mg.dL.>150 & !is.na(data$BG.Reading..mg.dL.),]")
BGveryHigh_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", numberDays = 7,
                                sumFuncs = "length",
                                filterCond = "data[data$BG.Reading..mg.dL.>300 & !is.na(data$BG.Reading..mg.dL.),]")

BGLow_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", numberDays = 7,
                           sumFuncs = "length",
                           filterCond = "data[data$BG.Reading..mg.dL.<80 & !is.na(data$BG.Reading..mg.dL.),]")
BGgood_Count<-summarizeData(allData, colName = "BG.Reading..mg.dL.", numberDays = 7,
                            sumFuncs = "length",
                            filterCond = "data[data$BG.Reading..mg.dL.>=80 & data$BG.Reading..mg.dL.<=150 & !is.na(data$BG.Reading..mg.dL.),]")
tempBasal_count<-summarizeData(allData, colName = "Temp.Basal.Amount", numberDays = 14,
                               sumFuncs = "length",
                               filterCond = "data[data$Temp.Basal.Amount==0 & !is.na(data$Temp.Basal.Amount),]")
suspendBasal_Count<-summarizeData(allData, colName = "Alarm", numberDays = 7,
                                  sumFuncs = "length",
                                  filterCond = "data[regexpr('SUSPEND',data$Alarm)>0 & !is.na(data$Alarm),]")


#timeDayTables used for heatmaps
BGvalue_timeDaytable<-timeDayTable(allData, tcol = "time2", dcol = "Date2", 
                                   valueVar = "BG.Reading..mg.dL.", 
                                   sumFunc = "mean", naRemove = TRUE,
                                   includeTotals = TRUE,
                                   numberDays = 7, filterCond = "")
SGvalue_timeDaytable<-timeDayTable(allData, tcol = "time2", dcol = "Date2", 
                                   valueVar = "Sensor.Glucose..mg.dL.", 
                                   sumFunc = "mean", naRemove = TRUE,
                                   includeTotals = TRUE,
                                   numberDays = 7, filterCond = "")
carbs_timeDaytable<-timeDayTable(allData, tcol = "time2", dcol = "Date2", 
                                 valueVar = "BWZ.Carb.Input..grams.", 
                                 sumFunc = "max", naRemove = TRUE,
                                 includeTotals = TRUE,replaceNAs = TRUE,
                                 numberDays = 7, filterCond = "")

#saved plots
#linePlots
executeSavedPlot(data = allData, numberDays = 5, plotName = "lineSumSens_SGper_Sett_BG", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "lineSumSens_BGper_Sett_BG", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "lineSumSens_BGper_subCarb_BG", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "lineSumSens_BGper_subCarb_Sett_BG", libraryPath = libraryPath)
#barplots hourly
executeSavedPlot(data = allData, numberDays = 5, plotName = "sumBar_highBG150_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "sumBar_lowBG80_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarInsulinHour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarBGallHour_Sett", libraryPath = libraryPath)
#every 3 hours barplots
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarBG3Hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarInsulin3Hour_Sett", libraryPath = libraryPath)
#daily barplots
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarInsulinDaily", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarBGDaily", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "stackBarSGDaily", libraryPath = libraryPath)
########boxplots hourly
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxSGhour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxBGhour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxCorrUhour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxFoodUhour_Sett", libraryPath = libraryPath)
#3hour boxplots
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxSG3hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxBG3hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxCorrU3hour_Sett", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxFoodU3hour_Sett", libraryPath = libraryPath)
##daily boxplots
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxSGdaily", libraryPath = libraryPath)
executeSavedPlot(data = allData, numberDays = 5, plotName = "boxBGdaily", libraryPath = libraryPath)

#heatmap
heatMap(BGvalue_timeDaytable, hasTotals = TRUE,
        margins = c(6,30), brks = seq(0,450,50), 
        brewerPallete = "RdBu")
heatMap(SGvalue_timeDaytable, hasTotals = TRUE,
        margins = c(6,30), brks = seq(0,450,50), 
        brewerPallete = "RdBu")
heatMap(carbs_timeDaytable, hasTotals = TRUE,
        margins = c(6,30), brks = seq(0,100,10), 
        brewerPallete = "RdBu",  textCol = "deeppink")

#dateSeq Reports
#saved plot
historySeqOut(data = NA,libraryPath, path, fileName,reportTitle = "Compare Summary Sensor Line Plot Since Last Pump Setting Change" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
                        seqType = "change", seqLength = 2) 
historySeqOut(data = NA,libraryPath, path, fileName,reportTitle = "Compare Summary Sensor Line Plot Since Last Pump Setting Change" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
              seqType = "change", seqLength = 2, outPath ="F:/",outFileName = "testOut") 
historySeqOut(data = NA,libraryPath, path, fileName,reportTitle = "Compare All Pump Setting Changes Summary Sensor Line Plot" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
              seqType = "change", seqLength = "all")
historySeqOut(data = allData,libraryPath, path, fileName,reportTitle = "Compare Summary Sensor Line Plot Weekly" ,
              plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
              seqType = "days", seqLength = 3, period = 7) 
#new plot parameters
parmList1<-list(scatterOnly = FALSE, pointSize = 10,
               startTime = "00:00", endTime = "23:00",
               addSensor = FALSE, addBG = TRUE,
               addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U."),
               addBarSub = FALSE,
               plotSummary = "Sensor.Glucose..mg.dL.")
historySeqOut(data = NA,libraryPath, path, fileName,reportTitle = "Bolus Type Points Summary Sensor Line Plot" ,
              plotName = NA, paramList = parmList1, plotType = "plotLine_ly",
              seqType = "change", seqLength = 2) 
#history sequence in Rstudio
historySeq(data = allData,libraryPath,  plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
           seqType = "change", seqLength = 2)



#line plot
plotLine_ly(allData,  scatterOnly = FALSE, pointSize = 10,
                      numberDays = 5, startDate = "2019-09-08", endDate = "2019-09-08",
                      startTime = "00:00", endTime = "23:00",
                      colorPalleteDaily = "rainbow", 
                      addSensor = FALSE, addBG = TRUE, settingOverlay = FALSE,
            #addBolusType = "Bolus.Volume.Delivered..U.",
            addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U."),
            #addBolusType = "",   
            #barSubPlot = TRUE,
            addBarSub = FALSE,
            plotSummary = "Sensor.Glucose..mg.dL.",
                      addSetting ="",filterCond = "",
                      legendInset = -0.2)
#barplots
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
                        addBarSub = FALSE,basal,
                        numberDays = 5, filterCond = "",
                        startDate = NA, endDate = NA,
                        startTime = "00:00", endTime = "23:00",
                        plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
                        addBG = FALSE, 
                        addSetting = "",settingOverlay = FALSE,percentSetting = 30,
                        legendInset = -0.2)
#simple with setting subplot
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
              addBarSub = FALSE,basal,
              numberDays = 5, filterCond = "",
              startDate = NA, endDate = NA,
              startTime = "00:00", endTime = "23:00",
              plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
              uniqueDT = TRUE,replaceNAs = FALSE,
              addBG = TRUE, 
              addSetting = c("basal","corrFactor","carbRatio"),settingOverlay = FALSE,percentSetting = 30,
              legendInset = -0.2)

#simple barplot period 3 hours
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
              addBarSub = FALSE,
              numberDays = 5, filterCond = "",
              startDate = NA, endDate = NA,
              startTime = "00:00", endTime = "23:00",period = 3,
              plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
              uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
              addBG = FALSE, 
              addSetting = c("basal","corrFactor","carbRatio"),settingOverlay = FALSE,percentSetting = 30,
              legendInset = -0.2)
#simple barplot timeStep day
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
           addBarSub = FALSE,
           numberDays = 5, filterCond = "",
           startDate = NA, endDate = NA,
           startTime = "00:00", endTime = "23:00",period = 1,timeStep = "day",
           plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "mean", stackedBar = "",
           uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
           addBG = FALSE, 
           addSetting = "",settingOverlay = FALSE,percentSetting = 30,
           legendInset = -0.2)
#boxplots
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,boxBar = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",
               plotSummary ="Sensor.Glucose..mg.dL.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = FALSE,ignoreNAs = TRUE,
               addSetting = c("basal","corrFactor","carbRatio"),settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,plotType = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",timeStep = "day",
               plotSummary ="BWZ.Correction.Estimate..U.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = TRUE,
               addSetting = "",settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)
summaryPlot_ly(p = NA, data = allData, barSubPlot = FALSE,ayCarb = NA,
               addBarSub = FALSE,plotType = "box",
               numberDays = 5, filterCond = "",
               startDate = NA, endDate = NA,
               startTime = "00:00", endTime = "23:00",timeStep = "day",
               plotSummary ="BWZ.Food.Estimate..U.", sumFunc = "", stackedBar = "",
               addBG = FALSE, uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = TRUE,
               addSetting = "",settingOverlay = FALSE,percentSetting = 30,
               legendInset = -0.2)




#plot settings
#all plots
#data to plot
data<-allData
#number of days to summarize
numberDays<-5
#custom filter function
filterCond<-""
#filterCond = "data[data$BG.Reading..mg.dL.>=150 | data$Sensor.Glucose..mg.dL.>=150,]"
#add BG meter value points
addBG<-FALSE
#column name to summarize
plotSummary <-"BG.Reading..mg.dL."
#pump settings to add (c("basal","corrFactor","carbRatio"))
addSetting <-c("basal","corrFactor","carbRatio")
#how far to drop the legend down from the plot
legendInset<--0.6
#margin values c(bottom, left, top, right)
#margins for heatmap c(bottom, left)
margins = c(10,4,3,15) 


#plotLine Settings
#scatterplot
scatterOnly<-FALSE
#add Bolus point values
addBolus<-TRUE
#type of bolus values to add (c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U."))
addBolusType = c("Bolus.Volume.Delivered..U.")
#add sensor data
addSensor<-FALSE
#color pallet for daily sensor data
colorPalleteDaily<-"rainbow"

#barplot settings
#summary function, aggregate on by hour
sumFunc<-"length"
#stacked bar ("BGrange" or "insulin")
stackedBar<-""
#substitute sensor BG for meter BG where not present, must use joinData as data
subSensorBG<-FALSE

#heatMap settings
#whether data table has totals included
hasTotals<-TRUE
#legend breakpoints
brks<-seq(0,450,50)
#heatmap color pallet
brewerPallete<-"RdBu"

#plot using hardCode settings above
plotLine(data, numberDays, scatterOnly,
         colorPalleteDaily, 
         addSensor, addBG, 
         addBolus, addBolusType,
         plotSummary,
         addSetting,filterCond,
         legendInset, margins)

barPlot(data,basal, corrFactor,carbRatio, numberDays, filterCond,
        plotSummary, sumFunc, stackedBar,
        addBG, addSetting,subSensorBG,
        legendInset, margins)

boxPlot(data,basal, corrFactor,carbRatio, numberDays, filterCond = "",
        plotSummary, 
        addBG, addSetting,subSensorBG,
        legendInset, margins)


#plots with in-func settings
#daily sensor values with BG
plotLine(allData, numberDays = 7, addSensor = TRUE,
         colorPalleteDaily = "rainbow",plotSummary ="", 
         addBolus = FALSE,addSetting = "",
         legendInset = -0.35, margins = c(10,4,3,15))
#summary BG values with settings
plotLine(allData, numberDays = 14, addSensor = FALSE,
         colorPalleteDaily = "rainbow", plotSummary = "BG.Reading..mg.dL.",
         addSetting = c("basal","corrFactor","carbRatio"), addBolus = FALSE,
         legendInset = -0.35, margins = c(10,4,3,15))
#summary sensor values with basal and bolus
plotLine(allData, numberDays = 7, addSensor = FALSE,
         colorPalleteDaily = "rainbow",plotSummary ="Sensor.Glucose..mg.dL.",
         addBolusType = c("Bolus.Volume.Delivered..U."),
         addSetting = c("basal"),
         legendInset = -0.35, margins = c(10,4,3,15))

#scatter basal and bolus and BG
plotLine(allData, numberDays = 5, scatterOnly = TRUE,addSensor = FALSE,
         plotSummary ="",
         addBolusType = c("BWZ.Correction.Estimate..U."),
         addSetting = c("basal"),
         legendInset = -0.35, margins = c(10,4,3,15))

#summary sensor values with BG and settings
plotLine(allData, numberDays = 7, addSensor = FALSE,
         colorPalleteDaily = "rainbow",plotSummary ="Sensor.Glucose..mg.dL.",
         addBolusType = "",addBolus = FALSE,
         addSetting = c("basal","corrFactor","carbRatio"),
         legendInset = -0.35, margins = c(10,4,3,15))



#bar plots

barPlot(allData, basal, corrFactor,carbRatio,
        numberDays = 5, plotSummary = "BG.Reading..mg.dL.", sumFunc = "mean", stackedBar = "",
        addBG = TRUE, addSetting = c("basal","corrFactor","carbRatio"),
        legendInset = -0.35, margins = c(10,4,2,15))

#stacked good range
barPlot(allData, basal, corrFactor,carbRatio, 
        numberDays = 30, plotSummary = "BG.Reading..mg.dL.", sumFunc = "mean", stackedBar = "BGrange",
        addBG = FALSE, addSetting = c("basal","corrFactor","carbRatio"),
        legendInset = -0.35, margins = c(10,4,2,15))
#stacked insulin
barPlot(allData, basal, corrFactor,carbRatio, 
        numberDays = 14, plotSummary = "BG.Reading..mg.dL.", sumFunc = "mean", stackedBar = "insulin",
        addBG = FALSE, addSetting = c("corrFactor","carbRatio"),
        legendInset = -0.35, margins = c(10,4,2,15))

#single bar plot filter
barPlot(allData, basal, corrFactor,carbRatio, 
        filterCond = "data[data$BG.Reading..mg.dL.>150 & !is.na(data$BG.Reading..mg.dL.),]",
        numberDays = 7, plotSummary = "BG.Reading..mg.dL.", sumFunc = "length", stackedBar = "",
        addBG = TRUE, addSetting = c("basal","corrFactor","carbRatio"),
        legendInset = -0.35, margins = c(10,4,2,15))
barPlot(allData, basal, corrFactor,carbRatio, 
        filterCond = "data[data$BG.Reading..mg.dL.<80 & !is.na(data$BG.Reading..mg.dL.),]",
        numberDays = 7, plotSummary = "BG.Reading..mg.dL.", sumFunc = "length", stackedBar = "",
        addBG = TRUE, addSetting = c("basal","corrFactor","carbRatio"),
        legendInset = -0.35, margins = c(10,4,2,15))

#boxplots
boxPlot(allData,basal, corrFactor,carbRatio, numberDays = 7, filterCond = "",
          plotSummary = "BWZ.Carb.Input..grams.", 
          addSetting = "",
          legendInset = -0.3, margins = c(10,4,2,15))
boxPlot(allData,basal, corrFactor,carbRatio, numberDays = 7, filterCond = "",
        plotSummary = "BG.Reading..mg.dL.", 
        addSetting ="basal",
        legendInset = -0.3, margins = c(10,4,2,15))

