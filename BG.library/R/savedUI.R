savedUI<-function(){
  ui=shinyUI(
    fluidPage(
      
      selectInput("plotName","Select saved plot by name",choices = names(plotList)),
      textOutput("description"),
      
      h3("Date and Time Parameters"),
      checkboxInput("fromChange","Use data from most recent pump settings change to present",value = TRUE),
      
      
      conditionalPanel(#only use numberDays and start End dates if not fromChange
        condition = "input.fromChange != 1",
        numericInput("numberDays","Number of Days from Latest Date",value = NA),
        dateRangeInput("daterange", "Date range",
                       format = "mm/dd/yy",
                       separator = " - ")
      ),
      
      #historytSeq
      h3("Historical Sequence Plot Parameters"),
      checkboxInput("outPutHistorySeq","Output a sequence of historical plots as html",value = TRUE),
      conditionalPanel(
        condition = "input.outPutHistorySeq==1",
        textInput("reportTitle","Title for output html document"),
        textInput("outPath","Path to directory in which to output html document"),
        h6("Blank will output file to ~BG.library/reports"),
        textInput("outFileName","Name of output file"),
        selectInput("seqType","Type of historical sequence to output",choices = c("change","days"),
                    selected = "change"),
        numericInput("seqLength", "Number of repetitions of historical sequence plots", value = 2)
      )
      
    ))
  return(ui)
  
}