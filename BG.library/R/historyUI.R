historyUI<-function(){
  ui=shinyUI(
    fluidPage(
      
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
        numericInput("seqLength", "Number of repetitions of historical sequence plots", value = 2),
        numericInput("periodHist","Number of Days for each historical sequence plot",value = 7)
      )
      
    ))
  return(ui)
  
}