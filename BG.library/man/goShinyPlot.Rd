% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goShinyPlot.R
\name{goShinyPlot}
\alias{goShinyPlot}
\title{goShinyPlot}
\usage{
goShinyPlot(input, output, session, libraryPath, filePath, data)
}
\arguments{
\item{input}{shiny user inputs as list}

\item{output}{shiny output as list}

\item{session}{shiny session info}

\item{libraryPath}{character string path to BG.library code}

\item{filePath}{character path to csv import file}

\item{data}{data.frame to be used to generate plots}
}
\description{
Format shiny input parameters and generate interactive
plot in shiny application\cr \cr
}
\examples{
libraryPath<-"F:/BG.library_github/BG.library/"
filePath<-"F:/BG.library_github/exampleData.csv"
#load functions
dataImport.list<-dataImport(filePath,libraryPath)
data<-dataImport.list$allData
shinyApp(  ui=shinyUI(
fluidPage(
 titlePanel(
   h1("Rshiny Interactive BG Plots")),
 
 sidebarLayout(
   sidebarPanel(width=6,
                #top level user input
                selectInput("shPlotType","Select Plot Type",
                            choices = c("scatter","bar","box","heatmap","Saved Plot"),
                            selected = 'bar'),  
                uiOutput("outputUI")
   ),#end sidebar
   mainPanel(width = 6,
             plotlyOutput("plotOne", width=900,height=900)
   )#end main panel
 )#end sidebar layout
)#end fluid page
),#end shiny ui

server=shinyServer(function(input, output,session) {
 #render UIs
 observe({
   if (input$shPlotType=="scatter"){
     output$outputUI<-renderUI({
       scatterUI()
     })
   }else if (input$shPlotType=="box" | input$shPlotType=="bar"){
     output$outputUI<-renderUI({
       boxBarUI(data)
     })
   }else if (input$shPlotType=="heatmap"){
     output$outputUI<-renderUI({
       heatmapUI(data)
     })
   }else{#saved plot
     output$outputUI<-renderUI({
       savedUI()
     })
   }
 })
 
 #interactive plot
 observe({
   output$plotOne  <- renderPlotly({
     goShinyPlot(input, output, session,
                 libraryPath, filePath,
                 data)
   })#end renderplot
 }) 
})#end shiny server
)#end shiny app
}
