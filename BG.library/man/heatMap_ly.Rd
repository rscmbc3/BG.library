% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatMap_ly.R
\name{heatMap_ly}
\alias{heatMap_ly}
\title{heatMap_ly}
\usage{
heatMap_ly(data, tcol = "time2", dcol = "Date2", removeDates = NA,
  valueVar, sumFunc, naRemove = TRUE, replaceNAs = FALSE,
  fromChange = TRUE, numberDays = NA, startDate = NA, endDate = NA,
  filterCond = "", timeStep = "hour", period = 1, brks = seq(0,
  450, 50), brewerPallete = "RdBu", revPallete = TRUE,
  textCol = "black", addHist = TRUE, libraryPath)
}
\arguments{
\item{data}{data.frame with data to plot}

\item{tcol}{character string indicating 'time' column name}

\item{dcol}{character string indicating 'date' column name}

\item{removeDates}{character vector of dates in format %Y-%m-%d to remove from data}

\item{sumFunc}{character string indicating function to aggregate data for plot}

\item{naRemove}{TRUE/FALSE whether missing values should be removed}

\item{replaceNAs}{TRUE/FALSE whether missing values should be replaced with zeros}

\item{valuevar}{character string indicating column name to plot}

\item{addBolusType}{character string vector of Bolus columns to add as scatter points
addBolusType = c("Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")}

\item{pointSize}{scatter point size}
}
\value{
\code{p} plot_ly interactive plot with BG values added if addBG
}
\description{
Creates interactive heatmap with custom breakpoints and colors.  Optional
\code{addHist} parameter allows for histogram subplot by color group. \cr \cr
}
\examples{
libraryPath<-"F:/BG.library_github/BG.library/"
filePath<-"F:/BG.library_github/exampleData.csv"
#load functions
devtools::load_all(libraryPath,recompile = FALSE) 
dataImport.list<-dataImport(filePath,libraryPath)
data<-dataImport.list$allData
heatMap_ly(data = data,valueVar = "BG.Reading..mg.dL.",
          sumFunc = "mean",
          brks = c(0,50,80,150,240,300,400,500), 
          libraryPath = libraryPath)
}
