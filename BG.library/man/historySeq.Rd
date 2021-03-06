% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/historySeq.R
\name{historySeq}
\alias{historySeq}
\title{historySeq}
\usage{
historySeq(data, plotName, paramList, plotType, seqType, seqLength,
  period = NA, removeDates = NA, libraryPath)
}
\arguments{
\item{data}{data.frame to be plotted}

\item{plotName}{character string name of saved plot to be executed.   If \code{plotName = NA} plot to execute is
not a saved plot.}

\item{paramList}{list of plot parameters needed to execute plot.  If executing a saved
plot set to \code{NA}}

\item{plotType}{character string indicating type of plot to execute (i.e. 'plotLine_ly',
'summaryPlot_ly' or 'heatmMap_ly')}

\item{seqType}{character string indicating the type of historical sequence to output
('change' indicating plots at dates where pump settings where changed or 'days' for plots
every \code{period} number of days)}

\item{seqLength}{number of plots to output}

\item{period}{number of days per plot if \code{seqType = 'days'}}

\item{removeDates}{character vector of dates in format %Y-%m-%d to remove from data}

\item{libraryPath}{character string path to BG.library code}
}
\description{
Execute series of historical plots through time sequence \cr \cr
}
\examples{
libraryPath<-"F:/BG.library_github/BG.library/"
filePath<-"F:/BG.library_github/exampleData.csv"
dataImport.list<-dataImport(filePath,libraryPath)
data<-dataImport.list$allData
historySeq(data = data,  plotName = "lineSumSens_SGper_Sett_BG", paramList = NA, plotType = NA,
          seqType = "change", seqLength = 3,libraryPath)
}
