% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/historySeqOut.R
\name{historySeqOut}
\alias{historySeqOut}
\title{historySeqOut}
\usage{
historySeqOut(data = NA, plotName = NA, paramList = NA,
  plotType = NA, seqType = "change", seqLength = 2, period = NA,
  removeDates = NA, reportTitle = "", outPath = NA,
  outFileName = NA, libraryPath, filePath)
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

\item{reportTitle}{character string title for html document}

\item{outPath}{character string directory in which to output html document, if \code{outPath<-NA}
temporary html document will be stored in ./BG.library/reports/historySeqOut.html}

\item{outFileName}{character string file name if \code{outFileName<-NA}
temporary html document will be stored in ./BG.library/reports/historySeqOut.html}

\item{libraryPath}{character string path to BG.library code}

\item{filePath}{character path to csv import file}
}
\description{
Execute series of historical plots through time sequence and output html
document with interactive plots\cr \cr
}
\examples{
libraryPath<-"F:/BG.library_github/BG.library/"
filePath<-"F:/BG.library_github/exampleData.csv"
historySeqOut(plotName = "lineSumSens_SGper_Sett_BG",
             reportTitle = "Compare Summary Sensor Line Plot Since Last Pump Setting Change" ,
             libraryPath = libraryPath, filePath = filePath)
}
