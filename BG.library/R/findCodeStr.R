#'@title findCodeStr
#'@description Determine all locations within the directory a given string occurs \\cr \\cr
#'@param path character string path to code directory.
#'@param str character string to find in function
#'@return `allReps` data.frame of filenames/paths and line numbers
#'@examples
#'findCodeStr("//gs.doi.net/baltimoremd-w/Projects/windows/Rhour/CodeLibrary/","readLines")
#'@keywords string find search


##################################################
## Project: Rhour
## Script purpose: Search directory of Rscripts and *.Rmd files for string defined using regexpr
## Date: 07.22.2019
## Author: Lillian Gorman Sanisaca, lgormansanisaca@usgs.gov
## RequiredLibraries : 
##################################################


findCodeStr<-function(path,str){
  #get all files
  files<-list.files(path,full.names = TRUE,pattern="\\.R",recursive = TRUE)
  Rfiles<-files[which(sapply(files, function(x) substr(x,nchar(x)-1,nchar(x))==".R"))]
  RmdFiles<-files[which(sapply(files, function(x) substr(x,nchar(x)-3,nchar(x))==".Rmd"))]
  files<-c(Rfiles,RmdFiles)
  
  #loop through files
  for (f in files){
    
    #read file as text
    x <- suppressWarnings(readLines(f))
    #trim white space
    x<-sapply(x,trimws)

      #search for str    
      findStr<-x[which(regexpr(str,x)>0)]
      lines<-which(regexpr(str,x)>0)

    
    if (length(lines)!=0){ #if str found

        found<-data.frame(file = rep(gsub(path,"",f),length(lines)),
                          lines = lines)
   
      
      #compile all strings found
      if (!exists("allReps")){
        allReps<-found
      }else{
        allReps<-rbind(allReps,found)
      }#end exist allReps
    }#end length(lines)
  }#end for f
  
  #print list of found
  if (exists("allReps")){
    return(allReps)
  }else{
    message(paste0("'",str,"' NOT FOUND IN ",path))
  }
}#end function
