#' @import ggplot2
#' @import plyr
#' @import reshape
#' @import graphics
#' @import utils
#' @import readxl
#' @import stats
#' @import grDevices
#' @import devtools
#' @import qqplotr


#' @export
installAllMyPackages <- function() {
  install.packages("Rcmdr", dependencies=TRUE)
  install.packages("tm", dependencies=TRUE)
  install.packages("wordcloud", dependencies=TRUE)
  install.packages("rmarkdown", dependencies=TRUE)
  install.packages("rJava", dependencies=TRUE)
  install.packages("Rserve", dependencies=TRUE)
  install.packages("RODBC", dependencies=TRUE)
  install.packages("whisker", dependencies=TRUE)
  install.packages("plyr",dependencies=TRUE)
# install.packages("DSUR",dependencies = TRUE)
  install.packages("devtools", dependencies = TRUE)
  installDSUR()
  install.packages("ggplot2",dependencies = TRUE)
  install.packages("reshape", dependencies = TRUE)
  install.packages("testthat",dependencies = TRUE)
  install.packages("knitr",dependencies = TRUE)
  install.packages("kableExtra",dependencies = TRUE)
  install.packages("xtable",dependencies = TRUE)
  
  #https://github.com/haozhu233/kableExtra/
  devtools::install_github("haozhu233/kableExtra")
}
#' @export
installDSUR<-function(){
  #https://github.com/Frostarella/DSUR.noof
  #library(devtools)
  install_github("Frostarella/DSUR.noof")
}
#' @export
unloadAPackage <- function(packageName){
  fullName <- paste("package:",packageName,sep="")
  detach(fullName, character.only = TRUE)
}
#' @export
loadAllMyPackages <- function(){
  #library(devtools); library(tm); library(wordcloud);library(rmarkdown);library(rJava);library(RODBC);library(whisker);library(plyr);library(ggplot2);library(reshape); library(testthat)
  stop("not for usage load in each project as needed or over the library set depends instead of imports")
}

#' @export
saveMyPackageList <- function() {
  save.pkg.list <- installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]
  save(save.pkg.list, file="pkglist.Rdata")
}
#' @export
loadAndInstallPackageList <- function() {
  save.pkg.list <- load("pkglist.Rdata")
  install.packages(save.pkg.list)
}
#' @export
runRcomander <- function() {
  #library(Rcmdr)
  stop("not fo usage just type library(Rcmdr)")
}

#' @export
loadSheetFromXMLFile <- function(fileName,sheetName) {
  myfile <- read_excel(fileName, sheet = sheetName, skip = 1, trim_ws=TRUE)
  
  #convert form characters to factors
  character_vars <- lapply(myfile, class) == "character"
  myfile[, character_vars] <- lapply(myfile[, character_vars], as.factor)

  myfile <- as.data.frame(myfile)
  
  return(myfile)
}

#helper functions
#' @export ListManipulator
ListManipulator <- setRefClass("ListManipulator", 
  methods = list(
    addElementToList = function (dfList, newElement){
      getNewIndex <- function(dfList){
        return(length(dfList)+1)
      }
      
      if(!is.list(dfList) | is.data.frame(dfList))
        stop("This is not a list!")
      
      dfList[[getNewIndex(dfList)]] <- newElement
      return(dfList)
    },
    
    throwErrorIfCharacterListIsNotOk = function(characterInputList, name){
      if(!is.atomic(characterInputList)) stop("This is not a valid list for",name,"! It should be a character list!")
      lapply(characterInputList,function(x) { if(!is.character(x) | x=="") stop(name," is not a valid list! It should be a character list!") else x })
    }
))