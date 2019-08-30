#' @export YearAnalizator
YearAnalizator <- setRefClass("YearAnalizator", 
  fields = list(dfm = "ANY",ftc="ANY"),
  methods = list(
    initialize = function() {
      dfm <<- DataFrameMaipulator$new()
      ftc <<- FrequencyTableCreator$new()
    },                           
    calculateMinAndMaxYearUsedByVariable = function(dataFrame,listOfVars,firstColumnTitle="") {
      if(!is.data.frame(dataFrame))
        stop("This is not a valid data frame!")
      
      if(length(dataFrame)==0)
        return(dataFrame)
      
      if(!dfm$areColumnNamesInDataFrame(dataFrame,listOfVars))
          stop("One or more column name does not exist in data frame!")
      
      usagePerYear <- ftc$createFrequencyTableForListOfVarsInComboWithVars(dataFrame,listOfVars,"Year")
      usagePerYear$Year <- as.numeric(as.character(usagePerYear$Year))
      
      resultDF <- data.frame()
      for(tool in listOfVars){
        colWithNoNA <- dfm$removeNAValuesFromDataFrame(usagePerYear[,c("Year",tool)])
        minYear <- NA; maxYear <- NA;
        if(nrow(colWithNoNA)>0){
          minYear <- min(colWithNoNA$Year)
          maxYear <- max(colWithNoNA$Year)
        }
        resultDF <- rbind(resultDF,data.frame(firstColumn=c(tool),lastYear=c(maxYear),firstYear=c(minYear)))
      }
      
      resultDF <- dfm$resolveProblemWithLostSortPosibilityAndFactorLevelsSort(resultDF)
      resultDF <- dfm$orderDataFrameValuesByColumns(resultDF,c("lastYear","firstYear"))
      resultDF <- dfm$changeColumnNameInDataFrame(resultDF,"firstColumn",firstColumnTitle)
      return(resultDF)
    }
  ))