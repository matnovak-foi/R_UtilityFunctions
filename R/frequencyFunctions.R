#' @export FrequencyTableCreator
FrequencyTableCreator <- setRefClass("FrequencyTableCreator",
                                     
  fields = list(dfm = "ANY", listm = "ANY"),
  
  methods = list(
    initialize = function() {
      dfm <<- DataFrameMaipulator$new()
      listm <<- ListManipulator$new()
    },
    
    createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars = function(mydata, varNameList, commonName, varNamesToCombineWith){
      
      checkInputData <- function(mydata,varNameList,varNamesToCombineWith){
        if(!is.data.frame(mydata))
          stop("dataFrame is no a data frame!")
        
        if(!dfm$areColumnNamesInDataFrame(mydata,varNameList) | !dfm$areColumnNamesInDataFrame(mydata,varNamesToCombineWith))
          stop("varNameList or varNamesToCombineWith contains columns names that are not avalible in given dataFrame")
      }
      
      checkIfDataFrameHasUnaryValues <- function(resultDFWithoutComboVars) {
        if(ncol(resultDFWithoutComboVars)!=2)
          stop("dataFrameShould only cotain one value in all vars given in varNameList!")
      }
      
      createFrequencyTableListWithOneElementWithoutComboVars <- function(resultDFWithoutComboVars){
        listOfResultsToBeJoined <- list()
        resultDFWithoutComboVars <- dfm$changeColumnNameInDataFrame(resultDFWithoutComboVars,colnames(resultDFWithoutComboVars)[2],"All")
        listOfResultsToBeJoined[[1]] <- resultDFWithoutComboVars
        return(listOfResultsToBeJoined)
      }
      
      createFrequencyTableListInComboWithEachComboVar <- function(listOfResultsToBeJoined, mydata, varNameList, commonName, varNamesToCombineWith) {
        
        createFrequencyTableForInComboWithOneVar <- function(mydata,varNameList,commonName,varNameToCombine){
          
          filterMyDataWithVarNameToCombine <- function(mydata,varNameToCombine){
            for(vnc in varNameToCombine) 
              mydata <- mydata[!is.na(mydata[,vnc]),]
            
            return(mydata)
          }
          
          createNewNameForFreqColumnWithThisCoboVar <- function(varNameToCombine) {
            newColName <- ""
            for(vnc in varNameToCombine) 
              newColName <- paste0(newColName,vnc)
            
            return(newColName)
          }
          
          newColName <- createNewNameForFreqColumnWithThisCoboVar(varNameToCombine)
          mydataFilteredByComboVar <- filterMyDataWithVarNameToCombine(mydata,varNameToCombine)
          resultDFByComboVar <- createFrequencyTableForListOfVarsByVarValues(mydataFilteredByComboVar,varNameList,commonName)
          resultDFByComboVar <- dfm$changeColumnNameInDataFrame(resultDFByComboVar,colnames(resultDFByComboVar)[2],newColName)
          
          return(resultDFByComboVar)
        }
        
        for(varNameToCombine in varNamesToCombineWith)
          listOfResultsToBeJoined <- listm$addElementToList(listOfResultsToBeJoined,createFrequencyTableForInComboWithOneVar(mydata,varNameList,commonName,varNameToCombine))
        
        return(listOfResultsToBeJoined)
      }
      
      checkInputData(mydata,varNameList,varNamesToCombineWith)
      
      if(dfm$isDataFrameEmpty(mydata))
        return(mydata)
      
      resultDFWithoutComboVars <- createFrequencyTableForListOfVarsByVarValues(mydata,varNameList,commonName)
      checkIfDataFrameHasUnaryValues(resultDFWithoutComboVars)
      
      if(length(varNamesToCombineWith)>0){
        listOfResultsToBeJoined <- createFrequencyTableListWithOneElementWithoutComboVars(resultDFWithoutComboVars)
        listOfResultsToBeJoined <- createFrequencyTableListInComboWithEachComboVar(listOfResultsToBeJoined, mydata, varNameList, commonName, varNamesToCombineWith)
        finalResult <- dfm$joinListOfDataFrames(listOfResultsToBeJoined,commonName)
      }else{
        finalResult <- resultDFWithoutComboVars
      }
      
      return(finalResult)
    },
    
    createFrequencyTableForListOfVarsByVarValues = function(mydata, varNameList, commonName){
      
      convertAllFactorExceptVarTypeToNumeric <- function(dataFrame,commonName) {
        for(cName in colnames(dataFrame)) 
          if(!identical(cName,commonName))
            dataFrame[,cName] <- as.numeric(as.character(dataFrame[,cName]))
          
          return(dataFrame)
      }
      
      listm$throwErrorIfCharacterListIsNotOk(varNameList,"varNameList")
      if(length(varNameList) == 0)
        stop("No varNames given!")
      
      dataFrame <- PRIVATE_createFrequencyTableForListOfVars(mydata,varNameList)
      dataFrame <- dfm$transponseDataFrame(dataFrame)
      dataFrame <- dfm$changeColumnNameInDataFrame(dataFrame,"VarLevels",commonName)
      dataFrame <- convertAllFactorExceptVarTypeToNumeric(dataFrame,commonName)
      
      return(dataFrame)
    },

    PRIVATE_createFrequencyTableForListOfVars = function(mydata, varNameList){
      
      prepareDataFrameFunctionWithoutComboVars <- function(dataFrame,varNamesToCombineWith,varName) {
        return(dfm$changeColumnNameInDataFrame(dataFrame,varName,"VarTypes"))
      }  
      
      return(PRIVATE_createFrequencyTableForListOfVarsInComboWithVarsOrWithout(mydata, varNameList, NULL, "VarTypes", prepareDataFrameFunctionWithoutComboVars))
    },
    
    createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency = function(mydata, varNameList, varNamesToCombineWith, titleOfComboVarFreqColumn){
      if(dfm$isDataFrameEmpty(mydata))
        return(mydata)
      
      if(!is.character(titleOfComboVarFreqColumn) | titleOfComboVarFreqColumn=="")
        stop("Title must be a character and can not be empty!")
      
      dataFrame <- createFrequencyTableForListOfVarsInComboWithVars(mydata, varNameList, varNamesToCombineWith)
      
      comboVarTotals <- createFreqencyTableWithoutNAValues(mydata,varNamesToCombineWith)
      comboVarTotals <- dfm$changeColumnNameInDataFrame(comboVarTotals,"freq",titleOfComboVarFreqColumn)
      dataFrameWithComboFreq <- dfm$joinListOfDataFrames(list(dataFrame,comboVarTotals),varNamesToCombineWith)
      
      return(dataFrameWithComboFreq)
    },
    
    createFrequencyTableForListOfVarsInComboWithVars = function(mydata, varNameList, varNamesToCombineWith) {
      
      prepareDataFrameFunctionWithComboVars <- function(dataFrame,varNamesToCombineWith,varName) {
        
        mergeAllValuesOfVaraibleInDataFrame <- function(dataFrame,varNamesToCombineWith) {
          return(createFreqencyTableWithoutNAValues(dataFrame,varNamesToCombineWith,"freq"))
        }
        
        return(mergeAllValuesOfVaraibleInDataFrame(dataFrame,varNamesToCombineWith))
      }  
      
      resultDF <- PRIVATE_createFrequencyTableForListOfVarsInComboWithVarsOrWithout(mydata, varNameList, varNamesToCombineWith, varNamesToCombineWith, prepareDataFrameFunctionWithComboVars)
      if(!identical(varNamesToCombineWith,c()))
        resultDF <- dfm$orderDataFrameValuesByColumns(resultDF,varNamesToCombineWith)
      
      return(resultDF)
    },
    
    PRIVATE_createFrequencyTableForListOfVarsInComboWithVarsOrWithout = function(mydata, varNameList, varNamesToCombineWith, joinByVaraible, prepareDataFrameFunction) {
      
      prepareDataFramesToBeJoinable <- function(dfList, varNameList, varNamesToCombineWith, prepareDataFrameFunction){
        
        prepareDataFrameToBeJoinable <- function(dataFrame, varNameList, varNamesToCombineWith, prepareDataFrameFunction) {
          
          findAppropreateVarName <- function(dataFrame,varNames) {
            for(varName in varNameList)
              if(dfm$areColumnNamesInDataFrame(dataFrame,varName))
                return(varName)
          }
          
          varName <- findAppropreateVarName(dataFrame, varNameList)
          dataFrame <- prepareDataFrameFunction(dataFrame,varNamesToCombineWith,varName)
          dataFrame <- dfm$changeColumnNameInDataFrame(dataFrame,"freq",varName)
          
          return(dataFrame)
        }
        
        dfList <- lapply(dfList, prepareDataFrameToBeJoinable, varNameList, varNamesToCombineWith, prepareDataFrameFunction)
        
        return(dfList)
      }
      
      checkIfAnyComboVarIsAlsoInTheVarNameList <- function(varNameList, comboVarNameList){
        if(is.character(comboVarNameList) & !identical(comboVarNameList,"")){
          for(comboVarName in comboVarNameList)
            if(length(grep(comboVarName,varNameList))!=0)
              stop("Combovar can not be also the var in the list!")
        }
      }
      
      listm$throwErrorIfCharacterListIsNotOk(varNamesToCombineWith,"varNamesToCombineWith")
      listm$throwErrorIfCharacterListIsNotOk(varNameList,"varNameList")
      
      if(dfm$isDataFrameEmpty(mydata))
        return(data.frame())
      
      checkIfAnyComboVarIsAlsoInTheVarNameList(varNameList,varNamesToCombineWith)
      
      dfList <- PRIVATE_createFrequencyTableList(mydata,varNameList,list(varNamesToCombineWith),.self$createFreqencyTableWithoutNAValues)
      dfList <- prepareDataFramesToBeJoinable(dfList,varNameList, varNamesToCombineWith, prepareDataFrameFunction)
      joinedDFlist <- dfm$joinListOfDataFrames(dfList,joinByVaraible)
      
      return(joinedDFlist)
    },
    
    PRIVATE_createFrequencyTableList = function(mydata, varNamesList, comboVarNamesList = list(), runFrequencyTableFunction=createFrequencyTable){
      
      runFrequencyTableFunctionForVarNAme <- function(dfFrequencyList,mydata,varName,comboVarNamesList,runFrequencyTableFunction){
        
        runFrequencyTableFunctionForVarNAmeAndComboVarName <- function(dfFrequencyList,mydata,varName,comboVarName,runFrequencyTableFunction){
          if(identical(varName,comboVarName)){
            freqT <- runFrequencyTableFunction(mydata,varName)
            dfFrequencyList <- listm$addElementToList(dfFrequencyList,freqT)
          }
          else{   
            freqT <- runFrequencyTableFunction(mydata,c(comboVarName,varName))
            dfFrequencyList <- listm$addElementToList(dfFrequencyList,freqT)
          }
          return(dfFrequencyList)
        }
        
        if(length(comboVarNamesList)==0)
          dfFrequencyList <- listm$addElementToList(dfFrequencyList,runFrequencyTableFunction(mydata,varName))
        else
          for(comboVarName in comboVarNamesList)
            dfFrequencyList <- runFrequencyTableFunctionForVarNAmeAndComboVarName(dfFrequencyList,mydata,varName,comboVarName,runFrequencyTableFunction)
          
          return(dfFrequencyList)
      }
      
      removeDuplicatesAndSortColumnsInEachDataFrame <- function(dfFrequencyList) {
        
        removeDuplicateColumns <- function(dfFrequencyList){
          dfFrequencyList <- lapply(dfFrequencyList,function (dfFreq) { dfFreq[,!grepl("\\w\\.\\d",colnames(dfFreq))] })
          return(dfFrequencyList)
        }
        
        moveVariableToTheEndInEachDF <- function(dfFrequencyList,varNameToMove) {
          dfFrequencyList <- lapply(dfFrequencyList, dfm$moveColumnToTheEndOfDataFrame, varNameToMove)
          return(dfFrequencyList)
        }
        
        dfFrequencyList <- removeDuplicateColumns(dfFrequencyList)
        dfFrequencyList <- dfm$orderColumnsAndValuesByNameInDataFrameList(dfFrequencyList)
        dfFrequencyList <- unique(dfFrequencyList)
        dfFrequencyList <- moveVariableToTheEndInEachDF(dfFrequencyList,"freq")
        
        return(dfFrequencyList)
      }  
      
      dfFrequencyList <- list()
      for(varName in varNamesList)
        dfFrequencyList <- runFrequencyTableFunctionForVarNAme(dfFrequencyList,mydata,varName,comboVarNamesList,runFrequencyTableFunction)
      
      dfFrequencyList <- removeDuplicatesAndSortColumnsInEachDataFrame(dfFrequencyList)
      
      return(dfFrequencyList)
    },
    
    createFrequencyTableForUnaryValueVariable = function(mydata, varName, naValuesMeaning){
      mydata <- createFrequencyTable(mydata,varName)
      
      mydata <- dfm$addNewFactorLevel(mydata,varName,varName)
      mydata[!is.na(mydata[,varName]),varName] <- varName
      
      mydata <- dfm$addNewFactorLevel(mydata,varName,naValuesMeaning)
      mydata[is.na(mydata[,varName]),varName] <- naValuesMeaning
      
      colnames(mydata) <- c("Name","Count")
      mydata <- dfm$removeNAValuesFromDataFrame(mydata)
      
      return(mydata)
    },
    
    createFreqencyTableWithoutNAValues = function(dataFrame, varNames, wegitheningVar = NULL) {
      freqTable <- createFrequencyTable(dataFrame, varNames, wegitheningVar)
      freqTable <- dfm$removeNAValuesFromDataFrame(freqTable)
      return(freqTable)
    },
    
    createFrequencyTable = function(dataFrame, varNames, wegitheningVar = NULL){
      if(!is.data.frame(dataFrame) || varNames=="")
        stop("No valid data!")
      
      if(dfm$isDataFrameEmpty(dataFrame))
        return(dataFrame)
      
      if(dfm$areColumnNamesInDataFrame(dataFrame,"freq") & is.null(wegitheningVar))
        dataFrame <- dfm$changeColumnNameInDataFrame(dataFrame, "freq", "tempFreq444")
      
      freqTable <- count(dataFrame,varNames,wt_var = wegitheningVar)
      
      if(dfm$areColumnNamesInDataFrame(dataFrame,"freq") & is.null(wegitheningVar))
        freqTable <- dfm$changeColumnNameInDataFrame(freqTable, "tempFreq444", "freq")
      
      return(freqTable)
    }            
))