#' @export DataFrameMaipulator
DataFrameMaipulator <- setRefClass("DataFrameMaipulator",
                                   
  fields = list(listm = "ANY"),
  
  methods = list(
    initialize = function() {
      listm <<- ListManipulator$new()
    }, 
    
    replaceAllValuesInColumnWithValue = function(dataFrame,varName,oldValue,newValue) {
      if(!is.data.frame(dataFrame))
        stop("Value under dataFrame is not a data frame!")
      else if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
      
      dfOneCol=NULL
      for(columnName in colnames(dataFrame)){
        if(columnName == varName)
          dfOneCol <- dataFrame[,varName]
      }
     
      if(is.null(dfOneCol))
        stop("Such variable does not exist!")
      
        if(length(dfOneCol[dfOneCol==oldValue])>0){
          
          if(is.factor(dataFrame[,varName])){
            dataFrame <- addNewFactorLevel(dataFrame,varName,newValue)
            dfOneCol <- dataFrame[,varName]
          }
          
          dfOneCol[dfOneCol==oldValue] <- newValue
          dataFrame[,varName] <- dfOneCol
          dataFrame <- removeUnusedFactorLevels(dataFrame)
        }

      return(dataFrame)
    },
    
    replaceAllNaValuesWithValue = function(dataFrame,value) {
      if(!is.data.frame(dataFrame))
        stop("Value under dataFrame is not a data frame!")
      else if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
      
    
      for(columnName in colnames(dataFrame)){
        dfOneCol <- dataFrame[,columnName]
        if(length(dfOneCol[is.na(dfOneCol)])>0){
        
          if(is.factor(dataFrame[,columnName])){
            dataFrame <- addNewFactorLevel(dataFrame,columnName,value)
            dfOneCol <- dataFrame[,columnName]
          }
            
          dfOneCol[is.na(dfOneCol)] <- value
          dataFrame[,columnName] <- dfOneCol
        }
      }
      return(dataFrame)
    },
    
    removeNAValuesFromDataFrame = function(dataFrameOrList) {
      dataFrameOrList <- na.omit(dataFrameOrList)
      dataFrameOrList <- removeUnusedFactorLevels(dataFrameOrList)
      return(dataFrameOrList)
    },
    
    orderDataFrameValuesByColumns = function(dataFrame, columnNames, descending=FALSE){
      
      createOrderCommandForColumnNames <-  function(dataFrame,columnNames,descending) {
        command <- "dataFrame <- dataFrame[with(dataFrame, order("
        
        for(col in columnNames)
          if(isColumnNameInDataFrame(dataFrame,col))
            command <- paste0(command,"dataFrame$",col,",")
          else
            stop("One or more column name does not exist in data frame!")
        
        command=substr(command, 1, nchar(command)-1)
       
         if(descending)
          command=paste0(command,",decreasing=",descending)
        
        command <- paste0(command,")), ]")
        return(command)
      }
      
      listm$throwErrorIfCharacterListIsNotOk(columnNames,"columnNames")
      
      command <- createOrderCommandForColumnNames(dataFrame,columnNames,descending)
    
      eval(parse(text=command)) 
      rownames(dataFrame) <- NULL
      
      return(dataFrame)
    },
    
    changeFactorToUnaryNumericType = function(factor, unaryNumericValue){
      if(!is.factor(factor))
        stop("This is not a factor!")
      if(!is.numeric(unaryNumericValue))
        stop("UnaryNumericValue is not a numeric value!")
    
      factor <- as.character(factor)
      factor[!is.na(factor)] <- unaryNumericValue
      factor<- as.numeric(factor)
      
      return(factor)
    },
    
    addTotalRowAtTheBottomOfDataFrame = function(dataFrame,useFirstColumnToDisplayTotal=FALSE){
      if(!is.data.frame(dataFrame))
        stop("This is not a data frame!")
      if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
    
      totalRow <- createNewRowForDataFrameWihValues(dataFrame,NA)
    
      for(col in colnames(dataFrame))
        if(is.numeric(dataFrame[,col])){
          colWithRemovedNA <- removeNAValuesFromDataFrame(dataFrame[,col])
          totalRow[,col] <- sum(colWithRemovedNA)
        }
      
      if(useFirstColumnToDisplayTotal)  
        totalRow[,1] <- "Total"
      
      dataFrame <- rbind(dataFrame, totalRow)
      return(dataFrame)
    },
    
    addTotalColumnAtTheEndForColumns = function(dataFrame, columnListToSum, totalColumnName) {
      if(!is.data.frame(dataFrame))
        stop("This is not a data frame!")
      if(isDataFrameEmpty(dataFrame) || length(columnListToSum)==0)
        return(dataFrame)
      if(!is.character(totalColumnName))
        stop("This is not a valid column name! It should be a character!")
      listm$throwErrorIfCharacterListIsNotOk(columnListToSum, "columnListToSum")
      
      newColumn <- rowSums(dataFrame[,columnListToSum])
      dataFrame <- cbind(dataFrame,newColumn)
      colnames(dataFrame)[ncol(dataFrame)] <- totalColumnName 
      
      return(dataFrame)
    },
    
    addNewFactorLevel = function(dataFrame,factorName,newFactorLevel) {
      dataFrame[,factorName] <- as.factor(dataFrame[,factorName])
      levels(dataFrame[,factorName]) <- c(levels(dataFrame[,factorName]),newFactorLevel)
      dataFrame[,factorName] <- factor(dataFrame[,factorName],levels(dataFrame[,factorName])[order(levels(dataFrame[,factorName]))])
      return(dataFrame)
    },
    
    removeUnusedFactorLevels = function(dataFrame) {
      dataFrame <- as.data.frame(lapply(dataFrame, function(x) { if(is.factor(x)) factor(x) else x }))
      return(dataFrame)
    },
    
    resolveProblemWithLostSortPosibilityAndFactorLevelsSort = function(dataFrame) {
      if(!is.data.frame(dataFrame))
        stop("This is not a valid data frame!")
      
      if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
    
      dataFrame <- as.data.frame(lapply(dataFrame, function(x) { if(is.factor(x)) as.factor(as.character(x)) else x }))
    },
    
    splitColumnInDataFrame = function(dataFrame, columnName, separator,newNameCol1,newNameCol2){
      if(!isColumnNameInDataFrame(dataFrame,columnName)){
        return(dataFrame)
      } else if(identical(newNameCol1,"") | identical(newNameCol2,"")){
        stop("New var name is missing!")
      } else if(!is.atomic(dataFrame[,columnName]) | is.numeric(dataFrame[,columnName])){
        return(dataFrame)
      }
      
      splitedDF <- data.frame(do.call('rbind', strsplit(as.character(dataFrame[,columnName]),separator,fixed=TRUE)))
      if(ncol(splitedDF)==1)
        stop("Non of the values contains this separator!")
      
      colnames(splitedDF) <- c(newNameCol1,newNameCol2)
      splitedDF <- cbind(dataFrame,splitedDF)
      removeUnusedFactorLevels(splitedDF)
      
      return(splitedDF)
    },
    
    changeColumnNameInDataFrame = function(dataFrame,oldName,newName){
      colnames(dataFrame)[colnames(dataFrame) == oldName] <- newName
      return(dataFrame)
    },
    
    isColumnNameInDataFrame = function(dataFrame, colName){
      return(length(grep(paste0("^",colName,"$"),colnames(dataFrame),ignore.case = FALSE))>0)
    },
    
    areColumnNamesInDataFrame = function(dataFrame,colNames) {
      for(colName in colNames){
        if(length(colName)>1) areColumnNamesInDataFrame(dataFrame,colName)
        else if(!isColumnNameInDataFrame(dataFrame,colName)) return(FALSE)
      }
      return(TRUE)
    },
    
    
    orderColumnsAndValuesByNameInDataFrameList = function (dataFrameList) {
      dataFrameList <-lapply(dataFrameList,function (x) { x[,order(names(x))] })
      return(dataFrameList)
    },
    
    joinListOfDataFrames = function (dfList, joinByVariableName = c()){
      if(length(dfList)<2)
        return(removeUnusedFactorLevels(dfList[[1]]))
    
      joinedDF = join_all(dfList, by = joinByVariableName, type = 'full')
      joinedDF <- resolveProblemWithLostSortPosibilityAndFactorLevelsSort(joinedDF)
      joinedDF <- removeUnusedFactorLevels(joinedDF)
      
      return(joinedDF)
    },
    
    
    transponseDataFrame = function(dataFrame,firstColumnAsHeader=TRUE) {
      
      makeFirstRowAHeader <- function(oldDataFrame, transpondedDataFrame) {
        myNewHeaders <- c("VarLevels",as.character(oldDataFrame[,1]))
        colnames(transpondedDataFrame) <- myNewHeaders
        transpondedDataFrame <- transpondedDataFrame[-1,]
        return(transpondedDataFrame)
      }
      
      if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
    
      myNewVariable <- data.frame(OldColumns=colnames(dataFrame))
      transpondedDataFrame <- as.data.frame(t(dataFrame))
      transpondedDataFrame <- cbind(myNewVariable, transpondedDataFrame)
      
      if(firstColumnAsHeader) 
        transpondedDataFrame <- makeFirstRowAHeader(dataFrame,transpondedDataFrame)
      
      transpondedDataFrame <- removeUnusedFactorLevels(transpondedDataFrame)
      rownames(transpondedDataFrame) <- NULL
      
      return(transpondedDataFrame)
    },
    
    createDataFrameWithNColumnsWithValue = function(numOfColumns,value){
      if(numOfColumns < 1)
        return(data.frame())
      
      newRowValues <- c(rep(value,numOfColumns))
      newRow <- do.call("cbind", lapply(newRowValues, as.data.frame))
      colnames(newRow) <- lapply(1:ncol(newRow),function(cNum) {paste0("C",cNum)})
      return(newRow)
    },
    
    createNewRowForDataFrameWihValues = function(dataFrame, value){
      newRow<-createDataFrameWithNColumnsWithValue(ncol(dataFrame),value)
      colnames(newRow) <- colnames(dataFrame)
      return(newRow)
    },
    
    addMissingaluesToColumnInDataFrame = function(dataFrame,colName,fillOtherVarsWith=0) {
    
      addMissingRowsToDataFrame <- function(dataFrame,colName) {
        
        addMissingRowToDataFrame <- function(dataFrame,colName,value) {
          newRow <- createNewRowForDataFrameWihValues(dataFrame,NA)
          newRow[,colName] <- value
          dataFrame <- rbind(dataFrame,newRow)
          
          return(dataFrame)
        }
        
        varRagne <- range(as.numeric(as.character(dataFrame[,colName])))
        for(i in varRagne[1]:varRagne[2])
          if(!(i %in% dataFrame[,colName]))
            dataFrame <- addMissingRowToDataFrame(dataFrame,colName,i)
          return(dataFrame)
      }
      
      
      if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
      
      if(!isColumnNameInDataFrame(dataFrame,colName))
        stop("Variable name does not exist in dataFrame!")
      
      if(!is.numeric(dataFrame[,colName]))
        stop("Variable sholud be numeric!")
      
      dataFrame <- addMissingRowsToDataFrame(dataFrame, colName)
      dataFrame <- replaceAllNaValuesWithValue(dataFrame,fillOtherVarsWith)
      dataFrame <- orderDataFrameValuesByColumns(dataFrame,colName)
      rownames(dataFrame) <- NULL
      
      return(dataFrame)
    },
    
    moveColumnToTheEndOfDataFrame = function (dataFrame, colNameToMove) { 
      if((isDataFrameEmpty(dataFrame) & length(dataFrame)==0) | !isColumnNameInDataFrame(dataFrame,colNameToMove))
        return(dataFrame)
      
      if(length(colnames(dataFrame))==2 & grep(colNameToMove,colnames(dataFrame))==1)
        dataFrame <- dataFrame[,c(2,1)]
      else if(length(colnames(dataFrame))>2) 
        dataFrame <- dataFrame[,c(colnames(dataFrame[,!grepl(colNameToMove,colnames(dataFrame))]),colNameToMove)] 
      
      return(dataFrame)
    },
    
    isDataFrameEmpty = function(inputDataFrame) {
      if(is.data.frame(inputDataFrame) & (is.null(nrow(inputDataFrame)) | nrow(inputDataFrame)==0))
        return(TRUE)
      
      return(FALSE)
    },
    
    round_df = function(df, digits) {
      nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      
      df[,nums] <- round(df[,nums], digits = digits)
      
      return(df)
    },
    
    createDataFrameOfPrintColumnValuesForColumns = function(dataFrame,listOfInterestedColumns,printColumnName){
      if(isDataFrameEmpty(dataFrame))
        return(dataFrame)
      
      if(!isColumnNameInDataFrame(dataFrame,printColumnName))
        stop("printColumnName name does not exist in dataFrame!")
      
      if(!areColumnNamesInDataFrame(dataFrame,listOfInterestedColumns))
        stop("One clumn name in the listOfInterestedColumns does not exist in dataFrame!")
      
      createPrintElementList <- function(elementList){
        printElementList <- ""
        for(e in elementList)
          printElementList <- paste0(printElementList,e,", ")
        
        printElementList<-substr(printElementList,0,nchar(printElementList)-2)
        
        return(printElementList)
      }
      
      dataFrameForList <- data.frame()
      for(element in listOfInterestedColumns){
        elementList <- dataFrame[!is.na(dataFrame[,element]),printColumnName]
        dataFrameForElement <- data.frame(Name=element,Values=createPrintElementList(elementList))
        dataFrameForList <- rbind(dataFrameForList,dataFrameForElement)
      }
      
      return(dataFrameForList)
    }
))
