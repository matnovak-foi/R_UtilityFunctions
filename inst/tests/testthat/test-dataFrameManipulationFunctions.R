context("DataFrameManipulationFunctions")
dfm <- DataFrameMaipulator$new()

emptyDataFrame <- data.frame()
dataFrameNoNAValues <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("value3","value3","value4","value4"), var3=c(1,2,3,4))
dataFrameWithNAValues <- data.frame(var1=c("value1",NA,NA), var2=c(NA,"value4",NA), var3=c(1,2,NA))

describe(" - when - replaceAllValuesInColumnWithValue",{
  context(" - when - replaceAllValuesInColumnWithValue")
  
  test_that("givenDegenerativeCases_thenReturnDataFrameAsIs",{
    expect_that(dfm$replaceAllValuesInColumnWithValue(emptyDataFrame,"var1","value1","newValue"),is_identical_to(emptyDataFrame))
    expect_that(dfm$replaceAllValuesInColumnWithValue("notADataFrame","var1","value1","newValue"),throws_error("Value under dataFrame is not a data frame!"))
    expect_that(dfm$replaceAllValuesInColumnWithValue(dataFrameNoNAValues,"unexistingVariable","value1","newValue"),throws_error("Such variable does not exist!"))
    expect_that(dfm$replaceAllValuesInColumnWithValue(dataFrameNoNAValues,"var1","unexistingValue","newValue"),is_identical_to(dataFrameNoNAValues))
  })
  
  test_that("givenDataFrameWithOldValues_thenReturnDataFrameWithNewValues",{
    expectedDataframe <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("newValue","newValue","value4","value4"), var3=c(1,2,3,4))
    expect_that(dfm$replaceAllValuesInColumnWithValue(dataFrameNoNAValues,"var2","value3","newValue"),is_identical_to(expectedDataframe))
  })
})

describe(" - when - replaceAllNaValuesWithValue",{
  context(" - when - replaceAllNaValuesWithValue")
  
  test_that("givenDegenerativeCases_thenReturnDataFrameAsIs",{
    expect_that(dfm$replaceAllNaValuesWithValue(emptyDataFrame,0),is_identical_to(emptyDataFrame))
    expect_that(dfm$replaceAllNaValuesWithValue("notADataFrame",0),throws_error("Value under dataFrame is not a data frame!"))
    expect_that(dfm$replaceAllNaValuesWithValue(dataFrameNoNAValues,0),is_identical_to(dataFrameNoNAValues))
  })
  
  test_that("givenDataFrameWithNaValues_thenReturnDataFrameWithNAAsZero",{
    dataFrameWithZeroInsteadOfNA <- data.frame(var1=c("value1","0","0"), var2=c("0","value4","0"), var3=c(1,2,0))
    expect_that(dfm$replaceAllNaValuesWithValue(dataFrameWithNAValues,0),is_identical_to(dataFrameWithZeroInsteadOfNA))
  })
})

describe(" - when - removeNAValuesFromDataFrame",{
  context(" - when - removeNAValuesFromDataFrame")
  
  test_that("givenDataFrameWithoutNAValues_ThenReciveItBackAsIs", {
    expect_that(dfm$removeNAValuesFromDataFrame(dataFrameNoNAValues),is_identical_to(dataFrameNoNAValues))
  }) 
  
  test_that("givenDataFrameWithOneNAValues_ThenReciveDataFrameWithoutNAValues", {
    dataFrameWithOneNA <- rbind(dataFrameNoNAValues,data.frame(var1=NA,var2="removingValue",var3=1))
    
    expect_that(dfm$removeNAValuesFromDataFrame(dataFrameWithOneNA),is_identical_to(dataFrameNoNAValues))
  }) 
  
  test_that("givenDataFrameWithMultipleNAValues_ThenReciveDataFrameWithoutNAValues", {
    dataFrameWithNAValues <- rbind(dataFrameNoNAValues,dataFrameWithNAValues)
  
    expect_that(dfm$removeNAValuesFromDataFrame(dataFrameWithNAValues),is_identical_to(dataFrameNoNAValues))
  }) 
})

describe(" - when - orderDataFrameValuesByColumns",{
  test_that("givenWrongData_throwsError",{
    expect_that(dfm$orderDataFrameValuesByColumns(emptyDataFrame,1),throws_error("columnNames is not a valid list! It should be a character list!"))
    expect_that(dfm$orderDataFrameValuesByColumns(emptyDataFrame,""),throws_error("columnNames is not a valid list! It should be a character list!"))
    expect_that(dfm$orderDataFrameValuesByColumns(emptyDataFrame,"var1"),throws_error("One or more column name does not exist in data frame!"))
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameWithNAValues,"wrongVarName"),throws_error("One or more column name does not exist in data frame!"))
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameWithNAValues,c("var1","wrongVarName")),throws_error("One or more column name does not exist in data frame!"))
  })
  
  test_that("givenOkData_thenRetrunOrderedDataFrame",{
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameNoNAValues,"var1"),is_identical_to(dataFrameNoNAValues))
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameNoNAValues,c("var1","var2")),is_identical_to(dataFrameNoNAValues))
    
    resultDF <- data.frame(var1=c("value2","value1","value1","value1"), var2=c("value4","value4","value3","value3"), var3=c(4,3,1,2))
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameNoNAValues,c("var1","var2"),descending=TRUE),is_identical_to(resultDF))
    
    dataFrameNoNAValuesDESC <- data.frame(var1=c("value2","value1","value1","value1"), var2=c("value4","value4","value3","value3"), var3=c(4,3,2,1))
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameNoNAValuesDESC,"var3"),is_identical_to(dataFrameNoNAValues))
    expect_that(dfm$orderDataFrameValuesByColumns(dataFrameNoNAValues,"var3",descending=TRUE),is_identical_to(dataFrameNoNAValuesDESC))
    
  })
})

describe(" - when- changeFactorToUnaryNumericType",{
  context(" - when - changeFactorToUnaryNumericType")
  
  factorWithoutNAValues <- dataFrameNoNAValues[,"var1"]
  factorWithNAValues <- dataFrameWithNAValues[,"var1"]
  
  test_that("givenWrongData",{
    expect_that(dfm$changeFactorToUnaryNumericType("",0),throws_error("This is not a factor!"))
    expect_that(dfm$changeFactorToUnaryNumericType(factorWithoutNAValues,""),throws_error("UnaryNumericValue is not a numeric value!"))
    
    numericVariable <- dataFrameNoNAValues[,"var3"]
    expect_that(dfm$changeFactorToUnaryNumericType(numericVariable,0),throws_error("This is not a factor!"))
  })
  
  test_that("givenOKData_returnConvertedFactor",{
    factorWithoutNAValues <- dataFrameNoNAValues[,"var1"]
    resultWithoutNAValues <- c(1,1,1,1)
    expect_that(dfm$changeFactorToUnaryNumericType(factorWithoutNAValues,1),is_identical_to(resultWithoutNAValues))
    
    factorWithNAValues <- dataFrameWithNAValues[,"var1"]
    resultWithNAValues <- c(1,NA,NA)
    expect_that(dfm$changeFactorToUnaryNumericType(factorWithNAValues,1),is_identical_to(resultWithNAValues))
  })
})

describe(" - when- addTotalRowAtTheBottomOfDataFrame",{
  context(" - when - addTotalRowAtTheBottomOfDataFrame")

  test_that("givenWrongData",{
    expect_that(dfm$addTotalRowAtTheBottomOfDataFrame(emptyDataFrame),is_identical_to(emptyDataFrame))
    expect_that(dfm$addTotalRowAtTheBottomOfDataFrame("emptyDataFrame"),throws_error("This is not a data frame!"))
  })
  
  test_that("givenOKData_returnDataFrameWithTotalRow",{
    resultDF <- data.frame(var1=c("value1","value1","value1","value2",NA), var2=c("value3","value3","value4","value4",NA), var3=c(1,2,3,4,10))
    expect_that(dfm$addTotalRowAtTheBottomOfDataFrame(dataFrameNoNAValues),is_identical_to(resultDF))
    
    resultDF <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("value3","value3","value4","value4"), var3=c(1,2,3,4))
    resultDF <- rbind(resultDF,data.frame(var1=c("Total"),var2=c(NA),var3=c(10)))
    expect_that(dfm$addTotalRowAtTheBottomOfDataFrame(dataFrameNoNAValues,TRUE),is_identical_to(resultDF))
    
    resultDF <- data.frame(var1=c("value1",NA,NA), var2=c(NA,"value4",NA), var3=c(1,2,NA))
    resultDF <- rbind(resultDF,data.frame(var1=c("Total"),var2=c(NA),var3=c(3)))
    expect_that(dfm$addTotalRowAtTheBottomOfDataFrame(dataFrameWithNAValues,TRUE),is_identical_to(resultDF))
  })
})

describe(" - when- addTotalColumnAtTheEndForColumns",{
  context(" - when - addTotalColumnAtTheEndForColumns")
  
  test_that("givenWrongData",{
    expect_that(dfm$addTotalColumnAtTheEndForColumns(emptyDataFrame,c("var1"),"Total"),is_identical_to(emptyDataFrame))
    expect_that(dfm$addTotalColumnAtTheEndForColumns("emptyDataFrame",c("var1"),"Total"),throws_error("This is not a data frame!"))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,c("var1"),-1),throws_error("This is not a valid column name! It should be a character!"))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,-1,"Total"),throws_error("columnListToSum is not a valid list! It should be a character list!"))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,"","Total"),throws_error("columnListToSum is not a valid list! It should be a character list!"))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,c(),"Total"),is_identical_to(dataFrameNoNAValues))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,c("var3"),"Total"),throws_error("'x' must be an array of at least two dimensions"))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,c("var3","wrongVarName"),"Total"),throws_error("undefined columns selected"))
  })
  
  test_that("givenOKData_returnDataFrameWithTotalColumn",{
    resultDF <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("value3","value3","value4","value4"), var3=c(1,2,3,4), var4=c(1,2,3,4), Total=c(2,4,6,8))
    dataFrameNoNAValues <- cbind(dataFrameNoNAValues,data.frame(var4=c(1,2,3,4)))
    expect_that(dfm$addTotalColumnAtTheEndForColumns(dataFrameNoNAValues,c("var3","var4"),"Total"),is_identical_to(resultDF))
  })
})

describe(" - when - addNewFactorLevel",{
  context(" - when - addNewFactorLevel")
  
  test_that("givenDataFrameAndNewFactorLevel_ThenGetDataFrameLevelsWithNewFactorLevel", {
    dataFrameWithNewLevel <- rbind(dataFrameNoNAValues,data.frame(var1="newValueType",var2="value3",var3=1))
    dataFrameLevelsResult <- levels(dataFrameWithNewLevel)
    
    dataFrameLevels <- levels(dfm$addNewFactorLevel(dataFrameNoNAValues,"var1","newFactorLevel"))
    
    expect_that(dataFrameLevels,is_identical_to(dataFrameLevelsResult))
  })
  
  test_that("givenDataFrameAndExistingFactorLevel_ThenGetDataFrameLevelsAsIs", {
    dataFrameLevelsResult <- levels(dataFrameNoNAValues)
    
    dataFrameLevels <- levels(dfm$addNewFactorLevel(dataFrameNoNAValues,"var1","newFactorLevel"))
    
    expect_that(dataFrameLevels,is_identical_to(dataFrameLevelsResult))
  })
})

describe(" - when - removeUnusedFactorLevels",{
  context(" - when - removeUnusedFactorLevels")
  
  test_that("givenDataFrameWithNoNeedToRemoveFactorLevels_thenReturnDataFrameAsIs",{
    expect_that(dfm$removeUnusedFactorLevels(dataFrameWithNAValues),is_identical_to(dataFrameWithNAValues))
    expect_that(levels(dfm$removeUnusedFactorLevels(dataFrameWithNAValues)),is_identical_to(levels(dataFrameWithNAValues)))
  })
  
  test_that("givenDataFrameWithNeedToRemoveFactorLevels_thenReturnDataFrameWithRemovedFactorLeves",{
    dataFrameWithNAValuesAndExtraLevels <- dataFrameWithNAValues[!is.na(dataFrameWithNAValues$var1),]
    resultDataFrameWithNAValuesAndNoExtraLevels <- data.frame(var1="value1",var2=NA,var3=1)
    resultDataFrameWithNAValuesAndNoExtraLevels$var2 <- as.factor(resultDataFrameWithNAValuesAndNoExtraLevels$var2)
    
    expect_that(dfm$removeUnusedFactorLevels(dataFrameWithNAValuesAndExtraLevels),is_identical_to(resultDataFrameWithNAValuesAndNoExtraLevels))
    expect_that(levels(dfm$removeUnusedFactorLevels(dataFrameWithNAValuesAndExtraLevels)),is_identical_to(levels(resultDataFrameWithNAValuesAndNoExtraLevels)))
  })
})

describe(" - when - resolveProblemWithLostSortPosibilityAndFactorLevelsSort",{
  test_that("givenWrongData_throwsErrorOrReturnEmptyDataFrame",{
    expect_that(dfm$resolveProblemWithLostSortPosibilityAndFactorLevelsSort(emptyDataFrame),is_identical_to(emptyDataFrame))
    expect_that(dfm$resolveProblemWithLostSortPosibilityAndFactorLevelsSort(""),throws_error("This is not a valid data frame!"))
  })
  
  test_that("givenOkData_thenRetrunOrderedDataFrame",{
    dataFrameNoNAValues <- rbind(dataFrameNoNAValues, data.frame(var1="a",var2="b",var3=0))
    resultDF <- data.frame(var1=c("value1","value1","value1","value2","a"), var2=c("value3","value3","value4","value4","b"), var3=c(1,2,3,4,0))
    expect_that(dfm$resolveProblemWithLostSortPosibilityAndFactorLevelsSort(dataFrameNoNAValues),is_identical_to(resultDF))
    
  })
})

describe(" - when - splitColumnInDataFrame",{
  context(" - when - splitColumnInDataFrame")
  
  test_that("givenWrongData_thenReturnDataFrameAsIs",{
    expect_that(dfm$splitColumnInDataFrame(dataFrameNoNAValues,"var1","nonExistingSeparaotr","var1_1","var1_2"),throws_error("Non of the values contains this separator!"))
    expect_that(dfm$splitColumnInDataFrame(dataFrameNoNAValues,"var1","ue","","var1_2"),throws_error("New var name is missing!"))
    expect_that(dfm$splitColumnInDataFrame(dataFrameNoNAValues,"var1","ue","var1_1",""),throws_error("New var name is missing!"))
  })
  
  test_that("givenDegenerativeCases_thenReturnDataFrameAsIs",{
    expect_that(dfm$splitColumnInDataFrame(emptyDataFrame,"var1","ue","var1_1","var1_2"),is_identical_to(emptyDataFrame))
    expect_that(dfm$splitColumnInDataFrame(dataFrameNoNAValues,"wrongVarName","ue","var1_1","var1_2"),is_identical_to(dataFrameNoNAValues))
  })
  
  
  test_that("givenDataFrameAndNameToSplit_thenReturnDataFrameWithSplitedColumnInTwo",{
    resultDF <- data.frame(var1=c("value1",NA,NA), var2=c(NA,"value4",NA), var3=c(1,2,NA), var1_1=c("val",NA,NA), var1_2=c("1",NA,NA))
    expect_that(dfm$splitColumnInDataFrame(dataFrameWithNAValues,"var1","ue","var1_1","var1_2"),is_identical_to(resultDF))
    
    resultDF2 <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("value3","value3","value4","value4"), var3=c(1,2,3,4), var1_1=c("val","val","val","val"), var1_2=c("1","1","1","2"))
    expect_that(dfm$splitColumnInDataFrame(dataFrameNoNAValues,"var1","ue","var1_1","var1_2"),is_identical_to(resultDF2))
    
    resultDF3 <- data.frame(var1=c("value1","value-1",NA), var2=c(NA,"value4",NA), var3=c(1,2,NA), var1_1=c("value1","val",NA), var1_2=c("value1","1",NA))
    dataFrameSomeValuesContainSeparator <- data.frame(var1=c("value1","value-1",NA), var2=c(NA,"value4",NA), var3=c(1,2,NA))
    expect_that(dfm$splitColumnInDataFrame(dataFrameSomeValuesContainSeparator,"var1","ue-","var1_1","var1_2"),is_identical_to(resultDF3))
  })
})

describe(" - when - changeColumnNameInDataFrame",{
  context(" - when - changeColumnNameInDataFrame")
  
  test_that("givenDataFrameAndSameOldNewVarName_ThenGetDataFrameAsIs", {
    expect_that(dfm$changeColumnNameInDataFrame(dataFrameNoNAValues,"var1","var1"),is_identical_to(dataFrameNoNAValues))
  })
  
  test_that("givenDataFrameAndNewVarName_ThenGetDataFrameWithNewName", {
    dataFrameOldName <- data.frame(oldVarName="value1",var2="value1")
    dataFrameNewName <- data.frame(newVarName="value1",var2="value1")
    expect_that(dfm$changeColumnNameInDataFrame(dataFrameOldName,"oldVarName","newVarName"),is_identical_to(dataFrameNewName))
  })
})

describe(" - when - isColumnNameInDataFrame/areColumnNamesInDataFrame",{
  context(" - when - isColumnNameInDataFrame/areColumnNamesInDataFrame")
  
  test_that("givenWrongData_ExpectFalse",{
    expect_false(dfm$isColumnNameInDataFrame(emptyDataFrame,""))
    expect_false(dfm$isColumnNameInDataFrame(dataFrameNoNAValues,"wrongVarName"))
    expect_false(dfm$areColumnNamesInDataFrame(dataFrameNoNAValues,c("var2","wrongVarName")))
  })
  
  test_that("givenOkData",{
    expect_true(dfm$isColumnNameInDataFrame(dataFrameNoNAValues,"var1"))
    expect_true(dfm$isColumnNameInDataFrame(dataFrameNoNAValues,"var2"))
    expect_true(dfm$areColumnNamesInDataFrame(dataFrameNoNAValues,c("var2","var1")))
  })
})

describe(" - when - orderColumnsAndValuesByNameInDataFrameList",{
  context(" - when - orderColumnsAndValuesByNameInDataFrameList")
  
  dataFrameList <- list()
  dataFrameListOrdered <- list()
  
  test_that("givenEmptyDataFrameList_ThenGetDataFrameLevelsAsIs", {
    expect_that(dfm$orderColumnsAndValuesByNameInDataFrameList(dataFrameList),is_identical_to(dataFrameList))
  })
  
  test_that("givenDataFrameListWithOneElementWithOrderedColumns_ThenGetDataFrameLevelsAsIs", {
    dataFrameList[[1]] <- dataFrameNoNAValues
    expect_that(dfm$orderColumnsAndValuesByNameInDataFrameList(dataFrameList),is_identical_to(dataFrameList))
  })
  
  test_that("givenDataFrameListWithMultipleElementsOrderedColumnsByName_ThenGetDataFrameLevelsAsIs", {
    dataFrameList[[1]] <- dataFrameNoNAValues
    dataFrameList[[2]] <- dataFrameWithNAValues
    
    expect_that(dfm$orderColumnsAndValuesByNameInDataFrameList(dataFrameList),is_identical_to(dataFrameList))
  })

  test_that("givenDataFrameListWithMultipleElementsNotOrderedColumnsByName_ThenGetDataFrameListOrderedByColumnNames", {
    dataFrameList[[1]] <- dataFrameNoNAValues[,c("var2","var1")]
    dataFrameList[[2]] <- dataFrameWithNAValues[,c("var2","var1")]
    
    dataFrameListOrdered[[1]] <- dataFrameNoNAValues[,c("var1","var2")]
    dataFrameListOrdered[[2]] <- dataFrameWithNAValues[,c("var1","var2")]
    
    expect_that(dfm$orderColumnsAndValuesByNameInDataFrameList(dataFrameList),is_identical_to(dataFrameListOrdered))
  })
  
  test_that("givenDataFrameListWithMultipleElementsNotOrderedColumnsByName_ThenGetDataFrameListOrderedByColumnNames", {
    dataFrameList[[1]] <- dataFrameNoNAValues[,c("var2","var1","var1","var2")]
    dataFrameList[[2]] <- dataFrameWithNAValues[,c("var2","var1","var2","var1")]
    
    dataFrameListOrdered[[1]] <- dataFrameNoNAValues[,c("var1","var1","var2","var2")]
    dataFrameListOrdered[[2]] <- dataFrameWithNAValues[,c("var1","var1","var2","var2")]
    
    expect_that(dfm$orderColumnsAndValuesByNameInDataFrameList(dataFrameList),is_identical_to(dataFrameListOrdered))
  })
})

describe(" - when - joinListOfDataFrames",{
  context(" - when - joinListOfDataFrames")
  dfList <- list()
  
  test_that("givenDegeneritveCases_thenReturnEmptyDataFrame",{
    dfList[[1]] <- emptyDataFrame
    expect_that(dfm$joinListOfDataFrames(dfList),is_identical_to(emptyDataFrame))
    
    dfList[[1]] <- dataFrameNoNAValues
    expect_that(dfm$joinListOfDataFrames(dfList),is_identical_to(dataFrameNoNAValues))
    expect_that(dfm$joinListOfDataFrames(dfList,"var1"),is_identical_to(dataFrameNoNAValues))
  })
  
  test_that("givenDataFrameListWAndNoJoinBy_thenReturnDataFrameWithAllDataFramesCombined",{
    dfList[[1]] <- dataFrameNoNAValues
    dfList[[2]] <- dataFrameNoNAValues
    expect_that(dfm$joinListOfDataFrames(dfList),is_identical_to(dataFrameNoNAValues))
    
    differentDataFrame <- dataFrameNoNAValues
    colnames(differentDataFrame) <- c("a","b","c")
    dfList[[2]] <- differentDataFrame
    resultJoinedDF <- data.frame(var1=c("value1","value1","value1","value2",NA,NA,NA,NA), var2=c("value3","value3","value4","value4",NA,NA,NA,NA), var3=c(1,2,3,4,NA,NA,NA,NA), a=c(NA,NA,NA,NA,"value1","value1","value1","value2"), b=c(NA,NA,NA,NA,"value3","value3","value4","value4"), c=c(NA,NA,NA,NA,1,2,3,4))
    expect_that(dfm$joinListOfDataFrames(dfList),is_identical_to(resultJoinedDF))
  })
  
  test_that("givenDataFrameListAndJoinByXVars_thenReturnDataFrameWithAllDataFramesCombined",{
    differentDataFrameSameSize <- dataFrameNoNAValues
    colnames(differentDataFrameSameSize) <- c("a","b","var3")
    dfList[[1]] <- dataFrameNoNAValues
    dfList[[2]] <- differentDataFrameSameSize
      
    test_that("same size DF",{
      resultJoinedDF <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("value3","value3","value4","value4"), var3=c(1,2,3,4), a=c("value1","value1","value1","value2"), b=c("value3","value3","value4","value4"))
      expect_that(dfm$joinListOfDataFrames(dfList,"var3"),is_identical_to(resultJoinedDF))
    })
    
    test_that("differet size DF",{
      differentSizeDataFrame <- rbind(differentDataFrameSameSize,data.frame(a="newVal",b="newVal2",var3=6))
      dfList[[2]] <- differentSizeDataFrame
      resultJoinedDF <- data.frame(var1=c("value1","value1","value1","value2",NA), var2=c("value3","value3","value4","value4",NA), var3=c(1,2,3,4,6), a=c("value1","value1","value1","value2","newVal"), b=c("value3","value3","value4","value4","newVal2"))
      
      expect_that(dfm$joinListOfDataFrames(dfList,"var3"),is_identical_to(resultJoinedDF))
    
      test_that("two combo vars",{
        colnames(differentSizeDataFrame) <- c("var1","b","var3")
        dfList[[2]] <- differentSizeDataFrame
        resultJoinedDF <- data.frame(var1=c("value1","value1","value1","value2","newVal"), var2=c("value3","value3","value4","value4",NA), var3=c(1,2,3,4,6), b=c("value3","value3","value4","value4","newVal2"))
    
        expect_that(dfm$joinListOfDataFrames(dfList,c("var1","var3")),is_identical_to(resultJoinedDF))
      })
    })
  })
})

describe(" - when - transponseDataFrame",{
  context(" - when - transponseDataFrame")
  test_that("givenDegenerativeCases",{
    expect_that(dfm$transponseDataFrame(emptyDataFrame),is_identical_to(emptyDataFrame))
    
    dataFrameWithOnecolumn <- data.frame(var1=c("val1","val2"))
    resultDFOneRow <- data.frame(OldColumns=c("var1"),V1=c("val1"),V2=c("val2"))
    expect_that(dfm$transponseDataFrame(dataFrameWithOnecolumn,FALSE),is_identical_to(resultDFOneRow))
    
    resultDFOnlyWithHeader <- data.frame(VarLevels=as.factor(c(NA)),val1=as.factor(c(NA)),val2=as.factor(c(NA)))
    resultDFOnlyWithHeader <- resultDFOnlyWithHeader[-1,]
    expect_that(dfm$transponseDataFrame(dataFrameWithOnecolumn),is_identical_to(resultDFOnlyWithHeader))
  })
  
  test_that("givenOkDataFrame_returnDataFrameTransponded", {
    resultDFFirstRowAsHeader <-  data.frame(VarLevels=c("var2","var3"), value1=c("value3",1), value1=c("value3",2), value1=c("value4",3), value2=c("value4",4))
    expect_that(dfm$transponseDataFrame(dataFrameNoNAValues),is_identical_to(resultDFFirstRowAsHeader))
    
    resultDFFirstRowNotAHeader <-  data.frame(OldColumns=c("var1","var2","var3"), V1=c("value1","value3",1), V2=c("value1","value3",2), V3=c("value1","value4",3), value2=c("value2","value4",4))
    expect_that(dfm$transponseDataFrame(dataFrameNoNAValues),is_identical_to(resultDFFirstRowAsHeader))
  })  
})

describe(" - when - createDataFrameWithNColumnsWithValue",{
  context(" - when - createDataFrameWithNColumnsWithValue")
  test_that("givenDegenerativeCases",{
    expect_that(dfm$createDataFrameWithNColumnsWithValue(0,0),is_identical_to(emptyDataFrame))
    expect_that(dfm$createDataFrameWithNColumnsWithValue(-1,0),is_identical_to(emptyDataFrame))
    
    noValuesDF <- data.frame(C1=NA)
    expect_that(dfm$createDataFrameWithNColumnsWithValue(1,NA),is_identical_to(noValuesDF))
  })
  
  test_that("givenOkData_getDataFrameWithOneRowAllValuesSame",{
    twoValuesDF <- data.frame(C1=0,C2=0)
    expect_that(dfm$createDataFrameWithNColumnsWithValue(2,0),is_identical_to(twoValuesDF))
    
    fiveValuesDF <- data.frame(C1="Value",C2="Value",C3="Value",C4="Value",C5="Value")
    expect_that(dfm$createDataFrameWithNColumnsWithValue(5,"Value"),is_identical_to(fiveValuesDF))
  })
})

describe(" - when - createNewRowForDataFrameWihValues",{
  context(" - when - createNewRowForDataFrameWihValues")
  test_that("givenDegenerativeCases",{
    expect_that(dfm$createNewRowForDataFrameWihValues(emptyDataFrame,0),is_identical_to(emptyDataFrame))
    noValuesDF <- data.frame(C1=NA)
    expect_that(dfm$createNewRowForDataFrameWihValues(noValuesDF,NA),is_identical_to(noValuesDF))
  })
  
  test_that("givenDataFrame_thenReturnOkRowForDataFrame",{
    newRowForDataFrameNoNAValues <- data.frame(var1=0,var2=0,var3=0)
    expect_that(dfm$createNewRowForDataFrameWihValues(dataFrameNoNAValues,0),is_identical_to(newRowForDataFrameNoNAValues))
    
    newRowForDataFrameNoNAValues <- data.frame(var1=NA,var2=NA,var3=NA)
    expect_that(dfm$createNewRowForDataFrameWihValues(dataFrameNoNAValues,NA),is_identical_to(newRowForDataFrameNoNAValues))
    
    newRowFordataFrameWithNAValues <- data.frame(var1="v1",var2="v1",var3="v1")
    expect_that(dfm$createNewRowForDataFrameWihValues(dataFrameWithNAValues,"v1"),is_identical_to(newRowFordataFrameWithNAValues))
  })
})

describe(" - when - addMissingaluesToColumnInDataFrame",{
  context(" - when - addMissingaluesToColumnInDataFrame")

  test_that("givenDegenerativeCases_thenReturnAppropreateError",{
    dataFrame <- data.frame(var1=c(1,2,3),var2=c("val1","val2","val3"))
    expect_that(dfm$addMissingaluesToColumnInDataFrame(emptyDataFrame,"var1"),is_identical_to(emptyDataFrame))
    expect_that(dfm$addMissingaluesToColumnInDataFrame(dataFrame,"var3"),throws_error("Variable name does not exist in dataFrame!"))
    expect_that(dfm$addMissingaluesToColumnInDataFrame(dataFrame,"var2"),throws_error("Variable sholud be numeric!"))
    
  })
  
  test_that("givenDataFrameAndVarName_thenReturnDataFrameWithFilledInValuesForVar",{
    dataFrame <- data.frame(var1=c(1,2,5),var2=c("val1","val2","val3"))
    resultDF <- data.frame(var1=c(1,2,3,4,5),var2=c("val1","val2","0","0","val3"))
    expect_that(dfm$addMissingaluesToColumnInDataFrame(dataFrame,"var1"),is_identical_to(resultDF))
  })
  
  test_that("givenDataFrameWithMultipleVarsAndVarName_thenReturnDataFrameWithFilledInValuesForVar",{
    dataFrame <- data.frame(var1=c(1,2,5),var2=c("val1","val2","val3"),var3=c("val1","val2","val3"),var4=c(1,1,1))
    resultDF <- data.frame(var1=c(1,2,3,4,5),var2=c("val1","val2","0","0","val3"),var3=c("val1","val2","0","0","val3"),var4=c(1,1,0,0,1))
    expect_that(dfm$addMissingaluesToColumnInDataFrame(dataFrame,"var1"),is_identical_to(resultDF))
  })
})

describe(" - when - moveColumnToTheEndOfDataFrame",{
  context(" - when - moveColumnToTheEndOfDataFrame")
  test_that("givenWrongData",{
    expect_that(dfm$moveColumnToTheEndOfDataFrame(emptyDataFrame,"var2"),is_identical_to(emptyDataFrame))
    expect_that(dfm$moveColumnToTheEndOfDataFrame(dataFrameNoNAValues,"wrongVarName"),is_identical_to(dataFrameNoNAValues))
    expect_that(dfm$moveColumnToTheEndOfDataFrame(dataFrameNoNAValues,-1),is_identical_to(dataFrameNoNAValues))
  })
  
  test_that("givenDegenerativeCases",{
    expect_that(dfm$moveColumnToTheEndOfDataFrame(emptyDataFrame,""),is_identical_to(emptyDataFrame))
    expect_that(dfm$moveColumnToTheEndOfDataFrame(dataFrameNoNAValues,"var3"),is_identical_to(dataFrameNoNAValues))
    
    dataFrameWithOneColumn <- data.frame(var1=c("value1","value1","value1","value2"))
    expect_that(dfm$moveColumnToTheEndOfDataFrame(dataFrameWithOneColumn,"var3"),is_identical_to(dataFrameWithOneColumn))
  })
  
  test_that("givenOkData_returnDataFrameWithGivenColumnAtTheEnd",{
    dataFrameNoNAValuesVar1LastColumn <- data.frame(var2=c("value3","value3","value4","value4"), var3=c(1,2,3,4), var1=c("value1","value1","value1","value2"))
    expect_that(dfm$moveColumnToTheEndOfDataFrame(dataFrameNoNAValues,"var1"),is_identical_to(dataFrameNoNAValuesVar1LastColumn))
    dataFrameNoNAValuesVar1LastColumn <- data.frame(var1=c("value1","value1","value1","value2"), var3=c(1,2,3,4), var2=c("value3","value3","value4","value4"))
    expect_that(dfm$moveColumnToTheEndOfDataFrame(dataFrameNoNAValues,"var2"),is_identical_to(dataFrameNoNAValuesVar1LastColumn))
  })
})

describe(" - when - createDataFrameOfPrintColumnValuesForColumns",{
  context(" - when - createDataFrameOfPrintColumnValuesForColumns")

  test_that("givenWrongData",{
    expect_that(dfm$createDataFrameOfPrintColumnValuesForColumns(emptyDataFrame,c(),""),is_identical_to(emptyDataFrame))
    expect_that(dfm$createDataFrameOfPrintColumnValuesForColumns(dataFrameWithNAValues,c(),"wrongVarName"),throws_error("printColumnName name does not exist in dataFrame!"))
    expect_that(dfm$createDataFrameOfPrintColumnValuesForColumns(dataFrameWithNAValues,c("wrongVarName"),"var3"),throws_error("One clumn name in the listOfInterestedColumns does not exist in dataFrame!"))
  })
  
  test_that("givenOkData",{
    resultDF <- data.frame(Name="var1",Values="1") 
    expect_that(dfm$createDataFrameOfPrintColumnValuesForColumns(dataFrameWithNAValues,c("var1"),"var3"),is_identical_to(resultDF))
    resultDF <- data.frame(Name=c("var1","var2"),Values=c("1","2")) 
    expect_that(dfm$createDataFrameOfPrintColumnValuesForColumns(dataFrameWithNAValues,c("var1","var2"),"var3"),is_identical_to(resultDF))
    resultDF <- data.frame(Name=c("var1","var2"),Values=c("NA","value4")) 
    expect_that(dfm$createDataFrameOfPrintColumnValuesForColumns(dataFrameWithNAValues,c("var1","var2"),"var2"),is_identical_to(resultDF))
    
  })
})