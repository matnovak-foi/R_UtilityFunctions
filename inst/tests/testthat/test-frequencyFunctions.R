context("FrequencyFunctions")
ftc <- FrequencyTableCreator$new()

emptyDataFrame <- data.frame()
noVariableName = "";

validDataFrame <- data.frame(var1=c("value1","value1","value1","value2"), var2=c("value3","value3","value4","value4"), var3=c("value5","value6","value6","value5"))
freqTableForValidDFVar1 <- data.frame(var1=c("value1","value2"),freq=as.integer(c(3,1)))
freqTableForValidDFVar1Var2 <- data.frame(var1=c("value1","value1","value2"),var2=c("value3","value4","value4"),freq=as.integer(c(2,1,1)))

validDataFrameWithNAValues <- data.frame(var1=c("value1","value1","value1","value2","value2",NA,NA), var2=c("value3","value3","value4","value4",NA,"value3",NA), var3=c("value5","value6","value6","value5",NA,NA,NA))
freqTableForValidDFwithNAValuesVar1 <- data.frame(var1=c("value1","value2",NA),freq=as.integer(c(3,2,2)))
freqTableForValidDFwithNAValuesVar1Var2 <- data.frame(var1=c("value1","value1","value2","value2",NA,NA),var2=c("value3","value4","value4",NA,"value3",NA),freq=as.integer(c(2,1,1,1,1,1)))


describe(" - when - createFrequencyTable",{
  context(" - when - createFrequencyTable")
  
  test_that("givenWrongInput_ThenReturnError", {
    emptyDataFrameWithColumns <- data.frame(var1="NA",var2="NA")
    emptyDataFrameWithColumns <- emptyDataFrameWithColumns[-1,]
    
    expect_that(ftc$createFrequencyTable(emptyDataFrame,"var1"),is_identical_to(emptyDataFrame))
    expect_that(ftc$createFrequencyTable(emptyDataFrameWithColumns,"var1"),is_identical_to(emptyDataFrameWithColumns))
    expect_that(ftc$createFrequencyTable(validDataFrame,noVariableName),throws_error("No valid data!"))
    expect_that(ftc$createFrequencyTable(validDataFrame,"wrongVar"),throws_error("object '\\w+' not found"))
    expect_that(ftc$createFrequencyTable(validDataFrame,c("var1","wrongVar")),throws_error("object '\\w+' not found"))
  }) 
  
  test_that("givenValidDataFrame_ThenGetCalculatedFreqForSelectedVars", {
    expect_that(ftc$createFrequencyTable(validDataFrame,"var1"),is_identical_to(freqTableForValidDFVar1))
    expect_that(ftc$createFrequencyTable(validDataFrame,c("var1","var2")),is_identical_to(freqTableForValidDFVar1Var2))
    expect_that(ftc$createFrequencyTable(validDataFrameWithNAValues,"var1"),is_identical_to(freqTableForValidDFwithNAValuesVar1))
    expect_that(ftc$createFrequencyTable(validDataFrameWithNAValues,c("var1","var2")),is_identical_to(freqTableForValidDFwithNAValuesVar1Var2))
    expect_that(ftc$createFreqencyTableWithoutNAValues(validDataFrameWithNAValues,c("var1","var2")),is_identical_to(freqTableForValidDFVar1Var2))
  }) 
  
  test_that("givenValidDataFrameWithWeights_ThenGetCalculatedFreqWithWeightsForSelectedVars", {
    dataFrameWithWeights <- validDataFrame
    dataFrameWithWeights$weightsVar <- c(1.2,2.2,3,4)
    freqTableForValidDFWithWeight <- data.frame(var1=c("value1","value2"),freq=c(6.4,4))
    
    dataFrameWithWeightVarNameFreq <- validDataFrame
    dataFrameWithWeightVarNameFreq$freq <- c(1.2,2.2,3,4)
    
    dataFrameWithWeightsAndVarNamedFreq <- dataFrameWithWeights
    dataFrameWithWeightsAndVarNamedFreq$freq <- c(2,2,2,2)
    
    expect_that(ftc$createFrequencyTable(dataFrameWithWeights,"var1","weightsVar"),is_identical_to(freqTableForValidDFWithWeight))
    expect_that(ftc$createFrequencyTable(dataFrameWithWeightVarNameFreq,"var1","freq"),is_identical_to(freqTableForValidDFWithWeight))
    expect_that(ftc$createFrequencyTable(dataFrameWithWeightsAndVarNamedFreq,"var1","weightsVar"),is_identical_to(freqTableForValidDFWithWeight))
  
    test_that("Two Var Names One called Freq Not Used As Weightening Var", {
      expect_that(ftc$createFrequencyTable(dataFrameWithWeightVarNameFreq,"var1"),is_identical_to(freqTableForValidDFVar1))
    }) 
  }) 
})

describe(" - when - createFrequencyTableForUnaryValueVariable",{
  context(" - when - createFrequencyTableForUnaryValueVariable")
  
  test_that("givenDataFrameWithOneVarUnary_ThenGetCalculatedFreqWithViceVersaTextForNAValues", {
    dataFrameWithOneValueVar <- validDataFrameWithNAValues[is.na(validDataFrameWithNAValues$var1) | validDataFrameWithNAValues$var1=="value1",]
    resultDataFrame <- data.frame(Name=c("var1","viceVersaText"),Count=as.integer(c(3,2)))
    
    expect_that(ftc$createFrequencyTableForUnaryValueVariable(dataFrameWithOneValueVar,"var1","viceVersaText"),is_identical_to(resultDataFrame))
  }) 
  
  test_that("givenDataFrameWithOneVarUnaryInteger_ThenGetCalculatedFreqWithViceVersaTextForNAValues", {
    dataFrameWithOneValueVar <- data.frame(var1=c(1,NA,1,NA,1))
    resultDataFrame <- data.frame(Name=c("var1","viceVersaText"),Count=as.integer(c(3,2)))
    
    expect_that(ftc$createFrequencyTableForUnaryValueVariable(dataFrameWithOneValueVar,"var1","viceVersaText"),is_identical_to(resultDataFrame))
  }) 
})

describe(" - when - PRIVATE_createFrequencyTableList",{
  context(" - when - PRIVATE_createFrequencyTableList")
  resultDataFrameList  <- list() 
  
  
  test_that("givenWrongData_ThenThrowError", {
    expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,noVariableName),throws_error("No valid data!"))
    expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame, list("wrongVarName")),throws_error("object '\\w+' not found"))
  }) 
  
  test_that("givenDegenerativeCases_ThenReturnEmptyList", {
    expect_that(ftc$PRIVATE_createFrequencyTableList(emptyDataFrame,list()),is_identical_to(list()))
    expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame, list()),is_identical_to(list()))
    
    test_that("givenOneVarNameAllWithAllNAValuesAndUsingcreateFreqencyTableWithoutNAValues_ThenGetListWithOneFreqTable", {
      allNADataFrame <- data.frame(var1=c(NA,NA,NA))
      resultDataFrameList[[1]] <- data.frame(var1=c(NA),freq=as.integer(c(NA)))
      resultDataFrameList[[1]] <- resultDataFrameList[[1]][!is.na(resultDataFrameList[[1]]),]
      
      expect_that(ftc$PRIVATE_createFrequencyTableList(allNADataFrame,list("var1"),list(),ftc$createFreqencyTableWithoutNAValues),is_identical_to(resultDataFrameList))
    }) 
    
    test_that("givenDataFrameWithOneVarNameBeforeFreq_ThenGetListWithOneFreqTable", {
      resultDataFrameList[[1]]<- data.frame(aVar1=c("value1","value2"),freq=as.integer(c(3,1)))
      validDataFrameNewNames <- validDataFrame
      colnames(validDataFrameNewNames)<-c("aVar1","aVar2","aVar3")
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrameNewNames,"aVar1"),is_identical_to(resultDataFrameList))
    }) 
  }) 
  
  test_that("givenDataFrameAndXNumberOfVarsComboWithYNumberOfVars_ThenGetListWithFreqTablesCombiningAllXVarsWithYVars", {
    freqTableForValidDFVar2 <- data.frame(var2=c("value3","value4"),freq=as.integer(c(2,2)))
   
    resultDataFrameList[[1]] <- freqTableForValidDFVar1
    test_that("cases with one var",{
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,"var1"),is_identical_to(resultDataFrameList))
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,"var1","var1"),is_identical_to(resultDataFrameList))
      
      resultDataFrameList[[1]]<- freqTableForValidDFVar1Var2
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,"var2","var1"),is_identical_to(resultDataFrameList))
    })
    
    resultDataFrameList[[2]] <- freqTableForValidDFVar1Var2
    test_that("cases with two vars",{
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,c("var1","var2"),"var1"),is_identical_to(resultDataFrameList))
      
      resultDataFrameList[[2]]<- freqTableForValidDFVar2
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,c("var1","var2")),is_identical_to(resultDataFrameList))
    })
    
    test_that("cases with three vars",{
      freqTableForValidDFVar1Var3 <- data.frame(var1=c("value1","value1","value2"),var3=c("value5","value6","value5"),freq=as.integer(c(1,2,1)))
      freqTableForValidDFVar2Var3 <- data.frame(var2=c("value3","value3","value4","value4"),var3=c("value5","value6","value5","value6"),freq=as.integer(c(1,1,1,1)))
      freqTableForValidDFVar1Var2Var3 <- data.frame(var1=c("value1","value1","value1","value2"),var2=c("value3","value3","value4","value4"),var3=c("value5","value6","value6","value5"),freq=as.integer(c(1,1,1,1)))
      
      resultDataFrameList[[3]]<- freqTableForValidDFVar1Var3
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,c("var1","var2","var3"),c("var1")),is_identical_to(resultDataFrameList))
      
      resultDataFrameList[[3]]<- freqTableForValidDFVar2
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,c("var1","var2"),c("var1","var2")),is_identical_to(resultDataFrameList))
      
      resultDataFrameList[[4]]<- freqTableForValidDFVar1Var3
      resultDataFrameList[[5]]<- freqTableForValidDFVar2Var3
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,c("var1","var2","var3"),c("var1","var2")),is_identical_to(resultDataFrameList))
      
      resultDataFrameList[[6]]<- freqTableForValidDFVar1Var2Var3
      expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrame,list("var1","var2","var3"),list("var1","var2",c("var1","var2"))),is_identical_to(resultDataFrameList))
      
      test_that("givenDataFramesUsingcreateFreqencyTableWithoutNAValues_ThenGetListWithFreqTablesCombiningAllXVarsWithYVarsWithoutNAValues", {
        freqTableForValidDFwithNAValuesVar2 <- data.frame(var1=c("value3","value4",NA),freq=as.integer(c(3,2,2)))
        freqTableForValidDFwithoutNaValuesVar1 <- data.frame(var1=c("value1","value2"),freq=as.integer(c(3,2)))
        freqTableForValidDFwithoutNAValuesVar2 <- data.frame(var2=c("value3","value4"),freq=as.integer(c(3,2)))
        
        resultDataFrameList[[1]]<- freqTableForValidDFwithoutNaValuesVar1
        resultDataFrameList[[2]]<- freqTableForValidDFVar1Var2
        resultDataFrameList[[3]]<- freqTableForValidDFwithoutNAValuesVar2
        resultDataFrameList[[4]]<- freqTableForValidDFVar1Var3
        resultDataFrameList[[5]]<- freqTableForValidDFVar2Var3
        resultDataFrameList[[6]]<- freqTableForValidDFVar1Var2Var3
        
        expect_that(ftc$PRIVATE_createFrequencyTableList(validDataFrameWithNAValues,list("var1","var2","var3"),list("var1","var2",c("var1","var2")),ftc$createFreqencyTableWithoutNAValues),is_identical_to(resultDataFrameList))
      }) 
    })
    
    
  }) 
})

describe(" - when - PRIVATE_createFrequencyTableForListOfVars",{
  context(" - when - PRIVATE_createFrequencyTableForListOfVars")
  
  test_that("givenEmptyDataFrame_thenReturnEmptyList",{
    expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(emptyDataFrame,c()),is_identical_to(emptyDataFrame))
  })
  
  test_that("givenWrongData_thenThrowError", {
    expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(validDataFrame,noVariableName),throws_error("varNameList is not a valid list! It should be a character list!"))
    expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(validDataFrameWithUnaryValues,list("var1",c("var1","var2"))),throws_error("This is not a valid list forvarNameList! It should be a character list!"))
  })
  
  test_that("givenValidDataFrameWithXVars_thenReturnFreqTableWithValuesTypes", {
    resultDF <- data.frame(VarTypes=c("value1","value2"),var1=as.integer(c(3,2)))
    expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(validDataFrameWithNAValues,"var1"),is_identical_to(resultDF))
    
    resultDF <- data.frame(VarTypes=c("value1","value2","value3","value4"),var1=as.integer(c(3,2,NA,NA)),var2=as.integer(c(NA,NA,3,2)))
    expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(validDataFrameWithNAValues,c("var1","var2")),is_identical_to(resultDF))
      
    resultDF <- data.frame(VarTypes=c("val1","val2"),var1=as.integer(c(2,1)),var2=as.integer(c(1,2)))
    dataFrameSameValues <- data.frame(var1=c("val1","val1","val2",NA),var2=c("val1","val2",NA,"val2"))
    expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(dataFrameSameValues,c("var1","var2")),is_identical_to(resultDF))
    
    test_that("givenValidDataFrameWithUnaryValuesAndXVars_thenReturnFreqTableForEachValue", {
      validDataFrameWithUnaryValues <- data.frame(var1=c("value1","value1","value1",NA,NA,"value1"), var2=c("value2","value2",NA,"value2",NA,"value2"))
      
      resultDF <- data.frame(VarTypes=c("value1"),var1=as.integer(c(4)))
      expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(validDataFrameWithUnaryValues,"var1"),is_identical_to(resultDF))
      
      resultDF <- data.frame(VarTypes=c("value1","value2"),var1=as.integer(c(4,NA)),var2=as.integer(c(NA,4)))
      expect_that(ftc$PRIVATE_createFrequencyTableForListOfVars(validDataFrameWithUnaryValues,c("var1","var2")),is_identical_to(resultDF))
    })
  })
})

describe(" - when - createFrequencyTableForListOfVarsInComboWithVars",{
  context(" - when - createFrequencyTableForListOfVarsInComboWithVars")
  validDataFrameWithUnaryValues <- data.frame(var1=c("value1","value1","value1",NA,NA,"value1"), var2=c("value2","value2",NA,"value2",NA,"value2"), var3=c("value3",NA,"value3","value3",NA,"value3"), comboVar1=c(1,1,2,3,4,NA), comboVar2=c(15,14,13,12,11,NA))
  
  test_that("givenEmptyDataFrame_thenReturnEmptyList",{
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(emptyDataFrame,c(),c()),is_identical_to(emptyDataFrame))
  })
  
  test_that("givenWrongData_thenThrowError", {
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrame,noVariableName,c("var1")),throws_error("varNameList is not a valid list! It should be a character list!"))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrame,c("var1"),noVariableName),throws_error("varNamesToCombineWith is not a valid list! It should be a character list!"))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithUnaryValues,c("var1","var2"),list("comboVar1",c("comboVar1","comboVar2"))),throws_error("This is not a valid list forvarNamesToCombineWith! It should be a character list!"))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithUnaryValues,list("var1",c("var1","var2")),c("comboVar1","comboVar2")),throws_error("This is not a valid list forvarNameList! It should be a character list!"))
    
    test_that("givenComboVarThatIsAlsoTheVarInTheList_throwError",{
      expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrame, c("var1","var2"),"var1"),throws_error("Combovar can not be also the var in the list!"))
      expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrame, c("var1","var2"),c("var1","var2")),throws_error("Combovar can not be also the var in the list!"))
    })
  })
  
  test_that("givenValidDataFrameWithUnaryValuesAndXVarsAndYComboVars_thenReturnFreqTableWithYComboVarsAndXVars", {
    resultDF <- data.frame(comboVar1=c(1,2),var1=c(2,1))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithUnaryValues,"var1","comboVar1"),is_identical_to(resultDF))
  
    resultDF <- data.frame(comboVar1=c(1,2,3),var1=c(2,1,NA),var2=c(2,NA,1))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithUnaryValues,c("var1","var2"),"comboVar1"),is_identical_to(resultDF))
    
    validDataFrameWithUnaryValuesNotSorted <- data.frame(var1=c("value1",NA,NA,"value1",NA,NA), var2=c(NA,"value2",NA,"value2","value2","value2"), comboVar1=c("v2","v3","v4",NA,"v1","v1"))
    resultDF <- data.frame(comboVar1=c("v1","v2","v3"),var1=c(NA,1,NA),var2=c(2,NA,1))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithUnaryValuesNotSorted,c("var1","var2"),"comboVar1"),is_identical_to(resultDF))
    
    
    resultDF <- data.frame(comboVar1=c(1,1,2,3),comboVar2=c(14,15,13,12),var1=c(1,1,1,NA),var2=c(1,1,NA,1))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithUnaryValues,c("var1","var2"),c("comboVar1","comboVar2")),is_identical_to(resultDF))
  })
  
  test_that("givenValidDataFrameWithoutUnaryValuesWithXVarsAndOneOrNoneComboVar_thenReturnFreqTableWithValuesTypesOfComboVar", {
    resultDF <- data.frame(var2=c("value3","value4"),var1=c(2,2))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithNAValues,"var1","var2"),is_identical_to(resultDF))
    
    resultDF <- data.frame(var2=c("value3","value4"),var1=c(2,2),var3=c(2,2))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVars(validDataFrameWithNAValues,c("var1","var3"),"var2"),is_identical_to(resultDF))
  })
})

describe(" - when - createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency",{
  context(" - when - createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency")
  validDataFrameWithUnaryValues <- data.frame(var1=c("value1","value1","value1",NA,NA,"value1"), var2=c("value2","value2",NA,"value2",NA,"value2"), var3=c("value3",NA,"value3","value3",NA,"value3"), comboVar1=c(1,1,2,3,4,NA), comboVar2=c(15,14,13,12,11,NA))
  
  test_that("givenEmptyDataFrame_thenReturnEmptyList",{
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(emptyDataFrame,c(),noVariableName,""),is_identical_to(emptyDataFrame))
  })
  
  test_that("givenWrongData_thenThrowError", {
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(validDataFrameWithNAValues,c("var1","var3"),"var2",-1),throws_error("Title must be a character and can not be empty!"))
  })
  
  test_that("givenValidDataFrameWithUnaryValuesAndXVarsAndYComboVars_thenReturnFreqTableWithYComboVarsAndXVars", {
    resultDF <- data.frame(comboVar1=c(1,2,3,4),var1=c(2,1,NA,NA),Total=as.integer(c(2,1,1,1)))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(validDataFrameWithUnaryValues,"var1","comboVar1","Total"),is_identical_to(resultDF))
    
    resultDF <- data.frame(comboVar1=c(1,2,3,4),var1=c(2,1,NA,NA),var2=c(2,NA,1,NA),Total=as.integer(c(2,1,1,1)))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(validDataFrameWithUnaryValues,c("var1","var2"),"comboVar1","Total"),is_identical_to(resultDF))
    
    resultDF <- data.frame(comboVar1=c(1,1,2,3,4),comboVar2=c(14,15,13,12,11),var1=c(1,1,1,NA,NA),var2=c(1,1,NA,1,NA),Total=as.integer(c(1,1,1,1,1)))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(validDataFrameWithUnaryValues,c("var1","var2"),c("comboVar1","comboVar2"),"Total"),is_identical_to(resultDF))
  })
  
  test_that("givenValidDataFrameWithoutUnaryValuesWithXVarsAndOneOrNoneComboVar_thenReturnFreqTableWithValuesTypesOfComboVar", {
    resultDF <- data.frame(var2=c("value3","value4"),var1=c(2,2),Total=as.integer(c(3,2)))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(validDataFrameWithNAValues,"var1","var2","Total"),is_identical_to(resultDF))
    
    resultDF <- data.frame(var2=c("value3","value4"),var1=c(2,2),var3=c(2,2),Total=as.integer(c(3,2)))
    expect_that(ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(validDataFrameWithNAValues,c("var1","var3"),"var2","Total"),is_identical_to(resultDF))
  })
  
})

describe(" - when - createFrequencyTableForListOfVarsByVarValues",{
  context(" - when - createFrequencyTableForListOfVarsByVarValues")
 
  test_that("givenValidDataFrameWithTwoVarsSameValues_thenReturnFreqTableWithValuesTypesInColumns", {
    resultDF <- data.frame(someName=c("var1","var2"),val1=c(2,1),val2=c(1,2))
    dataFrameSameValues <- data.frame(var1=c("val1","val1","val2",NA),var2=c("val1","val2",NA,"val2"))
    
    expect_that(ftc$createFrequencyTableForListOfVarsByVarValues(dataFrameSameValues,c("var1","var2"),"someName"),is_identical_to(resultDF))
  })

  test_that("givenValidDataFrameWithTwoVarsOneValue_thenReturnFreqTableWithValuesTypesInColumns", {
    resultDF <- data.frame(someName=c("var1","var2"),val1=c(3,3))
    dataFrameSameValues <- data.frame(var1=c("val1","val1","val1",NA),var2=c("val1","val1",NA,"val1"))
    expect_that(ftc$createFrequencyTableForListOfVarsByVarValues(dataFrameSameValues,c("var1","var2"),"someName"),is_identical_to(resultDF))
    
    resultDF <- data.frame(someName=c("var1","var2"),X1=c(3,3))
    dataFrameSameValues <- data.frame(var1=c(1,1,1,NA),var2=c(1,1,NA,1))
    expect_that(ftc$createFrequencyTableForListOfVarsByVarValues(dataFrameSameValues,c("var1","var2"),"someName"),is_identical_to(resultDF))
  })
})

describe(" - when - createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars",{
  context(" - when - createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars")
  dataFrameUnaryValues <- data.frame(var1=c("val1","val1","val1",NA),var2=c("val1","val1",NA,"val1"), comboVar1=c(1,1,NA,1), comboVar2=c(1,NA,NA,1))
  
  
  test_that("givenDegenerativeCasesOrWrongData_returnErrorOrApropreateValue",{
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(emptyDataFrame, c(), "commonName", c()),is_identical_to(emptyDataFrame))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars("noDataFrame", c(), "commonName", c()),throws_error("dataFrame is no a data frame!"))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(validDataFrame, "", "commonName", c()),throws_error("varNameList or varNamesToCombineWith contains columns names that are not avalible in given dataFrame"))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(validDataFrame, c(), "commonName", c()),throws_error("No varNames given!"))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(validDataFrame, c("var1"), c(), c()),throws_error("replacement has length zero"))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(validDataFrame, c("var1"), "commonName", ""),throws_error("varNameList or varNamesToCombineWith contains columns names that are not avalible in given dataFrame"))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(validDataFrame, "var1", "commonName", c()),throws_error("dataFrameShould only cotain one value in all vars given in varNameList!"))
    
    resultDF <- data.frame(commonName=c("var1","var2"),val1=c(3,3))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues,c("var1","var2"),"commonName",c()),is_identical_to(resultDF))
    
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues, "wrongVarName", "commonName", c()),throws_error("varNameList or varNamesToCombineWith contains columns names that are not avalible in given dataFrame"))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues, c("var1"), "commonName", c("wrongVarName")),throws_error("varNameList or varNamesToCombineWith contains columns names that are not avalible in given dataFrame"))
  })
  
  test_that("givenOkAllData_ReturnFreqTableWithListOfVarsByVars",{
    resultDF <- data.frame(commonName=c("var1","var2"),All=c(3,3),comboVar1=c(2,3))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues,c("var1","var2"),"commonName","comboVar1"),is_identical_to(resultDF))
    
    resultDF <- data.frame(commonName=c("var1","var2"),All=c(3,3),comboVar1=c(2,3),comboVar2=c(1,2))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues,c("var1","var2"),"commonName",c("comboVar1","comboVar2")),is_identical_to(resultDF))
    
    #print(createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues,c("var1","var2"),"commonName",list("comboVar1","comboVar2",c("comboVar1","comboVar2"))))
    resultDF <- data.frame(commonName=c("var1","var2"),All=c(3,3),comboVar1=c(2,3),comboVar2=c(1,2),comboVar1comboVar2=c(1,2))
    expect_that(ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(dataFrameUnaryValues,c("var1","var2"),"commonName",list("comboVar1","comboVar2",c("comboVar1","comboVar2"))),is_identical_to(resultDF))
    
  })
})