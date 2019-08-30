context("perYearAnalysis")
ya <- YearAnalizator$new()

emptyDataFrame <- data.frame()

describe(" - when - calculateMinAndMaxYearUsedByVariable",{
  context(" - when - calculateMinAndMaxYearUsedByVariable")
  dataFrameToolsYear <- data.frame(Year=c("2000","2001","2002","2004","2005"),MOSS=c(1,2,NA,3,2))
  
  test_that("givenWrongData_throwsErrorOrReturnEmptyDataFrame",{
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(emptyDataFrame,"var1"),is_identical_to(emptyDataFrame))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable("","var1"),throws_error("This is not a valid data frame!"))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(dataFrameToolsYear,1),throws_error("One or more column name does not exist in data frame!"))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(dataFrameToolsYear,"wrongVarName"),throws_error("One or more column name does not exist in data frame!"))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(dataFrameToolsYear,c("MOSS","wrongVarName")),throws_error("One or more column name does not exist in data frame!"))
  })

  test_that("givenOkDataFrameAndListOfVars_returnTableWithMinAndMaxYearThatWasUsedForEachVar",{
    
    resultDF <- data.frame(Tool=c("MOSS"),lastYear=c(2005),firstYear=c(2000))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(dataFrameToolsYear,"MOSS","Tool"),is_identical_to(resultDF))
    
    dataFrameToolsYear <- data.frame(Year=c("2000","2001","2002","2004","2005"),MOSS=c(1,2,NA,3,2), jPlag=c(NA,1,1,1,1),SIM=c(1,NA,NA,NA,NA))
    resultDF <- data.frame(Tool=c("SIM","MOSS","jPlag"),lastYear=c(2000,2005,2005),firstYear=c(2000,2000,2001))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(dataFrameToolsYear,c("MOSS","jPlag","SIM"),"Tool"),is_identical_to(resultDF))
    
    dataFrameToolsYear <- data.frame(Year=c("2000","2001","2002","2004","2005"),MOSS=c(1,2,NA,3,2), jPlag=c(NA,1,1,1,1),SIM=c(NA,NA,NA,NA,NA))
    resultDF <- data.frame(Tool=c("MOSS","jPlag","SIM"),lastYear=c(2005,2005,NA),firstYear=c(2000,2001,NA))
    expect_that(ya$calculateMinAndMaxYearUsedByVariable(dataFrameToolsYear,c("MOSS","jPlag","SIM"),"Tool"),is_identical_to(resultDF))
  })
  
})