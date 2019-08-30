context("globalFunctions")
listm <- ListManipulator$new()
emptyDataFrame <- data.frame()
emptyList <- list()

describe(" - when - addElementToList",{
  context(" - when - addElementToList")
  
  test_that("givenDegenerativeCases",{
    expect_that(listm$addElementToList(emptyList,NULL),is_identical_to(emptyList))
    expect_that(listm$addElementToList("",NULL),throws_error("This is not a list!"))
    expect_that(listm$addElementToList(emptyDataFrame,NULL),throws_error("This is not a list!"))
  })
  
  test_that("givenOkData_returnListWithOneMoreElement",{
    listWithOneElement <- emptyList
    listWithOneElement[[1]] <- "1"
    expect_that(listm$addElementToList(emptyList,"1"),is_identical_to(listWithOneElement))
    
    listWithTwoElements <- listWithOneElement
    listWithTwoElements[[2]] <- emptyDataFrame
    expect_that(listm$addElementToList(listWithOneElement,emptyDataFrame),is_identical_to(listWithTwoElements))
  })
})