setRefClass("GraphBuilderInterface", 
            fields = list(
              graphTitle = "character",
              xAxisTitle = "character",
              yAxisTitle = "character",
              legendTitle = "character",
              graphType = "character",
              xAxisLabelAngle = "numeric",
              facetWrapVariable= "ANY",
              chart = "ANY",
              dfm = "ANY"
            ),
            
            methods = list(
              initialize = function() {
                graphTitle <<- "Title"
                xAxisTitle <<- "xAxis" 
                yAxisTitle <<- "yAxis"
                xAxisLabelAngle <<- 0
                facetWrapVariable <<- NULL
                dfm <<- DataFrameMaipulator$new()
              },
              
              showDottedChart = function(yPerXData, xVarName, yVarName){
                stop("Not implemented showDottedChart!")
              },
              
              crateSomekindOfPerYearDottedChart = function(graphData, chartTitle, yVarName, yAxisTitle) {
                stop("Not implemented showDottedChart!")
              },
              
              showBarChart = function(mydata,var1Name,var2NamesList=c){
                stop("Not implemented showDottedChart!")
              },
              
              showChartSceleton = function() {
                stop("Not implemented showDottedChart!")       
              },
              
              showBoxPlot = function() {
                stop("Not implemented showBoxPlot!")       
              },
              
              showQQPlot = function() {
                stop("Not implemented showQQPlot!")       
              },
              
              showHistogram = function() {
                stop("Not implemented showHistogram!")       
              },
              
              showScatterPlot = function() {
                stop("Not implemented showScatterPlot!")       
              },
              
              showMeanComparisonGraph = function() {
                stop("Not implemented showMeanComparisonGraph!")       
              }
            )
)

#' @export GraphBuilderGGplot
GraphBuilderGGplot <- setRefClass("GraphBuilderGGplot", 
  contains = "GraphBuilderInterface",
  
  methods = list(
    showSomekindOfPerYearDottedChart = function(graphData, yVarName) {
      graphData[,"Year"] <- as.integer(as.character(graphData[,"Year"]))
      graphData <- dfm$addMissingaluesToColumnInDataFrame(graphData, "Year")
      graphData[,"Year"] <- as.numeric(as.character(unlist(graphData[,"Year"])))
      showDottedChart(graphData, "Year", yVarName)
    },
              
    showHistogram = function(dataFrame, varName, drawLine = TRUE, binwidth=30) {
		histogram <- ggplot(dataFrame, aes_string(varName)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = binwidth)
        if(drawLine)
			histogram <- histogram + stat_function(fun = dnorm, args = list(mean = mean(dataFrame[,varName], na.rm = TRUE), sd = sd(dataFrame[,varName], na.rm = TRUE)), colour = "red", size = 1)
		chart <<- histogram
		showChartSceleton()
        return(chart)
	},
    
	showScatterPlot = function(dataFrame, xVarName, yVarName, drawLine = TRUE){
		chart <<- showDottedChart(dataFrame, xVarName, yVarName)
		if(drawLine)
			chart <<- chart + geom_smooth(method = "lm", colour = "Red")
        return(chart)
	},
	
	showQQPlot = function(dataFrame,varName, qqLine=FALSE, qqBand=FALSE){
		qqPlot <- ggplot(dataFrame, aes_string(sample = varName)) + stat_qq()
		if(qqLine)
			qqPlot <- qqPlot + geom_abline(intercept=0, slope=1, colour = "Red")
		if(qqBand)
			qqPlot <- qqPlot + geom_qq_band(bandType = "boot",  alpha = 0.5, conf=0.95)
		chart <<- qqPlot
		showChartSceleton()
        return(chart)
	},
	
    showBoxPlot = function(dataFrame, xVarName, yVarName){
      BoxPlot <- ggplot(dataFrame, aes_string(xVarName, yVarName))
      chart <<- BoxPlot + geom_boxplot()
      showChartSceleton()
      return(chart)
    },
    
    showDottedChart = function(yPerXData, xVarName, yVarName){
      xVar <- yPerXData[,xVarName]
      yVar <- yPerXData[,yVarName]
      
      lineChart <- ggplot(yPerXData, aes(xVar,yVar))
      lineChart <- lineChart +  geom_point() 
      
      if(is.numeric(xVar)){
        lineChart <- lineChart + scale_x_continuous(breaks = pretty(xVar, n = 10)) 
      }else {
        #FOR NOW nothing
      }
      
      chart <<-lineChart
      showChartSceleton()
      return(chart)
    },
    
    showBarChart = function(mydata,var1Name,var2NamesList=c,facetWrapVariableName=NA){
      if(!is.na(facetWrapVariableName)){
        myDataStacked <- melt(mydata,id=c(var1Name,facetWrapVariableName),measured=var2NamesList)
      } else {
        myDataStacked <- melt(mydata,id=c(var1Name),measured=var2NamesList)
      }  
      
      myDataStacked <- dfm$changeColumnNameInDataFrame(myDataStacked,"variable","Legend")
      myDataStacked <- dfm$changeColumnNameInDataFrame(myDataStacked,"value","StackedColumn")

      if(length(var2NamesList)>0 & length(levels(myDataStacked[,"Legend"]))>1){
        barChart <- ggplot(myDataStacked, aes(myDataStacked[,var1Name],myDataStacked[,"StackedColumn"],
                                              fill=myDataStacked[,"Legend"]))
      } else {
        barChart <- ggplot(myDataStacked, aes(myDataStacked[,var1Name],myDataStacked[,"StackedColumn"]))
      }
      
      barChart <- barChart + geom_bar(stat="identity", width=0.5, position = position_dodge(width=0.6))
      chart <<- barChart
      showChartSceleton()
      return(chart)
    },
    
    showChartSceleton = function() {
      if(!is.null(facetWrapVariable))
        chart <<- chart + facet_wrap(facetWrapVariable)
      chart <<- chart + labs(x=xAxisTitle, y=yAxisTitle, fill=legendTitle,title=graphTitle,legend=legendTitle)
      if(xAxisLabelAngle != 0)
        chart <<- chart + theme(axis.text.x = element_text(angle = xAxisLabelAngle, hjust = 1))
      theme_update(plot.title = element_text(hjust = 0.5))     
    },
	
	showMeanComparisonGraph = function(dataFrame, xVarName, yVarName, legendVarName) {
	  meanComparisonGraph <- ggplot(dataFrame, aes_string(xVarName, yVarName, colour = legendVarName)) 
	    #scale_color_grey()
	    
	  chart <<- meanComparisonGraph + stat_summary(fun.y = mean, geom = "point") + 
	    stat_summary(fun.y = mean, geom = "line", aes_string(group= legendVarName)) +
	    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) 
	  showChartSceleton()
	  return(chart)
	}
  )
)

#' @export GraphBuilderBase
GraphBuilderBase <- setRefClass("GraphBuilderBase", 
  contains = "GraphBuilderInterface",                                 
                                
  methods = list(
    showDotterdChart = function(yPerXData, xVarName){
      xRange <- range(as.numeric(as.character(yPerXData[,xVarName])))
      freqRange <- range(yPerXData$freq)
      freqRange[1] <- 0 #include 0 on y axis
      
      yPerXData <- dfm$addMissingaluesToColumnInDataFrame(yPerXData, xRange)
      
      showChartSceleton(xRange,freqRange,graphTitle,xVarName,yAxisTitle)
      lines((as.numeric(as.character(yPerXData[,xVarName]))), yPerXData$freq, type="p")
    },
    
    showBarChart = function(mydata,var1Name,var2NamesList=c){
      mydataH <- as.matrix(mydata[,-1])
      mydataH <- t(mydataH)
      barplot(mydataH, names.arg = mydata[,1], beside = TRUE,col = rainbow(5))
      par(mar=c(5, 4, 4, 7), xpd=TRUE)
      legend("topright",legend=names(mydata[,-1]),title = legendTitle, fill=rainbow(5),inset= c(-0.2,0))
      showChartSceleton()
    },
    
    showChartSceleton = function() {
      par(mar=c(5, 4, 4, 8.1), xpd=FALSE)
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
      title(main = graphTitle, xlab = xAxisTitle, ylab = yAxisTitle)
    }
  )                             
)

