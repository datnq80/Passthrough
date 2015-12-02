LoadLEXCost <- function(costFilePath) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "LoadLEXCost"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  LEXCostRaw <- tryCatch({
    
    LEXCostRaw <- read.csv(LEXCostPath, 
                        col.names = c("month", "totalCost"),
                        colClasses = c("character", "numeric"))
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    LEXCostRaw
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "Done Mapping Non LEX Cost Data"), logger = reportName)
  })
  
  LEXCostRaw
}