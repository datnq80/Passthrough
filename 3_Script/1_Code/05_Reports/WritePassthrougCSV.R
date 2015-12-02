WritePassthrougCSV <- function(passthroughData) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "WritePassthrougCSV"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  tryCatch({
    
    monthReport <- unique(passthroughData$month)
    
    for (iMonth in monthReport){
      write.csv(filter(passthroughData, month==iMonth), 
                file.path(outputFolder,
                          paste0(ventureShort,"_",iMonth,"_Passthrough_data.csv")),
                row.names = FALSE)
    }
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "ended"), logger = reportName)
  })
}