LoadSKUDimWeight <- function(skuDimensionPath, dimWeightFactor){
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "LoadSKUDimWeight"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  skuDimWeightData <- tryCatch({
    
    setClass("myNumeric")
    setAs("character","myNumeric", 
          function(from) as.numeric(gsub('[",a-zA-Z]','',from)))
    skuDimWeightData <- data.frame(sku = character(),
                                   weight = numeric())
    for (file in list.files(skuDimensionPath)){
      if(file_ext(file)=="csv"){
        currentFileData <- read.csv(file.path(skuDimensionPath, file),
                                    col.names = c("sku", "product_weight",
                                                  "package_height", "package_length",
                                                  "package_width"),
                                    colClasses = c("character", "myNumeric",
                                                   "myNumeric", "myNumeric",
                                                   "myNumeric"))
        currentFileData %<>%
          mutate(dimWeight = package_width * package_height * package_length / 
                   dimWeightFactor) %>%
          mutate(weight = ifelse(is.na(dimWeight),
                                 ifelse(is.na(product_weight), NA,
                                        product_weight),
                                 ifelse(is.na(product_weight), dimWeight,
                                        ifelse(product_weight > dimWeight,
                                               product_weight, dimWeight))))
        currentFileData <- currentFileData %>%
          select(sku, weight)
        
        skuDimWeightData <- rbind_list(skuDimWeightData, currentFileData)
      }
    }
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    skuDimWeightData
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "ended"), logger = reportName)
  })
  
  skuDimWeightData
}