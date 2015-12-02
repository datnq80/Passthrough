LoadNonLEXCostData <- function(costFilePath) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
  })
  
  functionName <- "LoadNonLEXCostData"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  setClass('myDate')
  setAs("character","myDate", function(from) as.POSIXct(substr(from,1,10), format="%Y-%m-%d")) #covnert datetime string to datetime
  setClass("myInteger")
  setAs("character","myInteger", function(from) as.integer(gsub('"','',trimws(from))))
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('[",]','',trimws(from))))
  setClass("myTrackingNumber")
  setAs("character","myTrackingNumber", function(from) toupper((gsub('^0+','',trimws(from)))))
  
  costData <- data.frame(Delivery_Company = character(),
                         tracking_number = character(),
                         package_number = character(),
                         Pickup_Date = as.POSIXct(character()),
                         Cost_VAT = numeric(),
                         Cost_Ex_VAT = numeric(),
                         VAT = numeric(),
                         Month = character())
  
  fileList <- list.files(costFilePath)
  fileList <- fileList[grepl(".csv", fileList)]
  pb <- txtProgressBar(min=0,max=length(fileList) + 1, style = 3)
  iProgress <- 0
  setTxtProgressBar(pb, iProgress)
  
  for (file in fileList){
    if(file_ext(file) == "csv"){
      currentFile <- tryCatch({
        currentFile <- read.csv(file.path(costFilePath, file),
                                stringsAsFactors = FALSE, row.names = NULL,
                                quote = '"',
                                col.names = c("deliveryCompany", "trackingNumber",
                                              "packageNumber", "pickupDate", "costVAT",
                                              "costEXVAT", "VAT", "month"),
                                colClasses = c("character", "myTrackingNumber",
                                               "character", "myDate", "myNumeric",
                                               "myNumeric", "myNumeric", "character"))
        
        iProgress <- iProgress + 1
        setTxtProgressBar(pb, iProgress)
        
        for (iWarn in warnings()){
          logwarn(paste(functionName, file, iWarn), logger = reportName)
        }
        
        currentFile
      }, error = function(err) {
        logerror(paste(functionName, "Loop Loading CSV Files", file, err, sep = " - "), logger = consoleLog)
      }, finally = {
        loginfo(paste(functionName, "Done Loading File", file), logger = reportName)
      })

      costData <- rbind_list(costData,currentFile)
    }
  }
  
  loginfo(paste(functionName, "Done Loading CSV Data"), logger = consoleLog)
  
  costData %<>%
    group_by(month, trackingNumber, packageNumber) %>%
    summarize(packageCost = sum(costEXVAT, na.rm = TRUE)) %>%
    ungroup()
  
  iProgress <- iProgress + 1
  setTxtProgressBar(pb, iProgress)
  
  functionName <- "LoadNonLEXCostData"
  loginfo(paste("Function", functionName, "ended"), logger = reportName)
  
  costData
}