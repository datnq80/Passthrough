LoadSellerCharges <- function(sellerChargesPath) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "LoadSellerCharges"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  setClass('myDate')
  setAs("character","myDate", function(from) as.POSIXct(substr(from,  1,10),
                                                        format="%Y-%m-%d"))
  setClass("myInteger")
  setAs("character","myInteger", function(from) as.integer(gsub('"','',from)))
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('[",]','',from)))
  setClass("myTrackingNumber")
  setAs("character","myTrackingNumber", function(from) gsub('^0+','',toupper(trimws(from))))
  
  sellerCharges_raw <- tryCatch({
    
    sellerCharges <- data.frame(Seller_Name=character(),
                                tracking_number=character(),
                                package_number=character(),
                                Item_Number=numeric(),
                                Pickup_Date=as.POSIXct(character()),
                                Charges_VAT=numeric(),
                                Charges_Ex_VAT=numeric(),
                                VAT=numeric())
    
    for (file in list.files(sellerChargesPath)) {
      if(file_ext(file)=="csv"){
        currentFile <- tryCatch({
          currentFile <- read.csv(file.path(sellerChargesPath, file),
                                  stringsAsFactors = FALSE, row.names = NULL,
                                  quote = '"',
                                  col.names = c("Seller_Name","tracking_number","package_number",
                                                "Item_Number","Pickup_Date","Charges_VAT",
                                                "Charges_Ex_VAT","VAT"),
                                  colClasses = c("character","myTrackingNumber","myTrackingNumber",
                                                 "myNumeric","myDate","myNumeric",
                                                 "myNumeric","myNumeric"))
          
          for (iWarn in warnings()){
            logwarn(paste(functionName, file, iWarn), logger = reportName)
          }
          
          currentFile
        }, error = function(err) {
          logerror(paste(functionName, "Loop Loading CSV Files", file, err, sep = " - "), logger = consoleLog)
        }, finally = {
          loginfo(paste(functionName, "Done Loading File", file), logger = reportName)
        })
        
        sellerCharges <- rbind_list(sellerCharges,currentFile)
        
      }
    }
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    sellerCharges %<>% mutate(tracking_number = ifelse(tracking_number == "", "EmptyString",
                                                       tracking_number),
                              package_number = ifelse(package_number == "", tracking_number,
                                                      package_number))
    sellerCharges
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "ended"), logger = reportName)
  })
  
  sellerCharges_raw
}