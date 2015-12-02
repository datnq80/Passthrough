GetLEXItemCost <- function(LEXCost_raw, OMS_Data, SKUDimWeight) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "GetLEXItemCost"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  LEXItemCost <- tryCatch({
    
    LEXTracking <- OMS_Data %>%
      filter(grepl("LEX", shipment_provider_name),
             tracking_number != "") %>%
      mutate(month=format(Shipped_Date, "%Y%m")) %>%
      select(tracking_number, month) %>%
      arrange(tracking_number)
    
    LEXTrackingCost <- left_join(LEXTracking, LEXCost_raw, by = "month") %>%
      filter(!is.na(totalCost) & totalCost > 0) %>%
      group_by(month) %>%
      mutate(packageCost = totalCost / n()) %>%
      ungroup() %>%
      select(-(totalCost))
    
    LEXItemCost <- LEXTrackingCost %>%
      left_join(OMS_Data, by = "tracking_number") %>%
      left_join(SKUDimWeight, by = "sku") %>%
      group_by(month, tracking_number) %>%
      # check dim weight data - only use dimweight cost distribution for full dimweight data
      # In case missing one item dimweight - cost is divided euqally to all item within packages
      mutate(dimWeightCheck = ifelse(any(is.na(weight) | weight == 0), "Failed", "Passed")) %>%
      mutate(itemCost = ifelse(dimWeightCheck == "Passed", packageCost * weight / sum(weight),
                               packageCost / n())) %>%
      ungroup()
    
    LEXItemCost %<>%
      rename(trackingNumber = tracking_number,
             packageNumber = package_number)
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    LEXItemCost
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "Done Mapping Non LEX Cost Data"), logger = reportName)
  })
  
  LEXItemCost
}