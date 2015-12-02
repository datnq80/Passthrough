GetItemCostOMSData <- function(nonLEXCostData_raw, OMS_Data, SKUDimWeight) {
  
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "GetItemCostOMSData"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  itemCostData <- tryCatch({
    nonLEXCostData <- nonLEXCostData_raw
    
    # Replace empty string TrackingNumber/PackageNumber String with "EmptyString"
    nonLEXCostData %<>%
      mutate(trackingNumber = ifelse(trackingNumber == "", "EmptyString",
                                     trackingNumber),
             packageNumber = ifelse(packageNumber == "", trackingNumber,
                                    packageNumber))
    
    OMS_Data_NoDup <- OMS_Data %>%
      # creating unique key for removing duplicated records having same tracking number & item number
      mutate(uniqueKey = paste0(tracking_number, id_sales_order_item)) %>%
      filter(!duplicated(uniqueKey))
    
    # Mapped using Tracking Number
    nonLEXCostDataMapped <- left_join(nonLEXCostData, OMS_Data_NoDup,
                                      by = c("trackingNumber" = "tracking_number"))
    nonLEXCostDataMapped %<>%
      select(-c(uniqueKey, package_number))
    
    mappedCost <- filter(nonLEXCostDataMapped, !is.na(id_sales_order_item))
    itemCostbyTracking <- mappedCost %>%
      left_join(SKUDimWeight, by = "sku") %>%
      # cost has to be grouped by month it reported
      group_by(month, trackingNumber) %>%
      # check dim weight data - only use dimweight cost distribution for full dimweight data
      # In case missing one item dimweight - cost is divided euqally to all item within packages
      mutate(dimWeightCheck = ifelse(any(is.na(weight)) | any(weight == 0), "Failed", "Passed")) %>%
      mutate(itemCost = ifelse(dimWeightCheck == "Passed", packageCost * weight / sum(weight),
                               packageCost / n())) %>%
      ungroup()
    
    # Try Map using Package Number
    unMappedCost <- filter(nonLEXCostDataMapped, is.na(id_sales_order_item))
    
    unMappedCost %<>% select(1:4)
    mappedPackage <- left_join(unMappedCost, OMS_Data,
                               by = c("packageNumber" = "package_number"))
    
    unmapped <- mappedPackage %>%
      filter(is.na(id_sales_order_item)) %>%
      select(1:4)
    
    mappedPackage %<>%
      filter(!is.na(id_sales_order_item)) %>%
      # consolidate two tracking column into one column using invoice tracking in case no tracking # in OMS data
      mutate(trackingNumber = ifelse(!is.na(tracking_number), tracking_number,
                                     trackingNumber)) %>%
      # creating unique key for removing duplicated records having same tracking number & item number
      mutate(uniqueKey = paste0(packageNumber, id_sales_order_item)) %>%
      filter(!duplicated(uniqueKey)) %>%
      select(-c(uniqueKey, tracking_number))
    
    itemCostbyPackage<- mappedPackage %>%
      left_join(SKUDimWeight, by = "sku") %>%
      group_by(month, packageNumber) %>%
      # check dim weight data - only use dimweight cost distribution for full dimweight data
      # In case missing one item dimweight - cost is divided euqally to all item within packages
      mutate(dimWeightCheck = ifelse(any(is.na(weight) | weight == 0), "Failed", "Passed")) %>%
      mutate(itemCost = ifelse(dimWeightCheck == "Passed", packageCost * weight / sum(weight),
                               packageCost / n())) %>%
      ungroup()
    
    unmapped %<>%
      mutate(itemCost = packageCost)
    
    itemCost <- rbind_list(itemCostbyTracking, itemCostbyPackage, unmapped)
    
    for (iWarn in warnings()){
      logwarn(paste("Function", "mapping processing", iWarn), logger = reportName)
    }
    
    itemCost
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "Done Mapping Seller Charges"), logger = reportName)
  })
  
  loginfo(paste("Function", functionName, "ended"), logger = reportName)
  
  itemCostData
}