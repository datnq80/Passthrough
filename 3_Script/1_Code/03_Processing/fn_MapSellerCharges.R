MapSellerChargesOMS <- function(sellerCharges_raw, OMS_Data, SKUDimWeight) {
  sellerChargesMapped <- MapSellerCharges(sellerCharges_raw, OMS_Data, SKUDimWeight,
                                          "id_sales_order_item")
  
  sellerChargesMapped
}

MapSellerChargesBOB <- function(sellerCharges_raw, OMS_Data, SKUDimWeight) {
  sellerChargesMapped <- MapSellerCharges(sellerCharges_raw, OMS_Data, SKUDimWeight,
                                          "bob_id_sales_order_item")
  sellerChargesMapped
}

MapSellerCharges <- function(sellerCharges_raw, OMS_Data, SKUDimWeight,
                             itemIDColumn = "id_sales_order_item") {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "MapSellerCharges"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  sellerChargesMapped <- tryCatch({
    
    OMS_Data_NoDup <- OMS_Data %>%
      # creating unique key for removing duplicated records having same tracking number & item number
      mutate(uniqueKey = paste0(tracking_number, id_sales_order_item)) %>%
      # only get MP as seller charges only applicable to MP items
      filter(!duplicated(uniqueKey), business_unit == "MP")
    
    sellerChargesTracking <- filter(sellerCharges_raw, tracking_number != "EmptyString")
    sellerChargesTracking %<>% 
      group_by(tracking_number) %>%
      summarize(packageCharges = sum(Charges_Ex_VAT, na.rm = TRUE)) %>%
      ungroup()
    sellerChargesTrackingMapped <- left_join(sellerChargesTracking, OMS_Data_NoDup,
                                             by = "tracking_number")
    
    mappedCharges <- sellerChargesTrackingMapped %>%
      filter(!is.na(business_unit))
    itemChargesbyTracking <- mappedCharges %>%
      left_join(SKUDimWeight, by = "sku") %>%
      group_by(tracking_number) %>%
      # check dim weight data - only use dimweight cost distribution for full dimweight data
      # In case missing one item dimweight - cost is divided euqally to all item within packages
      mutate(dimWeightCheck = ifelse(any(is.na(weight) | weight == 0), "Failed", "Passed")) %>%
      mutate(itemCharges = ifelse(dimWeightCheck == "Passed", packageCharges * weight / sum(weight),
                                  packageCharges / n())) %>%
      ungroup() 
    itemChargesbyTracking %<>%
      select(tracking_number, package_number, id_sales_order_item, itemCharges)
    
    loginfo(paste(functionName, "Done Mapping Tracking"), logger = reportName)
    
    mappedTrackingNumber <- mappedCharges$tracking_number
    sellerChargesPackage <- sellerCharges_raw %>%
      filter(!(tracking_number %in% mappedTrackingNumber))
    sellerChargesPackage %<>%
      group_by(package_number) %>%
      summarize(packageCharges = sum(Charges_Ex_VAT, na.rm = TRUE)) %>%
      ungroup()
    sellerChargesPackageMapped <- left_join(sellerChargesPackage, OMS_Data_NoDup,
                                            by = "package_number")
    
    mappedCharges <- sellerChargesPackageMapped %>%
      filter(!is.na(business_unit))
    itemChargesbyPacakages <- mappedCharges %>%
      left_join(SKUDimWeight, by = "sku") %>%
      group_by(package_number) %>%
      # check dim weight data - only use dimweight cost distribution for full dimweight data
      # In case missing one item dimweight - cost is divided euqally to all item within packages
      mutate(dimWeightCheck = ifelse(any(is.na(weight) | weight == 0), "Failed", "Passed")) %>%
      mutate(itemCharges = ifelse(dimWeightCheck == "Passed", packageCharges * weight / sum(weight),
                                  packageCharges / n())) %>%
      ungroup()
    itemChargesbyPacakages %<>%
      select(tracking_number, package_number, id_sales_order_item, itemCharges)
    
    loginfo(paste(functionName, "Done Mapping Packages"), logger = reportName)
    
    mappedPackageNumber <- mappedCharges$package_number
    sellerChargesItem <- sellerCharges_raw %>%
      filter(!(tracking_number %in% mappedTrackingNumber),
             !(package_number %in% mappedPackageNumber),
             !is.na(Item_Number))
    noMappedAll <- sellerCharges_raw %>%
      filter(!(tracking_number %in% mappedTrackingNumber),
             !(package_number %in% mappedPackageNumber),
             is.na(Item_Number))
    sellerChargesItem %<>%
      group_by(Item_Number) %>%
      summarize(packageCharges = sum(Charges_Ex_VAT, na.rm = TRUE)) %>%
      ungroup()
    sellerChargesItemMapped <- left_join(sellerChargesItem, OMS_Data_NoDup,
                                         by = c("Item_Number" = itemIDColumn))
    noMapped <- filter(sellerChargesItemMapped, is.na(tracking_number))
    sellerChargesItemMapped %<>% select(tracking_number, packageCharges)
    sellerChargesItemMapped <- left_join(sellerChargesItemMapped, OMS_Data_NoDup,
                                         by = "tracking_number")
    
    mappedCharges <- sellerChargesItemMapped %>%
      filter(!is.na(business_unit))
    itemChargesbyItem <- mappedCharges %>%
      left_join(SKUDimWeight, by = "sku") %>%
      group_by(tracking_number) %>%
      # check dim weight data - only use dimweight cost distribution for full dimweight data
      # In case missing one item dimweight - cost is divided euqally to all item within packages
      mutate(dimWeightCheck = ifelse(any(is.na(weight) | weight == 0), "Failed", "Passed")) %>%
      mutate(itemCharges = ifelse(dimWeightCheck == "Passed", packageCharges * weight / sum(weight),
                                  packageCharges / n())) %>%
      ungroup() 
    itemChargesbyItem %<>%
      select(tracking_number, package_number, id_sales_order_item, itemCharges)
    
    noMappedItem <- sellerCharges_raw %>% filter(Item_Number %in% noMapped$Item_Number)
    noMappedAll <- rbind_list(noMappedAll, noMappedItem)
    noMappedAll %<>%
      select(tracking_number, package_number,
             id_sales_order_item = Item_Number, 
             itemCharges = Charges_Ex_VAT)
    
    allChargesMapped <- rbind_list(itemChargesbyTracking, itemChargesbyPacakages,
                                   itemChargesbyItem, noMappedAll)
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    allChargesMapped
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "Done Mapping Seller Charges Data"), logger = reportName)
  })
  
  sellerChargesMapped
}