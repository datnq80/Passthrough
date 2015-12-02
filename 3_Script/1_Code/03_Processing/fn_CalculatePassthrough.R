CalculatePassthrough <- function(mappedCostCharges) {
  suppressMessages({
    require(tidyr)
    require(dplyr)
    require(data.table)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "CalculatePassthrough"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  passthroughData <- tryCatch({
    
    passthroughData <- mappedCostCharges %>% 
      replace_na(list(itemCharges=0))
    
    passthroughData %<>%
      mutate(itemCharges = -abs(itemCharges))
    
    passthroughData %<>% arrange(month) %>%
      group_by(id_sales_order_item) %>%
      mutate(accumCost = cumsum(itemCost),
             accumCharges = cumsum(itemCharges)) %>%
      ungroup()
    
    passthroughData <- data.table(passthroughData)
    passthroughData[, Index := 1:.N, by = id_sales_order_item]
    passthroughData <- tbl_df(passthroughData)
    passthroughData %<>%
      ungroup() %>% group_by(id_sales_order_item) %>%
      mutate(itemCharges = ifelse(accumCharges + accumCost + itemCost < 0,
                                         ifelse(Index==n(), accumCharges + accumCost - itemCost , - itemCost),
                                         ifelse(accumCharges + accumCost - itemCost < 0,
                                                accumCharges + accumCost - itemCost, 0)))
    
    passthroughData %<>%
      ungroup() %>%
      mutate(itemCharges = ifelse( is.na(itemCharges), 0, itemCharges)) %>%
      mutate(remark=ifelse(itemCharges == 0, "No_Charged", "Charged")) %>%
      mutate(remark=ifelse(is.na(business_unit) | business_unit == "Retail", NA, remark))
    
    passthroughData %<>%
      select(month,id_sales_order_item, SC_SOI_ID, order_nr,
             business_unit, payment_method, trackingNumber,
             packageNumber, RTS_Date, Shipped_Date,
             Cancelled_Date, Delivered_Date, shipment_provider_name,
             Seller_Code, tax_class,
             itemCost, itemCharges,
             remark, bob_id_sales_order_item)
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    passthroughData
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "ended"), logger = reportName)
  })
  
  passthroughData
}