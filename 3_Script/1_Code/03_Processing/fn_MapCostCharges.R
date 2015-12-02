MapCostCharges <- function(allItemCost, sellerChargesMapped) {
suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(logging)
  })
  
  functionName <- "MapCostCharges"
  loginfo(paste("Function", functionName, "started"), logger = reportName)
  
  mappedCostCharges <- tryCatch({
    
    itemCharges <- sellerChargesMapped %>%
      group_by(id_sales_order_item) %>%
      summarize(itemCharges = sum(itemCharges, na.rm = TRUE)) %>%
      ungroup()
    
    firstMonth <- allItemCost %>%
      arrange(month) %>%
      select(month, id_sales_order_item) %>%
      filter(!duplicated(id_sales_order_item))
    
    itemChargesMonth <- itemCharges %>% 
      left_join(firstMonth, by = "id_sales_order_item") %>%
      filter(!is.na(month), !is.na(id_sales_order_item))
    
    mappedCostCharges <- left_join(allItemCost, itemChargesMonth,
                                   by = c("month" = "month",
                                          "id_sales_order_item" = "id_sales_order_item"))
    
    for (iWarn in warnings()){
      logwarn(paste(functionName, iWarn), logger = reportName)
    }
    
    mappedCostCharges
    
  }, error = function(err) {
    logerror(paste(functionName, err, sep = " - "), logger = consoleLog)
  }, finally = {
    loginfo(paste(functionName, "Done Mapping Non LEX Cost Data"), logger = reportName)
  })
  
  mappedCostCharges
}