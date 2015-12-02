suppressMessages({
  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(data.table)
  library(tidyr)
  library(logging)
})

dateLog <- format(Sys.time(), "%Y%m%d%H%M")

tryCatch({
  
  args <- commandArgs(trailingOnly = TRUE)
  venture <- args[1]
  dimWeightFactor <- as.numeric(args[2])
  
  ventureShort <- switch (venture,
                          "Indonesia" = "ID",
                          "Malaysia" = "MY",
                          "Philippines" = "PH",
                          "Singapore" = "SG",
                          "Thailand" = "TH",
                          "Vietnam" = "VN"
  )
  
  addHandler(writeToFile, logger="Passthrough",
             file=file.path("3_Script/2_Log",
                            paste0(ventureShort,"_passthrough_",dateLog,".csv")))
  addHandler(writeToConsole , logger="Passthrough.Console")
  
  loginfo(paste0(venture, " - Passthrough Start"), logger = "Passthrough.Console")
  loginfo("Initial Setup", logger = "Passthrough")
  
  
  runningFolderName <- format(Sys.Date(),"%Y%m%d")
  
  source("3_Script/1_Code/fn_loadCostData.R")
  source("3_Script/1_Code/fn_loadOMSData.R")
  source("3_Script/1_Code/fn_loadSellerCharges.R")
  source("3_Script/1_Code/fn_loadManualSellerCharges.R")
  source("3_Script/1_Code/fn_LoadSKUDimWeight.R")
  
  omsFolder <- file.path("1_Input",venture,"OMS_Data")
  sellerCharged <- file.path("1_Input",venture,"Seller_Charges_Data")
  manualData <- file.path("1_Input",venture,"Manual_Data")
  LEXCostPath <- file.path("1_Input",venture,"LEX_Cost",
                           paste0(ventureShort,"_LEX_Cost.csv"))
  skuDimensionPath <- file.path("1_Input",venture,"SKU_Dimension")
  outputFolder <- file.path("2_Output",runningFolderName)
  
  if(!dir.exists(outputFolder)){
    dir.create(outputFolder)
  }
  outputFolder <- file.path(outputFolder,venture)
  
  if(!dir.exists(outputFolder)){
    dir.create(outputFolder)
  }
  
  loginfo("Start Loading OMS Data", logger = "Passthrough.Console")
  if (!file.exists(file.path("3_Script/3_RData", venture, "OMS_Data.RData"))){
    OMS_Data <- loadOMSData(omsFolder)
    save(OMS_Data, file = file.path("3_Script/3_RData", venture, "OMS_Data.RData"))
  } else {
    load(file.path("3_Script/3_RData", venture, "OMS_Data.RData"))
  }
  loginfo("Start Loading SKU Dimension Data", logger = "Passthrough.Console")
  SKUDimWeight <- LoadSKUDimWeight(skuDimensionPath, dimWeightFactor)
  loginfo("Start Loading Cost Data", logger = "Passthrough.Console")
  if (!file.exists(file.path("3_Script/3_RData", venture, "Item_Cost.RData"))){
    Item_Cost <- loadCostData(manualData, LEXCostPath, 
                              OMS_Data, SKUDimWeight)
    save(Item_Cost, file = file.path("3_Script/3_RData", venture, "Item_Cost.RData"))
  } else {
    load(file.path("3_Script/3_RData", venture, "Item_Cost.RData"))
  }
  loginfo("Start Loading Seller Charges Data", logger = "Passthrough.Console")
  if (!file.exists(file.path("3_Script/3_RData", venture, "SellerCharges.RData"))){
    SellerCharges <- LoadManualSellerCharges(sellerCharged, OMS_Data)
    save(SellerCharges, file = file.path("3_Script/3_RData", venture, "SellerCharges.RData"))
  } else {
    load(file.path("3_Script/3_RData", venture, "SellerCharges.RData"))
  }
  
  save.image(file = "temp.RData")
  
  loginfo("Passthrough Calculation Start", logger = "Passthrough.Console")
  firstMonth <- Item_Cost %>%
    select(id_sales_order_item,Month) %>%
    arrange (Month) %>%
    filter(!duplicated(id_sales_order_item), !is.na(id_sales_order_item))
  SellerChargesMonth <- SellerCharges %>% 
    left_join(firstMonth, by=c("id_sales_order_item")) 
  
  Item_Cost_SellerCharge <- left_join(Item_Cost, SellerChargesMonth,
                                      by=c("id_sales_order_item",
                                           "Month" = "Month"))
  
  Passthrough_Data <- Item_Cost_SellerCharge %>% 
    rename(Item_SellerCharged = value) %>%
    mutate(Item_SellerCharged = ifelse(is.na(Item_SellerCharged),0,Item_SellerCharged))
  
  Passthrough_Data %<>% arrange(Month) %>%
    group_by(id_sales_order_item) %>%
    mutate(accumCost = cumsum(Item_Cost),
           accumCharges = cumsum(Item_SellerCharged)) %>%
    ungroup()
  Passthrough_Data <- data.table(Passthrough_Data)
  Passthrough_Data[, Index := 1:.N, by = id_sales_order_item]
  Passthrough_Data <- tbl_df(Passthrough_Data)
  Passthrough_Data %<>%
    ungroup() %>% group_by(id_sales_order_item) %>%
    mutate(Item_SellerCharged = ifelse(accumCharges + accumCost + Item_Cost < 0,
                                       ifelse(Index==n(), accumCharges + accumCost - Item_Cost , - Item_Cost),
                                       ifelse(accumCharges + accumCost - Item_Cost < 0,
                                              accumCharges + accumCost - Item_Cost, 0)))
  
  Passthrough_Data %<>%
    ungroup() %>%
    mutate(Item_SellerCharged = ifelse( is.na(Item_SellerCharged), 0, Item_SellerCharged)) %>%
    mutate(Is_Charged=ifelse(Item_SellerCharged == 0, "No_Charged", "Charged")) %>%
    mutate(Remark=ifelse(is.na(business_unit) | business_unit == "Retail", NA, Is_Charged))
  
  Passthrough_Data %<>%
    select(Month,id_sales_order_item, SC_SOI_ID, order_nr,
           business_unit, payment_method, tracking_number,
           package_number, RTS_Date, Shipped_Date,
           Cancelled_Date, Delivered_Date, shipment_provider_name,
           Seller_Code, tax_class,
           Item_Cost, Item_SellerCharged,
           Remark, bob_id_sales_order_item)
  
  Passthrough_Data %<>% 
    group_by(tracking_number) %>%
    mutate(Item_SellerCharged = sum(Item_SellerCharged, na.rm = TRUE) / n()) %>%
    ungroup()
    
  Passthrough_Data %<>%
    ungroup() %>%
    mutate(Remark=ifelse(Item_SellerCharged == 0, "No_Charged", "Charged")) %>%
    mutate(Remark=ifelse(is.na(business_unit) | business_unit == "Retail", NA, Is_Charged))
  
  monthReport <- unique(Passthrough_Data$Month)
  loginfo("Passthrough Calculation Done", logger = "Passthrough.Console")
  loginfo("Writing Data to CSV File", logger = "Passthrough.Console")
  for (iMonth in monthReport){
    write.csv(filter(Passthrough_Data, Month==iMonth), 
              file.path(outputFolder,
                        paste0(ventureShort,"_",iMonth,"_Passthrough_data.csv")),
              row.names = FALSE)
  }
  loginfo(paste0(venture, " - Passthrough Done!!!"), logger = "Passthrough.Console")
},error = function(err){
  logerror(err, logger = "Passthrough")
  logerror("PLease send 3_Script/Log folder to Regional OPS BI for additional support",
           logger = "Passthrough.Console")
})