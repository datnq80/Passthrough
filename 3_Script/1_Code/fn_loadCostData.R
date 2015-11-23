loadCostData <- function(costFilePath, LEXCostPath, 
                         OMS_Data, SKUDimWeight) {
  suppressMessages({
    require(dplyr, quietly = TRUE)
    require(tools, quietly = TRUE)
    require(magrittr, quietly = TRUE)
    require(methods, quietly= TRUE)
  })
  
  pb <- txtProgressBar(min=0,max=5, style = 3)
  iProgress <- 0
  setTxtProgressBar(pb, iProgress)
  
  setClass('myDate')
  setAs("character","myDate", function(from) as.POSIXct(substr(from,1,10), format="%Y-%m-%d")) #covnert datetime string to datetime
  setClass("myInteger")
  setAs("character","myInteger", function(from) as.integer(gsub('"','',from)))
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('[",]','',from)))
  setClass("myTrackingNumber")
  setAs("character","myTrackingNumber", function(from) gsub('^0+','',from))
  
  LoadLexCost <- function(LEXCostPath, OMS_Data) {
    LEXCost <- read.csv(LEXCostPath, 
                        col.names = c("Month", "totalCost"),
                        colClasses = c("character", "numeric"))
    LEXTrackings <- OMS_Data %>%
      filter(grepl("LEX", shipment_provider_name),
             tracking_number != "") %>%
      arrange(tracking_number, id_sales_order_item) %>%
      filter(!duplicated(tracking_number, id_sales_order_item)) %>%
      mutate(Month=format(Shipped_Date, "%Y%m"))
    
    LEXCostTrackings <- left_join(LEXTrackings, LEXCost, by = "Month") %>%
      filter(!is.na(totalCost) & totalCost > 0) %>%
      group_by(Month) %>%
      mutate(Item_Cost = totalCost / n()) %>%
      ungroup() 
    
    LEXCostTrackings %>%
      select(Month, id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, order_nr,
             business_unit, payment_method, tracking_number = tracking_number,
             package_number, RTS_Date, Shipped_Date,
             Cancelled_Date, Delivered_Date, shipment_provider_name,
             Seller_Code, tax_class,Item_Cost)
  }
  
  costData <- data.frame(Delivery_Company = character(),
                         tracking_number = character(),
                         package_number = character(),
                         Pickup_Date = as.POSIXct(character()),
                         Cost_VAT = numeric(),
                         Cost_Ex_VAT = numeric(),
                         VAT = numeric(),
                         Month = character())
  
  for (file in list.files(costFilePath)){
    if(file_ext(file) == "csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Delivery_Company", "tracking_number",
                                            "package_number", "Pickup_Date", "Cost_VAT",
                                            "Cost_Ex_VAT", "VAT", "Month"),
                              colClasses = c("character", "myTrackingNumber",
                                             "character", "myDate", "myNumeric",
                                             "myNumeric", "myNumeric", "character"))
      
      costData <- rbind_list(costData,currentFile)
    }
  }
  
  iProgress <- 1
  setTxtProgressBar(pb, iProgress)
  
  costData %<>% mutate(tracking_number = ifelse(tracking_number == "", "EmptyString",
                                                tracking_number),
                       package_number = ifelse(package_number == "", "EmptyString",
                                               package_number))
  nonLexCost <- costData %>%
    arrange(Month) %>%
    group_by(Month, tracking_number, package_number) %>%
    summarize(Delivery_Company = last(Delivery_Company),
              Pickup_Date = last(Pickup_Date),
              Cost = sum(Cost_Ex_VAT, na.rm=TRUE)) %>%
    select(tracking_number,
           package_number,
           Cost,
           Month)
  
  nonLexCost %<>% filter(Cost > 0.0)
  LEXCost <- LoadLexCost(LEXCostPath, OMS_Data)
  
  iProgress <- 2
  setTxtProgressBar(pb, iProgress)
  
  OMS_Data_Tracking <- OMS_Data %>%
    arrange(tracking_number, id_sales_order_item) %>%
    filter(!duplicated(tracking_number, id_sales_order_item))
  Cost_OMS_Mapped <- left_join(nonLexCost, OMS_Data_Tracking,
                               by = c("tracking_number" = "tracking_number"))
  
  OMS_Data_Package <- OMS_Data %>%
    arrange(package_number, id_sales_order_item) %>%
    filter(!duplicated(package_number, id_sales_order_item))
  Cost_OMS_MappedByPackage <- Cost_OMS_Mapped %>%
    filter(is.na(business_unit) & package_number != "EmptyString") %>%
    select(tracking_number, package_number, shipment_provider_name, Cost, Month) %>%
    left_join(OMS_Data, by=c("package_number" = "package_number"))
  
  Cost_OMS_Mapped_Final <- Cost_OMS_Mapped %>%
    filter(!(is.na(business_unit) & tracking_number != "EmptyString"))
  Cost_OMS_Mapped_Final <- rbind_list(Cost_OMS_Mapped_Final,
                                      select(Cost_OMS_MappedByPackage, -(tracking_number)),
                                      LEXCost)
  iProgress <- 3
  setTxtProgressBar(pb, iProgress)
  
  Cost_OMS_Mapped_Final %<>%
    left_join(SKUDimWeight, by = c("sku" = "sku"))
  
  iProgress <- 4
  setTxtProgressBar(pb, iProgress)
  
  Item_Cost <- Cost_OMS_Mapped_Final %>%
    filter(Cost > 0) %>%
    group_by(Month, tracking_number, package_number) %>%
    mutate(pkgWeight = ifelse(any(is.na(weight) | weight == 0), 0,
                              sum(weight))) %>%
    mutate(Item_Cost = Cost / ifelse(pkgWeight == 0, n(), pkgWeight / weight)) %>%
    ungroup() %>%
    select(Month,id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, order_nr,
           business_unit, payment_method, tracking_number,
           package_number, RTS_Date, Shipped_Date,
           Cancelled_Date, Delivered_Date, shipment_provider_name,
           Seller_Code, tax_class,
           Item_Cost)
  
  Item_Cost %<>%
    group_by(Month, id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID) %>%
    mutate(Item_Cost = sum(Item_Cost)) %>%
    filter(!duplicated(id_sales_order_item)) %>%
    ungroup()
  
  iProgress <- 5
  setTxtProgressBar(pb, iProgress)
  cat("\r\n")
  
  Item_Cost
}