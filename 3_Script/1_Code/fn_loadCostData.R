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
      filter(!duplicated(tracking_number, id_sales_order_item)) %>%
      mutate(Month=format(Shipped_Date, "%Y%m"))
    LEXCostTrackings <- left_join(LEXTrackings, LEXCost, by = "Month") %>%
      filter(!is.na(totalCost) & totalCost > 0) %>%
      group_by(Month) %>%
      mutate(Item_Cost = totalCost / n()) %>%
      ungroup() %>%
      select(Month, id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, order_nr,
             business_unit, payment_method, Tracking_Number = tracking_number,
             Package_Number, RTS_Date, Shipped_Date,
             Cancelled_Date, Delivered_Date, shipment_provider_name,
             Seller_Code, tax_class,
             Item_Cost)
  }
  
  costData <- data.frame(Delivery_Company = character(),
                         Tracking_Number = character(),
                         Package_Number = character(),
                         Pickup_Date = as.POSIXct(character()),
                         Cost_VAT = numeric(),
                         Cost_Ex_VAT = numeric(),
                         VAT = numeric(),
                         Month = character())
  
  for (file in list.files(costFilePath)){
    if(file_ext(file) == "csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Delivery_Company", "Tracking_Number",
                                            "Package_Number", "Pickup_Date", "Cost_VAT",
                                            "Cost_Ex_VAT", "VAT", "Month"),
                              colClasses = c("character", "myTrackingNumber",
                                             "character", "myDate", "myNumeric",
                                             "myNumeric", "myNumeric", "character"))
      
      costData <- rbind_list(costData,currentFile)
    }
  }
  
  iProgress <- 1
  setTxtProgressBar(pb, iProgress)
  
  costData %<>% mutate(Tracking_Number = ifelse(Tracking_Number == "", "EmptyString",
                                                Tracking_Number),
                       Package_Number = ifelse(Package_Number == "", "EmptyString",
                                               Package_Number))
  nonLexCost <- costData %>%
    arrange(Month) %>%
    group_by(Month, Tracking_Number, Package_Number) %>%
    summarize(Delivery_Company = last(Delivery_Company),
              Pickup_Date = last(Pickup_Date),
              Cost = sum(Cost_Ex_VAT, na.rm=TRUE)) %>%
    select(Tracking_Number,
           Package_Number,
           Cost,
           Month)
  
  nonLexCost %<>% filter(Cost > 0.0)
  LEXCost <- LoadLexCost(LEXCostPath, OMS_Data)
  
  iProgress <- 2
  setTxtProgressBar(pb, iProgress)
  
  OMS_Data_Tracking <- OMS_Data %>%
    filter(!duplicated(tracking_number, id_sales_order_item))
  Cost_OMS_Mapped <- left_join(nonLexCost, OMS_Data_Tracking,
                               by = c("Tracking_Number" = "tracking_number"))
  
  OMS_Data_Package <- OMS_Data %>%
    filter(!duplicated(package_number, id_sales_order_item))
  Cost_OMS_MappedByPackage <- Cost_OMS_Mapped %>%
    filter(is.na(business_unit) & Package_Number != "EmptyString") %>%
    select(Tracking_Number, Package_Number, shipment_provider_name, Cost, Month) %>%
    left_join(OMS_Data, by=c("Package_Number" = "package_number"))
  
  Cost_OMS_Mapped_Final <- Cost_OMS_Mapped %>%
    filter(!(is.na(business_unit) & Tracking_Number != "EmptyString"))
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
    group_by(Month, Tracking_Number, Package_Number) %>%
    mutate(pkgWeight = ifelse(any(is.na(weight) | weight == 0), 0,
                              sum(weight))) %>%
    mutate(Item_Cost = Cost / ifelse(pkgWeight == 0, n(), pkgWeight / weight)) %>%
    ungroup() %>%
    select(Month,id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, order_nr,
           business_unit, payment_method, Tracking_Number,
           Package_Number, RTS_Date, Shipped_Date,
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