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
  setAs("character","myTrackingNumber", function(from) (gsub('^0+','',toupper(from))))
  
  LoadLexCost <- function(LEXCostPath, OMS_Data) {
    LEXCost <- read.csv(LEXCostPath, 
                        col.names = c("Month", "totalCost"),
                        colClasses = c("character", "numeric"))
    LEXTrackings <- OMS_Data %>%
      filter(grepl("LEX", shipment_provider_name),
             tracking_number != "") %>%
      mutate(Month=format(Shipped_Date, "%Y%m")) %>%
      select(tracking_number, Month) %>%
      arrange(tracking_number) %>%
      filter(!duplicated(tracking_number))
    
    
    LEXCostTrackings <- left_join(LEXTrackings, LEXCost, by = "Month") %>%
      filter(!is.na(totalCost) & totalCost > 0) %>%
      group_by(Month) %>%
      mutate(Cost = totalCost / n()) %>%
      ungroup() %>%
      select(-(totalCost))
    
    LEXCostTrackings
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
                              stringsAsFactors = FALSE, row.names = NULL,
                              quote = '"',
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
                       package_number = ifelse(package_number == "", tracking_number,
                                               package_number))
  costTrackingNumber <- costData$tracking_number
  costPackageNumber <- costData$package_number
  
  nonLexCost <- costData %>%
    arrange(Month) %>%
    group_by(Month, tracking_number, package_number) %>%
    summarize(Delivery_Company = last(Delivery_Company),
              Pickup_Date = last(Pickup_Date),
              Cost = sum(Cost_Ex_VAT, na.rm=TRUE)) %>%
    select(tracking_number,
           package_number,
           Cost,
           Month) %>%
    ungroup()
  
  nonLexCost %<>% filter(Cost > 0.0)
  LEXCost <- LoadLexCost(LEXCostPath, OMS_Data)
  
  iProgress <- 2
  setTxtProgressBar(pb, iProgress)
  
  LEXTracking <- LEXCost$tracking_number
  OMS_Data_Tracking <- OMS_Data %>%
    filter(tracking_number %in% LEXTracking) %>%
    filter(!duplicated(uniqueTrackingKey))
  LEXCostMapped <- left_join(LEXCost, OMS_Data_Tracking,
                             by = "tracking_number")
  
  OMS_DataFiltered <-  OMS_Data %>%
    filter(tracking_number %in% costTrackingNumber |
             package_number %in% costPackageNumber)
  
  Cost_OMS_Mapped <- left_join(nonLexCost, OMS_DataFiltered,
                               by = c("tracking_number" = "tracking_number"))
  
  Cost_OMS_Mapped %<>%
    mutate(package_number = ifelse(is.na(package_number.y),
                                   package_number.x, package_number.y)) %>%
    select(-c(package_number.x, package_number.y))
  
  Cost_OMS_MappedByPackage <- Cost_OMS_Mapped %>%
    filter(is.na(business_unit) & package_number != "EmptyString") %>%
    select(tracking_number, package_number, Cost, Month)
  
  Cost_OMS_MappedByPackage %<>%
    left_join(OMS_DataFiltered, by = c("package_number" = "package_number"))
  
  Cost_OMS_MappedByPackage %<>%
    mutate(tracking_number = ifelse(is.na(tracking_number.y), 
                                    tracking_number.x, tracking_number.y)) %>%
    select(-c(tracking_number.x, tracking_number.y))
  
  Cost_OMS_Mapped_Final <- Cost_OMS_Mapped %>%
    filter(!(is.na(business_unit) & tracking_number != "EmptyString"))
  
  Cost_OMS_Mapped_Final <- rbind_list(Cost_OMS_Mapped_Final,
                                      Cost_OMS_MappedByPackage,
                                      LEXCostMapped)
  
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
  
  Item_Cost_Mapped <- Item_Cost %>%
    filter(!is.na(id_sales_order_item)) %>%
    group_by(Month, id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID) %>%
    mutate(Item_Cost = sum(Item_Cost)) %>%
    ungroup()
  
  Cost_Final <- rbind(Item_Cost_Mapped, filter(Item_Cost, is.na(id_sales_order_item)))
  
  iProgress <- 5
  setTxtProgressBar(pb, iProgress)
  cat("\r\n")
  
  Item_Cost
}