loadCostData <- function(costFilePath, OMS_Data) {
  
  require(dplyr, quietly = TRUE)
  require(tools, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(methods, quietly= TRUE)
  
  setClass('myDate')
  setAs("character","myDate", function(from) as.POSIXct(substr(from,1,10), format="%Y-%m-%d")) #covnert datetime string to datetime
  setClass("myInteger")
  setAs("character","myInteger", function(from) as.integer(gsub('"','',from)))
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('[",]','',from)))
  setClass("myTrackingNumber")
  setAs("character","myTrackingNumber", function(from) gsub('^0+','',from))
  
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

  costData %<>% mutate(Tracking_Number = ifelse(Tracking_Number == "", "EmptyString",
                                                Tracking_Number),
                       Package_Number = ifelse(Package_Number == "", "EmptyString",
                                               Package_Number))
  
  costData %<>%
    arrange(Month) %>%
    group_by(Month, Tracking_Number, Package_Number) %>%
    summarize(Delivery_Company = last(Delivery_Company),
              Pickup_Date = last(Pickup_Date),
              Cost = sum(Cost_Ex_VAT, na.rm=TRUE)) %>%
    select(Tracking_Number,
           Package_Number,
           Delivery_Company,
           Pickup_Date,
           Cost,
           Month)
  
  costData %<>% filter(Cost > 0.0)
  
  Cost_OMS_Mapped <- left_join(costData, OMS_Data,
                               by = c("Tracking_Number" = "tracking_number"))
  
  Cost_OMS_MappedByPackage <- Cost_OMS_Mapped %>%
    filter(is.na(business_unit) & Tracking_Number != "EmptyString") %>%
    select(Tracking_Number, Package_Number, Delivery_Company, Pickup_Date, Cost, Month) %>%
    left_join(OMS_Data, by=c("Package_Number" = "package_number"))
  
  Cost_OMS_Mapped_Final <- Cost_OMS_Mapped %>%
    filter(!(is.na(business_unit) & Tracking_Number != "EmptyString"))
  Cost_OMS_Mapped_Final <- rbind_list(Cost_OMS_Mapped_Final, select(Cost_OMS_MappedByPackage, -(tracking_number)))
  
  Item_Cost <- Cost_OMS_Mapped_Final %>%
    group_by(Month, Tracking_Number, Package_Number) %>%
    mutate(Item_Cost = Cost / n()) %>%
    ungroup() %>%
    select(Month,id_sales_order_item, SC_SOI_ID, order_nr,
           business_unit, payment_method, Tracking_Number,
           Package_Number, RTS_Date, Shipped_Date,
           Cancelled_Date, Delivered_Date, shipment_provider_name,
           Seller_Code, tax_class,
           Item_Cost)
  
  Item_Cost
}