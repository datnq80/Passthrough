LoadManualSellerCharges <- function(costFilePath, OMS_Data) {
  
  require(dplyr, quietly = TRUE)
  require(tools, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(methods, quietly= TRUE)
  
  #covnert datetime string to datetime
  setClass('myDate')
  setAs("character","myDate", function(from) as.POSIXct(substr(from,  1,10),
                                                        format="%Y-%m-%d"))
  
  setClass("myInteger")
  setAs("character","myInteger", function(from) as.integer(gsub('"','',from)))
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('[",]','',from)))
  setClass("myTrackingNumber")
  setAs("character","myTrackingNumber", function(from) gsub('^0+','',from))
  
  sellerCharges <- data.frame(Seller_Name=character(),
                              Tracking_Number=character(),
                              Package_Number=character(),
                              Item_Number=integer(),
                              Pickup_Date=as.POSIXct(character()),
                              Charges_VAT=numeric(),
                              Charges_Ex_VAT=numeric(),
                              VAT=numeric())
  
  for (file in list.files(costFilePath)) {
    if(file_ext(file)=="csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Seller_Name","Tracking_Number","Package_Number",
                                            "Item_Number","Pickup_Date","Charges_VAT",
                                            "Charges_Ex_VAT","VAT"),
                              colClasses = c("character","myTrackingNumber","myTrackingNumber",
                                             "myInteger","myDate","myNumeric",
                                             "myNumeric","myNumeric"))
      sellerCharges <- rbind_list(sellerCharges,currentFile)
    }
  }
  
  sellerCharges %<>% filter(!is.na(Charges_Ex_VAT) & Charges_Ex_VAT != 0)
  sellerCharges %<>% mutate(Tracking_Number = ifelse(Tracking_Number == "", "EmptyString",
                                                Tracking_Number),
                            Package_Number = ifelse(Package_Number == "", "EmptyString",
                                                    Package_Number))
  OMS_Data_MP <- OMS_Data %>% filter(business_unit == "MP")
  
  SellerCharges_Item <- filter(sellerCharges, !is.na(Item_Number))
  SellerCharges_Tracking <- filter(sellerCharges, is.na(Item_Number))
  
  SellerCharges_Item_OMS <- left_join(SellerCharges_Item, OMS_Data_MP,
                                      by = c("Item_Number"= "bob_id_sales_order_item"))
  SellerCharges_Tracking_OMS <- left_join(SellerCharges_Tracking, OMS_Data_MP,
                                          by = c("Tracking_Number" = "tracking_number"))
  SellerCharges_Package <- filter(SellerCharges_Tracking_OMS, is.na(bob_id_sales_order_item)) %>%
    select(1:8)
  SellerCharges_Package_OMS <- left_join(SellerCharges_Package, OMS_Data_MP,
                                         by = c("Package_Number" = "package_number"))
  
  itemChargedTracking <- SellerCharges_Tracking_OMS %>%
    group_by(Tracking_Number) %>%
    mutate(item_Charges = Charges_Ex_VAT / n()) %>% 
    ungroup() %>%
    select(id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, item_Charges)
  
  itemChargedPackage <- SellerCharges_Package_OMS %>%
    group_by(Package_Number) %>%
    mutate(item_Charges = Charges_Ex_VAT / n()) %>%
    ungroup() %>%
    select(id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, item_Charges)
  
  itemChargedItem <- SellerCharges_Item_OMS %>%
    select(id_sales_order_item, bob_id_sales_order_item=Item_Number, SC_SOI_ID, item_Charges=Charges_Ex_VAT)
  itemCharged <- rbind_list(itemChargedTracking, itemChargedPackage, itemChargedItem)
  
  itemCharged %<>%
    filter(!is.na(id_sales_order_item)) %>%
    group_by(id_sales_order_item) %>%
    summarize(value=-abs(sum(item_Charges)))
  
  itemCharged
}