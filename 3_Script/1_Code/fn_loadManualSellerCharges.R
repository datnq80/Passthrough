LoadManualSellerCharges <- function(costFilePath, OMS_Data) {
  
  require(dplyr, quietly = TRUE)
  require(tools, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(methods, quietly= TRUE)
  
  pb <- txtProgressBar(min=0,max=5, style = 3)
  iProgress <- 0
  setTxtProgressBar(pb, iProgress)
  
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
                              tracking_number=character(),
                              package_number=character(),
                              Item_Number=integer(),
                              Pickup_Date=as.POSIXct(character()),
                              Charges_VAT=numeric(),
                              Charges_Ex_VAT=numeric(),
                              VAT=numeric())
  
  for (file in list.files(costFilePath)) {
    if(file_ext(file)=="csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Seller_Name","tracking_number","package_number",
                                            "Item_Number","Pickup_Date","Charges_VAT",
                                            "Charges_Ex_VAT","VAT"),
                              colClasses = c("character","myTrackingNumber","myTrackingNumber",
                                             "myInteger","myDate","myNumeric",
                                             "myNumeric","myNumeric"))
      sellerCharges <- rbind_list(sellerCharges,currentFile)
    }
  }
  
  iProgress <- 1
  setTxtProgressBar(pb, iProgress)
  
  sellerCharges %<>% filter(!is.na(Charges_Ex_VAT) & Charges_Ex_VAT != 0)
  sellerCharges %<>% mutate(tracking_number = ifelse(tracking_number == "", "EmptyString",
                                                tracking_number),
                            package_number = ifelse(package_number == "", tracking_number,
                                                    package_number))
  OMS_Data_MP <- OMS_Data %>% filter(business_unit == "MP")
  OMS_Data_MP_Item <- OMS_Data_MP %>%
    filter(!duplicated(bob_id_sales_order_item))
  
  SellerCharges_Item <- filter(sellerCharges, !is.na(Item_Number))
  SellerCharges_Item_OMS <- left_join(SellerCharges_Item, OMS_Data_MP_Item,
                                      by = c("Item_Number"= "bob_id_sales_order_item"))
  
  iProgress <- 2
  setTxtProgressBar(pb, iProgress)
  
  SellerCharges_Tracking <- filter(sellerCharges, is.na(Item_Number))
  trackingFilter <- SellerCharges_Tracking$tracking_number
  OMS_Data_MP_Tracking <- OMS_Data_MP %>%
    filter(tracking_number %in% trackingFilter) %>%
    filter(!duplicated(tracking_number, id_sales_order_item))
  SellerCharges_Tracking_OMS <- left_join(SellerCharges_Tracking, OMS_Data_MP_Tracking,
                                          by = c("tracking_number" = "tracking_number"))
  SellerCharges_Tracking_OMS %<>%
    mutate(package_number = ifelse(is.na(bob_id_sales_order_item), 
                                   package_number.x, package_number.y)) %>%
    select(-c(package_number.x, package_number.y))
  
  iProgress <- 3
  setTxtProgressBar(pb, iProgress)
  
  SellerCharges_Package <- SellerCharges_Tracking_OMS %>%
    filter(is.na(bob_id_sales_order_item)) %>%
    select(Seller_Name, tracking_number, package_number, Pickup_Date, Charges_VAT,
           Charges_Ex_VAT, VAT)
  packageFilter <- SellerCharges_Package$package_number
  OMS_Data_MP_Package <- OMS_Data_MP %>%
    filter(package_number %in% packageFilter) %>%
    filter(!duplicated(package_number, id_sales_order_item))
  SellerCharges_Package_OMS <- left_join(SellerCharges_Package, OMS_Data_MP,
                                         by = c("package_number" = "package_number"))
  SellerCharges_Package_OMS %<>%
    mutate(tracking_number = ifelse(is.na(bob_id_sales_order_item), 
                                    tracking_number.x, tracking_number.y)) %>%
    select(-c(tracking_number.x, tracking_number.y))
  
  iProgress <- 3
  setTxtProgressBar(pb, iProgress)
  
  itemChargedTracking <- SellerCharges_Tracking_OMS %>%
    group_by(tracking_number) %>%
    mutate(item_Charges = Charges_Ex_VAT / n()) %>% 
    ungroup() %>%
    select(id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, item_Charges)
  
  itemChargedPackage <- SellerCharges_Package_OMS %>%
    group_by(package_number) %>%
    mutate(item_Charges = Charges_Ex_VAT / n()) %>%
    ungroup() %>%
    select(id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, item_Charges)
  
  iProgress <- 4
  setTxtProgressBar(pb, iProgress)
  
  itemChargedItem <- SellerCharges_Item_OMS %>%
    select(id_sales_order_item, bob_id_sales_order_item=Item_Number, SC_SOI_ID, item_Charges=Charges_Ex_VAT)
  
  itemCharged <- rbind_list(itemChargedTracking, itemChargedPackage, itemChargedItem)
  
  itemCharged %<>%
    filter(!is.na(id_sales_order_item)) %>%
    group_by(id_sales_order_item) %>%
    summarize(value=-abs(sum(item_Charges)))
  
  iProgress <- 5
  setTxtProgressBar(pb, iProgress)
  
  cat("\r\n")
  
  itemCharged
}