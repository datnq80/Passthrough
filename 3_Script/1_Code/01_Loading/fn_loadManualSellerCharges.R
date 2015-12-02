LoadManualSellerCharges <- function(costFilePath, OMS_Data) {
  
  require(dplyr, quietly = TRUE)
  require(tools, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(methods, quietly= TRUE)
  
  pb <- txtProgressBar(min=0,max=6, style = 3)
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
  setAs("character","myTrackingNumber", function(from) toupper(gsub('^0+','',from)))
  
  sellerCharges <- data.frame(Seller_Name=character(),
                              tracking_number=character(),
                              package_number=character(),
                              Item_Number=numeric(),
                              Pickup_Date=as.POSIXct(character()),
                              Charges_VAT=numeric(),
                              Charges_Ex_VAT=numeric(),
                              VAT=numeric())
  
  for (file in list.files(costFilePath)) {
    if(file_ext(file)=="csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE, row.names = NULL,
                              quote = '"',
                              col.names = c("Seller_Name","tracking_number","package_number",
                                            "Item_Number","Pickup_Date","Charges_VAT",
                                            "Charges_Ex_VAT","VAT"),
                              colClasses = c("character","myTrackingNumber","myTrackingNumber",
                                             "myNumeric","myDate","myNumeric",
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
    arrange(desc(Shipped_Date)) %>%
    filter(!duplicated(bob_id_sales_order_item))
  
  SellerCharges_Item <- filter(sellerCharges, !is.na(Item_Number)) %>%
    select(Item_Number, Charges_Ex_VAT)
  SellerCharges_Item_OMS <- left_join(SellerCharges_Item, OMS_Data_MP_Item,
                                      by = c("Item_Number"= "bob_id_sales_order_item"))
  
  itemTrackingCharged <- SellerCharges_Item_OMS %>%
    group_by(tracking_number) %>%
    summarize(packageCharges = sum(Charges_Ex_VAT, na.rm = TRUE)) %>%
    ungroup()
  
  trackingFilter <- itemTrackingCharged$tracking_number
  
  OMS_Data_MP %<>%
    mutate(uniqueTrackingKey = paste0(tracking_number, id_sales_order_item)) %>%
    mutate(uniquePackageKey = paste0(package_number, id_sales_order_item))
  
  OMS_Data_MP_Tracking <- OMS_Data_MP %>%
    filter(tracking_number %in% trackingFilter) %>%
    filter(!duplicated(uniqueTrackingKey))
  
  itemTrackingChargedItemLevel <- itemTrackingCharged %>% 
    left_join(OMS_Data_MP_Tracking, by = "tracking_number")
  
  itemChargedItem <- itemTrackingChargedItemLevel %>%
    group_by(tracking_number) %>%
    mutate(item_Charges = packageCharges / n()) %>%
    ungroup() %>%
    select(id_sales_order_item, bob_id_sales_order_item, SC_SOI_ID, item_Charges)
  
  
  iProgress <- 2
  setTxtProgressBar(pb, iProgress)
  
  itemNotMapped <- (filter(SellerCharges_Item_OMS, is.na(id_sales_order_item)))$Item_Number
  
  SellerCharges_Tracking <- sellerCharges %>%
    filter(is.na(Item_Number) | Item_Number %in% itemNotMapped) %>%
    group_by(tracking_number, package_number) %>%
    summarize(Charges_Ex_VAT = sum(Charges_Ex_VAT, na.rm = TRUE)) %>%
    ungroup()
  
  trackingFilter <- SellerCharges_Tracking$tracking_number
  
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
    select(tracking_number, package_number, Charges_Ex_VAT)
  packageFilter <- SellerCharges_Package$package_number
  OMS_Data_MP_Package <- OMS_Data_MP %>%
    filter(package_number %in% packageFilter) %>%
    filter(!duplicated(uniquePackageKey))
  SellerCharges_Package_OMS <- left_join(SellerCharges_Package, OMS_Data_MP,
                                         by = c("package_number" = "package_number"))
  SellerCharges_Package_OMS %<>%
    mutate(tracking_number = ifelse(is.na(bob_id_sales_order_item), 
                                    tracking_number.x, tracking_number.y)) %>%
    select(-c(tracking_number.x, tracking_number.y))
  
  iProgress <- 4
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
  
  iProgress <- 5
  setTxtProgressBar(pb, iProgress)
  
  
  itemCharged <- rbind_list(itemChargedTracking, itemChargedPackage, itemChargedItem)
  
  itemCharged %<>%
    filter(!is.na(id_sales_order_item)) %>%
    group_by(id_sales_order_item) %>%
    summarize(value=-abs(sum(item_Charges, na.rm = TRUE))) %>%
    ungroup()
  
  iProgress <- 6
  setTxtProgressBar(pb, iProgress)
  
  cat("\r\n")
  
  itemCharged
}