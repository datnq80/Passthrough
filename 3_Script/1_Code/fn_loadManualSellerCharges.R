LoadManualSellerCharges <- function(costFilePath){
  
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
  
  sellerCharges <- data.frame(Seller_Name=character(),
                              Tracking_Number=character(),
                              Item_Number=character(),
                              Pickup_Date=as.POSIXct(character()),
                              Charges_VAT=numeric(),
                              Charges_Ex_VAT=numeric(),
                              VAT=numeric())
  
  for (file in list.files(costFilePath)){
    if(file_ext(file)=="csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Seller_Name","Tracking_Number",
                                            "Item_Number","Pickup_Date","Charges_VAT",
                                            "Charges_Ex_VAT","VAT"),
                              colClasses = c("character","myTrackingNumber",
                                             "character","myDate","myNumeric",
                                             "myNumeric","myNumeric"))
      sellerCharges <- rbind_list(sellerCharges,currentFile)
    }
  }
  
  sellerCharges %<>%
    group_by(Tracking_Number) %>%
    summarize(value=sum(Charges_Ex_VAT, na.rm=TRUE))
  
  sellerCharges %>% filter()
  OMS_Data_MP <- OMS_Data %>% filter(business_unit=="MP")
  sellerCharges <- left_join(sellerCharges, OMS_Data_MP,
                             by=c("Tracking_Number"="tracking_number"))
  
  sellerCharges %<>%
    group_by(Tracking_Number) %>%
    mutate(value=-abs(value/n())) %>%
    ungroup() %>%
    select(src_id=id_sales_order_item,
           value) %>%
    filter(!is.na(src_id))

  
  sellerCharges
}