loadOMSData <- function(omsDataFolder){
  
  require(dplyr, quietly = TRUE)
  require(tools, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(methods, quietly= TRUE)
  
  setClass("myDateTime")
  setAs("character","myDateTime", function(from) as.POSIXct(gsub('"','',from), format="%Y-%m-%d %H:%M:%S"))
  setClass("myInteger")
  setAs("character","myInteger", function(from) as.integer(gsub('"','',from)))
  setClass("myNumeric")
  setAs("character","myNumeric", function(from) as.numeric(gsub('"','',from)))
  
  omsDataAll <- data.frame(order_nr=integer(),
                           id_sales_order_item=integer(),
                           SC_SOI_ID=integer(),
                           business_unit=character(),
                           payment_method=character(),
                           sku=character(),
                           unit_price=numeric(),
                           paid_price=numeric(),
                           shipping_fee=numeric(),
                           shipping_surcharge=numeric(),
                           Item_Status=character(),
                           RTS_Date=as.POSIXct(character()),
                           Shipped_Date=as.POSIXct(character()),
                           Cancelled_Date=as.POSIXct(character()),
                           Delivered_Date=as.POSIXct(character()),
                           tracking_number=character(),
                           package_number=character(),
                           shipment_provider_name=character(),
                           Seller_Code=character(),
                           tax_class=character(),
                           shipping_city=character(),
                           shipping_region=character())
  
  for (file in list.files(omsDataFolder)){
    if(file_ext(file)=="csv"){
      currentFileData <- read.csv(file.path(omsDataFolder,file),
                                  quote = '"', sep=",",
                                  col.names=c("order_nr","id_sales_order_item","SC_SOI_ID",
                                              "business_unit","payment_method","sku",
                                              "unit_price","paid_price",
                                              "shipping_fee","shipping_surcharge","Item_Status",
                                              "RTS_Date","Shipped_Date","Cancelled_Date",
                                              "Delivered_Date","tracking_number","package_number",
                                              "shipment_provider_name","Seller_Code",
                                              "tax_class","shipping_city","shipping_region"),
                                  colClasses = c("myInteger","myInteger","myInteger",
                                                 "character","character","character",
                                                 "myNumeric","myNumeric",
                                                 "myNumeric","myNumeric","character",
                                                 "myDateTime","myDateTime","myDateTime",
                                                 "myDateTime","character","character",
                                                 "character","character",
                                                 "character","character","character"))
      
      if (is.null(omsDataAll))
        omsDataAll <- currentFileData
      else
        omsDataAll <- rbind_list(omsDataAll,currentFileData)
    }
  }
  
  omsDataAll %<>%
    mutate(Tracking_number=gsub("^0","",tracking_number)) #remove leading ZERO of tracking number to mapped with Invoice Data
  
  omsDataAll
}