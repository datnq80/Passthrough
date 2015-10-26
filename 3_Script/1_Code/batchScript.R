##### Init #####
library(dplyr)
library(magrittr)
library(tools)
library(lubridate)

setClass('myDate')
setAs("character","myDate", function(from) as.POSIXct(substr(from,1,10), format="%Y-%m-%d")) #covnert datetime string to datetime
setClass('myDateTime')
setAs("character","myDateTime", function(from) as.POSIXct(from, format="%Y-%m-%d %H:%M:%S")) #covnert datetime string to datetime

##### Load Initial Variables #####
dateFolder <- readChar("../../1_Input/RunningFolder.txt",
                       file.info("../../1_Input/RunningFolder.txt")$size)
cat("\r\nRunning Folder: ",dateFolder,"\r\n")

runningFolder <- file.path("../../1_Input",dateFolder)
venture <- "Thailand"
ventureFolder <- file.path(runningFolder,venture)
outputFolder <- file.path("../../2_Output",dateFolder)
if(!dir.exists(outputFolder))
    dir.create(outputFolder)
outputVentureFolder <- file.path(outputFolder,venture)
if(!dir.exists(outputVentureFolder))
    dir.create(outputVentureFolder)

##### Loading Data #####
ventureInvoiceFolder <- file.path(ventureFolder,"Invoices")
allInvoice <- loadInvoiceData(ventureInvoiceFolder)

##### Load OMS Data #####
oms_data <- read.csv(file.path(ventureFolder,"OMS_Data","Actuals_V6_TrackingNumber_Delivered_Malaysia_201508.csv"),
                     col.names=c("id_sales_order_item","item_number","business_unit",
                                 "paid_price","shipping_fee","bulk_fee",
                                 "shipped_status","item_shipped_date","current_item_status",
                                 "item_delivery_date","sku","regional_category_key",
                                 "order_nr","order_created_date","order_grand_total",
                                 "payment_method","package_number","tracking_number",
                                 "shipment_provider_name","gtm_routing","package_shipping_status",
                                 "package_shipping_date","package_delivery_status","package_delivery_date",
                                 "item_length_2","item_width_2","item_height_2",
                                 "item_weight","item_dim_weight","item_dimension_status",
                                 "item_carrier_fee","item_cod_fee","item_insurance_fee",
                                 "item_dw_carrier_fee","package_dw_value","package_weight_value",
                                 "package_dimension_status","package_weight_status","matched_tag",
                                 "item_in_order","item_in_package","warehouse_name"),
                     colClasses = c("integer","integer","character",
                                    "numeric","numeric","numeric",
                                    "factor","myDateTime","factor",
                                    "myDateTime","character","character",
                                    "integer","myDateTime","numeric",
                                    "factor","character","character",
                                    "factor","factor","factor",
                                    "myDateTime","factor","myDateTime",
                                    "numeric","numeric","numeric",
                                    "numeric","numeric","numeric",
                                    "numeric","numeric","numeric",
                                    "numeric","numeric","numeric",
                                    "factor","factor","factor",
                                    "integer","integer","character"))

##### Load Seller Charges Data #####

##### duplicated tracking number #####
duplicatedTrackingNumber <- filter(allInvoice,duplicated(allInvoice))$tracking_number
duplicated <- allInvoice %>% 
    filter(tracking_number %in% duplicatedTrackingNumber) %>%
    arrange(tracking_number)
write.csv(duplicated, file = file.path(outputVentureFolder,"duplicatedInvoiceRecords.csv"),
          row.names = FALSE)

invoiceMappedOMS <- left_join(allInvoice, oms_data,
                              by=("tracking_number"))

##### not mapped #####
notmapped <- filter(invoiceMappedOMS, is.na(order_nr))
write.csv(select(notmapped, 1:28), file = file.path(outputVentureFolder,"notMapped.csv"),
          row.names = FALSE)

##### mapped #####
mapped <- filter(invoiceMappedOMS, !is.na(order_nr) & !(tracking_number %in% duplicatedTrackingNumber))
mapped %<>% 
    mutate(carrying_fee=ifelse(is.na(carrying_fee),0,carrying_fee)) %>%
    mutate(redelivery_fee=ifelse(is.na(redelivery_fee),0,redelivery_fee)) %>%
    mutate(rejection_fee=ifelse(is.na(rejection_fee),0,rejection_fee)) %>%
    mutate(special_area_fee=ifelse(is.na(special_area_fee),0,special_area_fee)) %>%
    mutate(special_handling_fee=ifelse(is.na(special_handling_fee),0,special_handling_fee)) %>%
    mutate(cod_fee=ifelse(is.na(cod_fee),0,cod_fee)) %>%
    mutate(insurance_fee=ifelse(is.na(insurance_fee),0,insurance_fee)) %>%
    group_by(tracking_number) %>%
    mutate(Paidrpice_percent=paid_price/sum(paid_price)) %>%
    ungroup() %>%
    mutate(item_carrier_fee=(carrying_fee+redelivery_fee+rejection_fee+special_area_fee+special_handling_fee)/item_in_package) %>%
    mutate(item_cod_fee=cod_fee*Paidrpice_percent) %>%
    mutate(item_insurance_fee=insurance_fee*Paidrpice_percent)

final_mapped <- select(mapped,
                       id_sales_order_item,item_number,business_unit,
                       paid_price,shipping_fee,bulk_fee,shipped_status,
                       item_shipped_date,current_item_status,item_delivery_date,
                       sku,regional_category_key,order_nr,
                       order_created_date,order_grand_total,payment_method,
                       package_number=package_number.y,tracking_number,shipment_provider_name,
                       gtm_routing,package_shipping_status,package_shipping_date,
                       package_delivery_status,package_delivery_date,item_length_2,
                       item_width_2,item_height_2,item_weight,
                       item_dim_weight,item_dimension_status,item_carrier_fee,
                       item_cod_fee,item_insurance_fee,item_dw_carrier_fee,
                       package_dw_value,package_weight_value,package_dimension_status,
                       package_weight_status,matched_tag,item_in_order,
                       item_in_package,warehouse_name)

write.csv(final_mapped, file = file.path(outputVentureFolder,"mapped.csv"),
          row.names = FALSE)

##### data quality quick check #####
invoiceTracking <- allInvoice$tracking_number
invoiceTracking2 <- allInvoice$tracking_number_rts
omsTracking <- oms_data$tracking_number
sum(!(invoiceTracking %in% omsTracking))
sum(invoiceTracking2 %in% omsTracking)
sum(invoiceTracking == invoiceTracking2)
sum(duplicated(allInvoice$tracking_number))
View(filter(allInvoice,duplicated(tracking_number)))
sum(duplicated(final_mapped$id_sales_order_item))
##### Process Data #####
    
##### Output #####