library(dplyr)
library(magrittr)
library(lubridate)
library(lubridate)

venture <- "Malaysia"
runningFolderName <- format(Sys.Date(),"%Y%m%d")

source("../1_Code/fn_loadOMSData.R")
source("../1_Code/fn_loadSellerCharges.R")
source("../1_Code/fn_loadCostData.R")

omsFolder <- file.path("../../1_Input",venture,"OMS_Data")
sellerCharged <- file.path("../../1_Input",venture,"Seller_Charges_Data")
manualData <- file.path("../../1_Input",venture,"Manual_Data")
outputFolder <- file.path("../../2_Output",runningFolderName)
if(!dir.exists(outputFolder)){
  dir.create(outputFolder)
}
outputFolder <- file.path(outputFolder,venture)
if(!dir.exists(outputFolder)){
  dir.create(outputFolder)
}

Cost <- loadCostData(manualData)
SellerCharges <- loadSellerCharges(sellerCharged)
OMS_Data <- loadOMSData(omsFolder)

Cost_OMS_Mapped <- left_join(Cost, OMS_Data,
                             by=c("Tracking_Number"="Tracking_number"))

Cost_OMS_SellerCharge <- left_join(Cost_OMS_Mapped, SellerCharges,
                                   by=c("id_sales_order_item"="src_id"))

Passthrough_Data <- Cost_OMS_SellerCharge %>% group_by(Tracking_Number) %>%
  mutate(Item_Cost=Cost/n()) %>%
  mutate(Item_SellerCharged=sum(value,na.rm=TRUE)/n()) %>%
  ungroup()

Passthrough_Data %<>%
  select(id_sales_order_item, SC_SOI_ID, order_nr,
         business_unit, payment_method, Tracking_Number,
         Package_Number, RTS_Date, Shipped_Date,
         Cancelled_Date, Delivered_Date, shipment_provider_name,
         Seller_Code, tax_class,
         Item_Cost, Item_SellerCharged)

write.csv(Passthrough_Data, file.path(outputFolder,"MY_Passthrough_data.csv"),
          row.names = FALSE)

Test <- Cost_OMS_SellerCharge %>% group_by(Tracking_Number) %>%
  mutate(Item_Cost=Cost/n()) %>%
  mutate(Item_SellerCharged=sum(value,na.rm=TRUE)/n()) %>%
  group_by(Tracking_Number,Cost) %>%
  summarize(Counts=n(),
            Cost_calculate=sum(Item_Cost, na.rm=TRUE))

Passthrough_Data %>%
  filter(business_unit=="MP") %>%
  group_by(Shipped_Month=month(Shipped_Date)) %>%
  summarize(Cost=sum(Item_Cost),
            Charged=sum(Item_SellerCharged))

SellerCharges %>%
  group_by(Txn_Date=format(created_at,"%Y-%m")) %>%
  summarize(Charged=sum(abs(value)))