library(dplyr)
library(magrittr)
library(lubridate)
library(lubridate)

venture <- "Vietnam"
ventureShort <- switch (venture,
                        "Indonesia" = "ID",
                        "Malaysia" = "MY",
                        "Philippines" = "PH",
                        "Singapore" = "SG",
                        "Thailand" = "TH",
                        "Vietnam" = "VN"
)
runningFolderName <- format(Sys.Date(),"%Y%m%d")

source("../1_Code/fn_loadCostData.R")
source("../1_Code/fn_loadOMSData.R")
source("../1_Code/fn_loadSellerCharges.R")
source("../1_Code/fn_loadManualSellerCharges.R")

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
OMS_Data <- loadOMSData(omsFolder)
SellerCharges <- LoadManualSellerCharges(sellerCharged)

Cost_OMS_Mapped <- left_join(Cost, OMS_Data,
                             by=c("Tracking_Number"="Tracking_number"))

Cost_OMS_SellerCharge <- left_join(Cost_OMS_Mapped, SellerCharges,
                                   by=c("id_sales_order_item"="src_id"))

Passthrough_Data <- Cost_OMS_SellerCharge %>% 
  group_by(Tracking_Number) %>%
  mutate(Item_Cost=Cost/n()) %>%
  group_by(Tracking_Number) %>%
  mutate(Item_SellerCharged=value) %>%
  ungroup()
Passthrough_Data %<>%
  mutate(Item_SellerCharged=ifelse(is.na(Item_SellerCharged),0,Item_SellerCharged)) %>%
  mutate(Is_Charged=ifelse(Item_SellerCharged==0,"No_Charged","Charged"))

Passthrough_Data %<>%
  select(Month,id_sales_order_item, SC_SOI_ID, order_nr,
         business_unit, payment_method, Tracking_Number,
         Package_Number, RTS_Date, Shipped_Date,
         Cancelled_Date, Delivered_Date, shipment_provider_name,
         Seller_Code, tax_class,
         Item_Cost, Item_SellerCharged,
         Is_Charged)

monthReport <- unique(Passthrough_Data$Month)

for (iMonth in monthReport){
  write.csv(filter(Passthrough_Data, Month==iMonth), 
            file.path(outputFolder,
                      paste0(ventureShort,"_",iMonth,"_Passthrough_data.csv")),
            row.names = FALSE)
}

