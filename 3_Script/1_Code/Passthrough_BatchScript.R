library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)
library(tidyr)

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

OMS_Data <- loadOMSData(omsFolder)
Item_Cost <- loadCostData(manualData, OMS_Data)
SellerCharges <- LoadManualSellerCharges(sellerCharged, OMS_Data)

firstMonth <- Item_Cost %>%
  select(id_sales_order_item,Month) %>%
  arrange (Month) %>%
  filter(!duplicated(id_sales_order_item), !is.na(id_sales_order_item))
SellerChargesMonth <- SellerCharges %>% 
  left_join(firstMonth, by=c("id_sales_order_item")) 

Item_Cost_SellerCharge <- left_join(Item_Cost, SellerChargesMonth,
                                   by=c("id_sales_order_item",
                                        "Month" = "Month"))

Passthrough_Data <- Item_Cost_SellerCharge %>% 
  rename(Item_SellerCharged = value) %>%
  mutate(Item_SellerCharged = ifelse(is.na(Item_SellerCharged),0,Item_SellerCharged))

Passthrough_Data %<>% arrange(Month) %>%
  group_by(id_sales_order_item) %>%
  mutate(accumCost = cumsum(Item_Cost),
         accumCharges = cumsum(Item_SellerCharged)) %>%
  ungroup()
Passthrough_Data <- data.table(Passthrough_Data)
Passthrough_Data[, Index := 1:.N, by = id_sales_order_item]
Passthrough_Data <- tbl_df(Passthrough_Data)
Passthrough_Data %<>%
  ungroup() %>% group_by(id_sales_order_item) %>%
  mutate(Item_SellerCharged = ifelse(accumCharges + accumCost + Item_Cost < 0,
                                     ifelse(Index==n(), accumCharges + accumCost - Item_Cost , - Item_Cost),
                                     ifelse(accumCharges + accumCost - Item_Cost < 0,
                                            accumCharges + accumCost - Item_Cost, 0)))

Passthrough_Data %<>%
  ungroup() %>%
  mutate(Item_SellerCharged = ifelse( is.na(Item_SellerCharged), 0, Item_SellerCharged)) %>%
  mutate(Is_Charged=ifelse(Item_SellerCharged == 0, "No_Charged", "Charged")) %>%
  mutate(Remark=ifelse(is.na(business_unit) | business_unit == "Retail", NA, Is_Charged))

Passthrough_Data %<>%
  select(Month,id_sales_order_item, SC_SOI_ID, order_nr,
         business_unit, payment_method, Tracking_Number,
         Package_Number, RTS_Date, Shipped_Date,
         Cancelled_Date, Delivered_Date, shipment_provider_name,
         Seller_Code, tax_class,
         Item_Cost, Item_SellerCharged,
         Remark)

monthReport <- unique(Passthrough_Data$Month)

for (iMonth in monthReport){
  write.csv(filter(Passthrough_Data, Month==iMonth), 
            file.path(outputFolder,
                      paste0(ventureShort,"_",iMonth,"_Passthrough_data.csv")),
            row.names = FALSE)
}

Passthrough_Data %>% group_by(format(RTS_Date, "%Y%m"), business_unit) %>% summarize(ItemCostTotal=sum(Item_Cost),
                                                                                     SellerCharges=sum(Item_SellerCharged))