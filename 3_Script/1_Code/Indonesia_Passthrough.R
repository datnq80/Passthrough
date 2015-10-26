library(dplyr)
library(magrittr)
library(lubridate)
library(lubridate)

venture <- "Indonesia"
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
if(!dir.exists(file.path(outputFolder,venture))){
  dir.create(file.path(outputFolder,venture))
}

Cost <- loadCostData(file.path(runningFolder,venture,paste0(ventureShort,"_Cost_Data_Formmatted.csv")))
SellerCharges <- loadSellerCharges(file.path(sellerCharged,venture))
OMS_Data <- loadOMSData(omsVentureFolder)

CombinedData <- right_join(OMS_Data, Cost,
                           by=c("tracking_number"="Tracking.Number"))

CombinedData %<>% 
  mutate(OMS_Missing=ifelse(is.na(order_nr),"Missing","Okay"))

CombinedData %<>%
  group_by(tracking_number) %>%
  mutate(Item_Cost=(Cost/n())) %>%
  ungroup()

#CombinedData_MP <- filter(CombinedData,
#                          business_unit=="MP")

CombinedData %<>%
  group_by(tracking_number) %>%
  mutate(Item_Cost=Cost/n()) %>%
  ungroup()

SellerChargesNarrow <- SellerCharges %>% 
  group_by(src_id,sc_id_sales_order_item) %>%
  summarize(Total.Charges=sum(value))

CombinedDataCharges <- left_join(CombinedData, SellerChargesNarrow,
                                 by=c("id_sales_order_item"="src_id"))

PassthroughData <- CombinedDataCharges %>%
  mutate(Month.Report="201508",
         Total.Charges=abs(Total.Charges)) %>%
  select(Month.Report,order_nr,id_sales_order_item,
         tracking_number,package_number,Shipped_Date,
         Cancelled_Date,Delivered_Date,Delivery.Company,
         unit_price,Item_Cost, Total.Charges,
         SC_SOI_ID,OMS_Missing) %>%
  mutate(Item_Cost=ifelse(is.na(Item_Cost),0,Item_Cost),
         Total.Charges=ifelse(is.na(Total.Charges),0,Total.Charges)) %>%
  mutate(Difference=Total.Charges-Item_Cost,
         Passthrough=Total.Charges/Item_Cost) %>%
  select(Month.Report,order_nr,id_sales_order_item,
         tracking_number,package_number,Shipped_Date,
         Cancelled_Date,Delivered_Date,Delivery.Company,
         unit_price,Item_Cost, Total.Charges,
         Difference,Passthrough, SC_SOI_ID,OMS_Missing)

write.csv(PassthroughData, file = file.path(outputFolder,venture,paste0(ventureShort,"_PassthroughData.csv")),
          row.names = FALSE)

View(filter(SellerCharges, !(sc_id_sales_order_item %in% CombinedDataCharges$sc_id_sales_order_item) &
              format(created_at,"%Y-%m")=="2015-08"))

SellerCharges %>% group_by(Month=format(created_at,"%Y-%m")) %>% summarize(total=sum(value))
PassthroughData %>% group_by(Month=format(created_at,"%Y-%m")) %>% summarize(total=sum(value))


sc_id <- SellerCharges$src_id
oms_id <- CombinedData$id_sales_order_item
sum(sc_id %in% oms_id)
