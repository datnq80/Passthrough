library(dplyr)
library(magrittr)
library(reshape2)

setClass('myDate')
setAs("character","myDate", function(from) as.POSIXct(substr(from,1,10), format="%Y-%m-%d")) #covnert datetime string to datetime


TH_RawData <- read.csv("../../1_Input/20151014/Thailand/TH_Rawdata.csv",
                       stringsAsFactors = FALSE,
                       col.names = c("Delivery.Company", "Pickup.Date",
                                     "Tracking.Number","Cost.Type",
                                     "Freight.Charge","Business.Unit"),
                       colClasses = c("character","myDate",
                                      "character","factor",
                                      "numeric","factor"))

TH_MP_RawData <- TH_RawData %>% filter(Business.Unit=="MP")

duplicatedValue <- filter(TH_MP_RawData, duplicated(TH_MP_RawData))
TH_MP_RawData_noDup <- filter(TH_MP_RawData, !duplicated(TH_MP_RawData))

THCostData <- TH_MP_RawData_noDup %>%
    group_by(Delivery.Company,Tracking.Number,Cost.Type,Pickup.Date) %>%
    summarize(Freight.Charge=sum(Freight.Charge, na.rm=TRUE))

THCostDataWide <- dcast(THCostData, Delivery.Company + Tracking.Number + Pickup.Date ~ Cost.Type,
                        value.var = "Freight.Charge")

THCostDataTotal <- TH_MP_RawData_noDup %>%
    mutate(Package.Number=NA) %>%
    group_by(Delivery.Company, Tracking.Number,Package.Number,Pickup.Date) %>%
    summarize(Freight.Charge=sum(Freight.Charge))

write.csv(THCostDataTotal,"../../1_Input/20151014/Thailand/TH_Cost_Data_Formmatted.csv",
          row.names = FALSE)

write.csv(duplicatedValue, "../../2_Output/20151014/Thailand/TH_Cost_DataRaw_Duplicated.csv",
          row.names = FALSE)