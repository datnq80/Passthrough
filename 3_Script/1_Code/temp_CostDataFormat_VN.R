rawData <- read.csv("../../1_Input/20151014/Vietnam/AUG shipping fee in template to send.csv",
                    stringsAsFactors = FALSE,
                    col.names = c("Tracking.Number","Package.Number",
                                 "Delivery.Company","Cost","Cost.Type",
                                 "Note","Nono"),
                    colClasses = c("character","character",
                                   "factor","numeric","factor",
                                   "factor","character"))

sum(duplicated(rawData$Tracking.Number))

rawDataCheck <- rawData %>% filter(!duplicated(rawData) & !is.na(Tracking.Number) & Note!="MP (not in KPI of aug)")

CostData <- rawDataCheck %>%
    mutate(Pickup.Date=NA) %>%
    select(Delivery.Company,Tracking.Number,Package.Number,
           Pickup.Date, Cost) %>%
    group_by(Delivery.Company, Tracking.Number,Package.Number,Pickup.Date) %>%
    summarize(Total_Cost=sum(Cost, na.rm=TRUE))

write.csv(CostData, "../../1_Input/20151014/Vietnam/VN_Cost_Data_Formmatted.csv",
          row.names = FALSE)