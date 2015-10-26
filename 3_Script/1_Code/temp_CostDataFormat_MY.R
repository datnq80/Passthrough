setClass("myNumeric")
setAs("character","myNumeric", function(from) as.numeric(gsub('[",]','',from)))

rawData <- read.csv("../../1_Input/20151014/Malaysia/MY_Aug_Raw_Data.csv",
                    stringsAsFactors = FALSE,
                    col.names = c("Tracking.Number","Supplier Type","tracking_number_rts",
                                  "package_chargeable_weight","carrying_fee"),
                    colClasses = c("character","character","character",
                                   "myNumeric","myNumeric"))


CostData <- rawData %>%
    mutate(Pickup.Date=NA,
           Delivery.Company=NA,
           Package.Number=NA) %>%
    select(Delivery.Company,Tracking.Number,Package.Number,
           Pickup.Date, Cost=carrying_fee) %>%
    group_by(Delivery.Company, Tracking.Number,Package.Number,Pickup.Date) %>%
    summarize(Total_Cost=sum(Cost, na.rm=TRUE))

write.csv(CostData, "../../1_Input/20151014/Malaysia/MY_Cost_Data_Formmatted.csv",
          row.names = FALSE)