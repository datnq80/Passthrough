loadCostData <- function(costFilePath){
  
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
  
  costData <- data.frame(Delivery_Company=character(),
                         Tracking_Number=character(),
                         Package_Number=character(),
                         Pickup_Date=as.POSIXct(character()),
                         Cost_VAT=numeric(),
                         Cost_Ex_VAT=numeric(),
                         VAT=numeric(),
                         Month=character())
  
  for (file in list.files(costFilePath)){
    if(file_ext(file)=="csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Delivery_Company","Tracking_Number",
                                            "Package_Number","Pickup_Date","Cost_VAT",
                                            "Cost_Ex_VAT","VAT","Month"),
                              colClasses = c("character","myTrackingNumber",
                                             "character","myDate","myNumeric",
                                             "myNumeric","myNumeric","character"))
      costData <- rbind_list(costData,currentFile)
    }
  }
  costData %<>%
    arrange(Month) %>%
    group_by(Tracking_Number) %>%
    summarize(Package_Number=first(Package_Number),
              Delivery_Company=last(Delivery_Company),
              Pickup_Date=last(Pickup_Date),
              Cost=sum(Cost_Ex_VAT, na.rm=TRUE),
              Month=first(Month))
  
  costData %<>% filter(Cost>0.0)
  
  costData
}