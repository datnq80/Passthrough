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
  
  costData <- data.frame(Delivery_Company=character(),
                         Tracking_Number=character(),
                         Package_Number=character(),
                         Pickup_Date=as.POSIXct(character()),
                         Cost=numeric())
  
  for (file in list.files(costFilePath)){
    if(file_ext(file)=="csv"){
      currentFile <- read.csv(file.path(costFilePath, file),
                              stringsAsFactors = FALSE,
                              col.names = c("Delivery_Company","Tracking_Number",
                                            "Package_Number","Pickup_Date","Cost"),
                              colClasses = c("character","character",
                                             "character","myDate","myNumeric"))
      costData <- rbind_list(costData,currentFile)
    }
  }
  costData %<>%
    group_by(Tracking_Number,
             Package_Number) %>%
    summarize(Delivery_Company=last(Delivery_Company),
              Pickup_Date=last(Pickup_Date),
              Cost=sum(Cost))
  costData
}