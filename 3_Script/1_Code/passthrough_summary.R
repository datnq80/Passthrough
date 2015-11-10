loadPassthroughData <- function(filePath, ventureName){
  require(dplyr)
  
  allData <- NULL
  for (ifile in list.files(filePath)){
    currentData <- read.csv(file.path(filePath, ifile),
                     stringsAsFactors = FALSE)
    if (is.null(allData)) {
      allData <- tbl_df(currentData)
    }
    else {
      allData <- rbind_list(allData, currentData)
    }
  }
  
  allData %<>% mutate(venture=ventureName)
  allData
}

ID <- loadPassthroughData("../../2_Output/20151106/Indonesia", "Indonesia")
MY <- loadPassthroughData("../../2_Output/20151106/Malaysia", "Malaysia")
PH <- loadPassthroughData("../../2_Output/20151106/Philippines", "Philippines")
TH <- loadPassthroughData("../../2_Output/20151106/Thailand", "Thailand")
VN <- loadPassthroughData("../../2_Output/20151106/Vietnam", "Vietnam")

allData <- rbind_list(ID, MY, PH, TH, VN)

View(allData %>% group_by(venture, Month) %>%
  filter(business_unit != "Retail") %>%
  summarize(cost = sum(Item_Cost, na.rm = TRUE),
            sellerCharged = sum(Item_SellerCharged, na.rm = TRUE),
            passthrough = sellerCharged/ cost))
