suppressMessages({
  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(data.table)
  library(tidyr)
  library(logging)
})

dateLog <- format(Sys.time(), "%Y%m%d%H%M")

tryCatch({
  
  args <- commandArgs(trailingOnly = TRUE)
  venture <- args[1]
  dimWeightFactor <- as.numeric(args[2])
  
  ventureShort <- switch (venture,
                          "Indonesia" = "ID",
                          "Malaysia" = "MY",
                          "Philippines" = "PH",
                          "Singapore" = "SG",
                          "Thailand" = "TH",
                          "Vietnam" = "VN"
  )
  
  reportName <- paste0("Passthrough_", venture)
  consoleLog <- paste0("Passthrough_", venture,".Console")
  
  addHandler(writeToFile, logger = reportName,
             file=file.path("3_Script/2_Log/",
                            paste0(ventureShort,"_passthrough_",dateLog,".csv")))
  addHandler(writeToConsole , logger=consoleLog)
  
  loginfo(paste0(venture, " - Passthrough Start"), logger = consoleLog)
  loginfo("Initial Setup", logger = reportName)
  
  
  runningFolderName <- format(Sys.Date(),"%Y%m%d")
  
  source("3_Script/1_Code/01_Loading/fn_loadOMSData.R")
  source("3_Script/1_Code/01_Loading/fn_LoadSKUDimWeight.R")
  source("3_Script/1_Code/01_Loading/fn_LoadNonLEXCost.R")
  source("3_Script/1_Code/01_Loading/fn_LoadLexCost.R")
  source("3_Script/1_Code/01_Loading/fn_LoadSellerCharges.R")
  source("3_Script/1_Code/03_Processing/fn_GetItemCostOMSData.R")
  source("3_Script/1_Code/03_Processing/fn_GetLEXItemCost.R")
  source("3_Script/1_Code/03_Processing/fn_MapSellerCharges.R")
  source("3_Script/1_Code/03_Processing/fn_CalculatePassthrough.R")
  
  omsFolder <- file.path("1_Input",venture,"OMS_Data")
  sellerChargesPath <- file.path("1_Input",venture,"Seller_Charges_Data")
  manualData <- file.path("1_Input",venture,"Manual_Data")
  LEXCostPath <- file.path("1_Input",venture,"LEX_Cost",
                           paste0(ventureShort,"_LEX_Cost.csv"))
  skuDimensionPath <- file.path("1_Input",venture,"SKU_Dimension")
  outputFolder <- file.path("2_Output",runningFolderName)
  
  if(!dir.exists(outputFolder)){
    dir.create(outputFolder)
  }
  outputFolder <- file.path(outputFolder,venture)
  
  if(!dir.exists(outputFolder)){
    dir.create(outputFolder)
  }
  
  loginfo("Start Loading OMS Data", logger = "Passthrough.Console")
  if (!file.exists(file.path("3_Script/3_RData", venture, "OMS_Data.RData"))){
    OMS_Data <- loadOMSData(omsFolder)
    save(OMS_Data, file = file.path("3_Script/3_RData", venture, "OMS_Data.RData"))
  } else {
    load(file.path("3_Script/3_RData", venture, "OMS_Data.RData"))
  }
  loginfo("Start Loading SKU Dimension Data", logger = "Passthrough.Console")
  SKUDimWeight <- LoadSKUDimWeight(skuDimensionPath, dimWeightFactor)
  loginfo("Start Loading Cost Data", logger = "Passthrough.Console")
  nonLEXCostData_raw <- LoadNonLEXCostData(costFilePath = manualData)
  nonLEXItemCost <- GetItemCostOMSData(nonLEXCostData_raw, OMS_Data, SKUDimWeight)
  LEXCost_raw <- LoadLEXCost(costFilePath = LEXCostPath)
  LEXItemCost <- GetLEXItemCost(LEXCost_raw, OMS_Data, SKUDimWeight)
  allItemCost <- rbind_list(nonLEXItemCost, LEXItemCost)
  
  loginfo("Start Loading Seller Charges Data", logger = "Passthrough.Console")
  sellerCharges_raw <- LoadSellerCharges(sellerChargesPath)
  sellerChargesMapped <- MapSellerCharges(sellerCharges_raw, OMS_Data, SKUDimWeight)
  mappedCostCharges <- MapCostCharges(allItemCost, sellerChargesMapped)
  passthroughData <- CalculatePassthrough(mappedCostCharges)
  
  WritePassthrougCSV(passthroughData)
  
  loginfo(paste0(venture, " - Passthrough Done!!!"), logger = "Passthrough.Console")
},error = function(err){
  logerror(err, logger = "Passthrough")
  logerror("PLease send 3_Script/Log folder to Regional OPS BI for additional support",
           logger = "Passthrough.Console")
})