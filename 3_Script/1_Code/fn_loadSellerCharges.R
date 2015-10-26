loadSellerCharges <- function(sellerChargesFilePath){
    
    require(dplyr, quietly = TRUE)
    require(tools, quietly = TRUE)
    require(magrittr, quietly = TRUE)
    require(methods, quietly= TRUE)
    
    setClass('myDateTime')
    setAs("character","myDateTime", function(from) as.POSIXct(from, format="%Y-%m-%d %H:%M:%S")) #covnert datetime string to datetime    
    
    sellerCharges <- NULL
    for (file in list.files(sellerChargesFilePath)){
        if (file_ext(file)=="csv"){
            currentFileData <- read.csv(file.path(sellerChargesFilePath,file),
                                        stringsAsFactors = FALSE,
                                        col.names = c("sc_id_sales_order_item","src_id","id_transaction",
                                                      "fk_seller","fk_transaction_type","is_unique",
                                                      "transaction_source","fk_user","description",
                                                      "value","taxes_vat","taxes_wht","ref",
                                                      "ref_date","number","fk_transaction_statement",
                                                      "created_at","updated_at","fk_qc_user"),
                                        colClasses = c("integer","integer","integer",
                                                       "integer","integer","character",
                                                       "character","integer","character",
                                                       "numeric","numeric","numeric","integer",
                                                       "character","character","integer",
                                                       "myDateTime","myDateTime","character"))
            
            if (is.null(sellerCharges))
                sellerCharges <- currentFileData
            else
                sellerCharges <- rbind_list(sellerCharges,currentFileData)
        }
    }
    sellerCharges
}