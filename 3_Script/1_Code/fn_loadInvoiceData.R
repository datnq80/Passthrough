loadInvoiceData <- function(invoiceFolder){
    allInvoice <- NULL
    for (file in list.files(file.path(invoiceFolder))){
        if (file_ext(file)=="csv"){
            invoice <- read.csv(file.path(invoiceFolder,file),
                                stringsAsFactors = FALSE,
                                col.names = c("line_id","3pl_name","package_pickup_date",
                                              "package_pod_date","invoice_number","package_number",
                                              "tracking_number","tracking_number_rts","order_number",
                                              "package_volume","package_height","package_width",
                                              "package_length","package_weight","package_chargeable_weight",
                                              "carrying_fee","redelivery_fee","rejection_fee",
                                              "cod_fee","special_area_fee","special_handling_fee",
                                              "insurance_fee","vat","origin_branch",
                                              "destination_branch","delivery_zone_zip_code","rate_type"),
                                colClasses = c("numeric","character","myDateTime",
                                               "myDateTime","character","character",
                                               "character","character","character",
                                               "numeric","numeric","numeric",
                                               "numeric","numeric","numeric",
                                               "numeric","numeric","numeric",
                                               "numeric","numeric","numeric",
                                               "numeric","numeric","character",
                                               "character","character","character"))
            invoice %<>% 
                mutate(Invoice_file=file) %>%
                filter(!(is.na(tracking_number) | tracking_number==""))
            if (is.null(allInvoice))
                allInvoice <- invoice
            else
                allInvoice <- rbind_list(allInvoice,invoice)
        }
    }
    allInvoice
}