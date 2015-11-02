THsellerChargesRaw <- 
  read.csv("../../1_Input/Thailand/Seller_Charges_Data/sellerCharges.csv")

THsellerCharges <- THsellerChargesRaw %>%
  group_by(Tracking.Number) %>%
  summarize(value=sum(Cost.Without.VAT))

THsellerCharges %<>%
  left_join(OMS_Data, by=c("Tracking.Number"="tracking_number")) %>%
  group_by(Tracking.Number) %>%
  summarize(src_id=first(id_sales_order_item),
            value=first(value))

THsellerChargesRev <- THsellerCharges %>%
  group_by(src_id) %>%
  summarize(value=-sum(value)) %>%
  filter(!is.na(src_id))

SellerCharges <- THsellerChargesRev
View(filter(SellerCharges, is.na(src_id)))
     
