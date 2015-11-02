VNsellerChargesRaw <- 
  read.csv("../../1_Input/Vietnam/Seller_Charges_Data/VN seller center charges-Aug (clean).csv",
           col.names = c("Tracking.Number","Cost.Without.VAT"))

VNsellerCharges <- VNsellerChargesRaw %>%
  mutate(Tracking.Number=gsub("\\*","",Tracking.Number)) %>%
  group_by(Tracking.Number) %>%
  summarize(value=sum(Cost.Without.VAT))

VNsellerCharges %<>%
  left_join(OMS_Data, by=c("Tracking.Number"="tracking_number")) %>%
  group_by(Tracking.Number) %>%
  summarize(src_id=first(id_sales_order_item),
            value=first(value))

VNsellerChargesRev <- VNsellerCharges %>%
  group_by(src_id) %>%
  summarize(value=sum(value)) %>%
  filter(!is.na(src_id))

SellerCharges <- VNsellerChargesRev


trackingNotMapped <- filter(VNsellerCharges, is.na(src_id))
OMS_pkg <- OMS_Data$package_number
sum(trackingNotMapped$Tracking.Number %in% OMS_pkg)

gsub("\\*","","*Tracking.Number*")
