select
	 id_sales_order_item
	,item_number
	,business_unit
	,paid_price
	,shipping_fee
	,bulk_fee
	,shipped_status
	,item_shipped_date
	,current_item_status
	,item_delivery_date
	,sku
	,order_nr
	,order_created_date
	,order_grand_total
	,payment_method
	,package_number
	,tracking_number
	,shipment_provider_name
	,gtm_routing
	,package_shipping_status
	,package_shipping_date
	,package_delivery_status
	,package_delivery_date
	,null as item_length_2
	,null as item_width_2
	,null as item_height_2
	,null as item_weight
	,null as item_dim_weight
	,null as item_dimension_status
	,null as item_carrier_fee
	,null as item_cod_fee
	,null as item_insurance_fee
	,null as item_dw_carrier_fee
    ,null as package_dw_value
    ,null as package_weight_value
	,null as package_dimension_status
	,null as package_weight_status
	,null as matched_tag
	,if(gtm_routing = 'drop_shipping',null,mwh_name) as warehouse_name
from
(
	select
		  base0.*                
	from
	(
	select
	 soi.id_sales_order_item
	,soi.bob_id_sales_order_item as item_number
	,if(soi.fk_marketplace_merchant is null,'Retail','MP') as business_unit
	,ifnull(soi.paid_price,0) as paid_price
	,ifnull(soi.shipping_fee,0) as shipping_fee
	,ifnull(soi.shipping_surcharge,0) as bulk_fee
	,if(sum(if(soi_hist_status.name = 'shipped',1,0))>0,'shipped',null) as shipped_status
	,max(if(soi_hist_status.name = 'shipped',soi_hist.created_at,null)) as item_shipped_date
	,soi_status.name as current_item_status
	,ifnull(soi.real_delivery_date,max(if(soi_hist_status.name = 'delivered',soi_hist.created_at,null))) as item_delivery_date
	,soi.sku
	,so.order_nr
	,so.created_at as order_created_date
	,so.grand_total as order_grand_total
	,so_payment.name as payment_method
	,pac.package_number
	,pacd.tracking_number
	,shipcom.shipment_provider_name
	,case
		when soi.fk_marketplace_merchant is not null then shiptype.name
		when soi.fk_marketplace_merchant is null and queue_item.fk_sales_order_item is null then 'warehouse'
		else 'cross_docking' end as gtm_routing
	,if(sum(if(pac_hist_status.name = 'shipped',1,0))>0,'shipped',null) as package_shipping_status
	,max(if(pac_hist_status.name = 'shipped',pac_hist.created_at,null)) as package_shipping_date
	,if(sum(if(pac_hist_status.name = 'delivered',1,0))>0,'delivered',null) as package_delivery_status
	,max(if(pac_hist_status.name = 'delivered',pac_hist.created_at,null)) as package_delivery_date
    ,mwh.name as mwh_name
    
    ,pac.id_package
    ,so.id_sales_order
    
	from ims_sales_order_item as soi
	inner join oms_warehouse as mwh
		on mwh.id_warehouse = soi.fk_mwh_warehouse
	inner join oms_shipping_type as shiptype
		on shiptype.id_shipping_type = soi.fk_shipping_type
	left join oms_queue_sales_order_item as queue_item
		on queue_item.fk_sales_order_item = soi.id_sales_order_item
	inner join ims_sales_order_item_status as soi_status
		on soi_status.id_sales_order_item_status = soi.fk_sales_order_item_status
	inner join ims_sales_order as so
		on so.id_sales_order = soi.fk_sales_order
	inner join ims_sales_order_process as so_payment
		on so_payment.id_sales_order_process = so.fk_sales_order_process
	inner join ims_sales_order_item_status_history as soi_hist
		on soi_hist.fk_sales_order_item = soi.id_sales_order_item
	inner join ims_sales_order_item_status as soi_hist_status
		on soi_hist_status.id_sales_order_item_status = soi_hist.fk_sales_order_item_status
	inner join oms_package_item as paci
		on paci.fk_sales_order_item = soi.id_sales_order_item
		and paci.isdeleted = 0
	inner join oms_package as pac
		on pac.id_package = paci.fk_package
		and pac.isdeleted = 0
	inner join oms_package_dispatching as pacd
		on pacd.fk_package = pac.id_package
	inner join oms_shipment_provider as shipcom
		on shipcom.id_shipment_provider = pacd.fk_shipment_provider
	inner join oms_package_status_history as pac_hist
		on pac_hist.fk_package = pac.id_package
	inner join oms_package_status as pac_hist_status
		on pac_hist_status.id_package_status = pac_hist.fk_package_status
	where date(soi.created_at) between date('2015-05-01') and date('2015-08-31')
	group by soi.id_sales_order_item
	) as base0  
) as base1