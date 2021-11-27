# clean_phone_number: Cleaning up and formatting phone numbers -----------------------------------------
# App(s): Main Shares, Species Assignment, Home Delivery
clean_colname <- function(df, type) {
  

  if(type == "Orders") {
    colName1 <- "Order ID"
    
    colNames <- c(
      "order_id", "line_item_id", "shopify_order", "order_date", "order_time", "email", 
      "product_name", "variant_name",  "quantity", "order_tags", "vendor", "customer_name", 
      "opt_out", "location", "pickup_site", "source",    "line_item_name", "weight_lb", 
      "product_tags", "sku", "price", "order_note", "order_canceled"
    )
    
    position <- c(1:23)

  }
  else {
    colName1 <- "Customer ID"
    colNames <- c(
      "customer_id", "name", "email", "address", "city", "state", 
      "country", "zip", "phone", "member_tags", "note")
    
    position <- c(1:11)
      
  }
  
  df %>%
    select(all_of(position)) %>% 
    `colnames<-`(colNames) %>% 
    filter(!str_detect(colName1, regex(.[[1]], ignore_case = T))) 
  
}



# clean_oyster: Combine rows of Oyster orders (Live King Tide Oysters) into a single row  ----------------
# App(s): Special Orders & Early Deadline Orders
# clean_oyster <- function(df) {
#   df %>%
#     
#     mutate(is_oyster = str_detect(tolower(share_type), "oysters"))%>%
#     
#     group_by(name, share_type) %>%
#     mutate(row_index = min(1, n()):n()) %>% 
#     
#     ## Live King Tide Oysters - $2 -> Live King Tide Oysters - $2 x 3
#     mutate(order = ifelse(is_oyster, paste(order, "x", n()), order)) %>% 
#     
#     ## calculate the total price for oyster orders
#     mutate(price = ifelse(is_oyster, price * n(), price)) %>% ungroup() %>%
#     
#     ## for oyster orders, only keep the first row
#     filter(!is_oyster | row_index == 1) %>%
#     select(-row_index, -is_oyster)
# }


# clean_subscription: data cleaning for shopify_orders -------------------------------------
# App(s): Main Shares, Species Assignment

  
clean_subscription <- function(df) {
  
  df %>%
    #selecting only subscription/recharge orders
    filter(str_detect(tolower(order_tags), "subscription"),
           str_detect(tolower(product_name), "share"),
           is.na(order_canceled)) %>% 
    
    #getting share_size
    separate(product_name, "share_size", sep = " Share") %>%
    
    mutate(shopify_order = as.numeric(gsub("#", "", shopify_order))) %>%
    

    select(shopify_order,
           email, 
           share_size, 
           opt_out,
           location,
           pickup_site) %>% 
    
    #delivery locations
    mutate(pickup_site = ifelse(str_detect(pickup_site, regex('home delivery', ignore_case = T)), "Home Delivery", pickup_site)) %>%
    separate(pickup_site, "pickup_site", sep = " \\(") %>%
    mutate(pickup_site_label = ifelse(str_detect(pickup_site, regex('home delivery', ignore_case = T)), paste(pickup_site, location, sep = " - "), pickup_site)) %>% 
    left_join(delivery_day %>% select(location,	pickup_site,	delivery_day, production_day)) %>% 

    #combining delivery day with location
    mutate_if(is.character, str_trim) %>% 
    mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>%

    #share_type. might not be necessary
    mutate(share_type = "Fillet") %>%
    mutate(opt_out = gsub("No ", "", opt_out)) %>% 
    
    arrange(-shopify_order) %>%
    filter(duplicated(email) == FALSE) %>% 
    select(-shopify_order)
    
}

# Clean up store orders and retrieve unmatched emails ---------------------------------------------
# App(s): Main Shares, Species Assignment, Early Deadline Orders
clean_weight_unit <- function(df) {
  
  df %>%
    mutate(weight_unit = ifelse(str_detect(tolower(product_name), "oyster"), "pcs", 
                                ifelse(str_detect(tolower(variant_name), "pint"), "pint", 
                                       ifelse(is.na(weight_lb), "-", weight_unit)))) %>% 
    mutate(weight_lb = ifelse(str_detect(tolower(product_name), "oyster"), quantity, weight_lb)) %>% 
    mutate(weight_lb = as.numeric(weight_lb))

}


# shiny_category_full: Get full forms of shiny_category from abbreviations -------------------------------------
# App(s): Special Orders & Early Deadline Orders

shiny_category_full <- function(x) {
  case_when(
    str_detect(x, "VO") ~ "Vendor Orders",
    str_detect(x, "FS") ~ "Fresh Seafood",
    str_detect(x, "IF") ~ "Frozen Products", 
    str_detect(x, "ID") ~ "Dry Products",
    TRUE                ~ NA_character_)
}


# get_delivery_date: Get dates according to delivery day ------------------------------------
# global.R, species-assignment-module.R
# wday_date function is in www/data/contants.R
get_delivery_date <- function(x) {
  case_when(
    x == "Monday"              ~ wday_date(1),
    x == "Tuesday"             ~ wday_date(2),
    x == "Wednesday"           ~ wday_date(3),
    x %in% c("Thursday", "LA") ~ wday_date(4),
    x == "Friday"              ~ wday_date(5))
}


