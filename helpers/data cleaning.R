# clean_phone_number: Cleaning up and formatting phone numbers -----------------------------------------
# App(s): Main Shares, Species Assignment, Home Delivery
clean_colname <- function(df, type) {
  

  if(type == "Orders") {
    
      colNames <- c(
      "order_id", 
      "order_date", 
      "order_time", 
      "order_day", 
      "customer_name",
      "customer_email",
      "product_name", 
      "variant_name",
      "product_type",
      "product_vendor",
      "quantity", 
      "sku", 
      "weight_lb", 
      "product_tag",
      "variant_price",
      "order_tag"
      
      )
    
    position <- c(1:16)

  }
  else {

      colNames <- c(
        "order_id", 
        "order_date", 
        "order_time", 
        "order_day",
        "customer_email",
        "subscription_size",
        "product_tag",
        "customer_tag",
        "order_tag",
        "customer_name",
        "address",
        "city",
        "state",
        "zip",
        "phone",
        "notes",
        "sub_1",
        "sub_2",
        "sub_3",
        "sub_4",
        "sub_5"
        )
      
      position <- c(1:21)
  }
  
  df %>%
    select(all_of(position)) %>% 
    `colnames<-`(colNames) 
  
}



# clean_subscription: data cleaning for shopify_orders -------------------------------------
# App(s): Main Shares, Species Assignment



  
clean_subscription <- function(df) {
  
  df %>%

    #clean and separate subscription properties, location, pickup_site, opt_outs
    gather(key = "position", value = "sub_value", sub_1:sub_5) %>% 
    separate(sub_value, c("key", "value"), sep = ":") %>% 
    mutate(key_name = ifelse(str_detect(key, "location"), "location",
                             ifelse(str_detect(key, "Site"), "pickup_site",
                                    ifelse(str_detect(key, "seafood exclusions"), "opt_out", NA)))) %>% 
    select(-position, -key) %>% 
    filter(!is.na(key_name)) %>%
    separate(value, "value_trim", sep = "\\(", extra = "drop") %>% 
    spread(key_name, value_trim) %>%
    
    #pickup site label format
    mutate(pickup_site_label = ifelse(str_detect(pickup_site, regex("Home Delivery", ignore_case = T)), paste0("Home Delivery - ", location), paste0(pickup_site, " - ",location))) %>% 
    mutate(pickup_site = ifelse(str_detect(pickup_site_label, "Home Delivery | Topa"), pickup_site_label, pickup_site)) %>%
    
    #getting share_size
    separate(subscription_size, "share_size", sep = " Share") %>%
    mutate(share_size = ifelse(share_size == "XL", "ExtraLarge", share_size)) %>% 
    
    #get delivery day
    mutate(delivery_day = str_extract(customer_tag, "Tuesday|Wednesday|Thursday")) %>% 
    mutate(delivery_day = ifelse(location == "Los Angeles ", "LA", delivery_day)) %>% 
    mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>% 
    
    mutate(opt_out = gsub("No ", "", opt_out)) %>% 
    mutate(share_type = "Fillet") %>%
    
    mutate_if(is.character, str_trim) 
    
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


