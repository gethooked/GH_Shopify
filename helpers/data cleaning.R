# clean_date: Cleaning up and formatting phone numbers -----------------------------------------
clean_date <- function(df) {
  
  df %>% 
    separate(order_date, c("order_date","order_day"), sep = ',') %>% 
    mutate(order_date = as.Date(paste0(order_date, " ", format(Sys.Date(), "%Y")), format = "%B %d %Y")) %>%
    select(-order_day)
    
}

new_member_cutoff <- function(df) {
  
  df %>% 
    mutate(delivery_week = ifelse(str_detect(order_tag, "1st Order") & order_date > cutoff_date, "Next Week", ""))
}


# App(s): Main Shares, Species Assignment, Home Delivery
clean_colname <- function(df, type) {
  

  if(type == "Orders") {
    
      colNames <- c(
      "order_id", 
      "order_date", 
      "order_time", 
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
      "order_tag"
      )
    
    position <- c(1:14)

  }
  else {

      colNames <- c(
        "order_id", 
        "order_date", 
        "order_time", 
        "customer_email",
        "subscription_size",
        "product_tag",
        "order_tag",
        "customer_name",
        "address1",
        "address2",
        "city",
        "state",
        "zip",
        "phone",
        "notes",
        "customer_tag",
        "location",
        "pickup_site",
        "opt_out"
        )
      
      position <- c(1:19)
  }
  
  df %>%
    select(all_of(position)) %>% 
    `colnames<-`(colNames) 
  
}



# clean_subscription: data cleaning for shopify_orders -------------------------------------
# App(s): Main Shares, Species Assignment
  
clean_subscription <- function(df) {
  
  df %>%
    mutate(pickup_site_label = paste0(pickup_site, " - ",location)) %>%
  
    mutate(pickup_site = ifelse(str_detect(pickup_site, "Home Delivery|Topa"), pickup_site_label, pickup_site)) %>%
    
    #getting share_size
    # separate(subscription_size, "share_size", sep = " Share") %>%
    mutate(share_size = ifelse(subscription_size == "XL", "ExtraLarge", subscription_size)) %>% 
    
    #get delivery day
    mutate(delivery_day = str_extract(order_tag, "Tuesday|Wednesday|Thursday")) %>% 
    mutate(delivery_day = ifelse(location == "Los Angeles", "LA", delivery_day)) %>% 
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


