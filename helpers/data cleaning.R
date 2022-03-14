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
  else if(type == "Subscription") {
    
    colNames <- c(
      "order_id", 
      "order_date", 
      "order_time", 
      "customer_email",
      "subscription_size",
      "product_tag",
      "order_tag",
      "customer_name"
      #      "address1",
      #      "address2",
      #      "city",
      #      "state",
      #      "zip",
      #      "phone",
      #      "notes",
      #      "customer_tag",
      #      "location",
      #      "pickup_site",
      #      "opt_out"
    )
    
    position <- c(1:8)
  }
  
  else {
    colNames <- c(
      "customer_id",
      "customer_email",
      "full_name",
      "share_size",
      "pickup_location",
      "pickup_site",
      "opt_out",
      "delivery_day",
      "address",
      "delivery_notes",
      "city",
      "state",
      "zip",
      "phone",
      "email_verified",
      "customer_tags",
      "partner_email"
      #      "delivery_day",
      #      "order_count"
    )
    
    position <- c(1:17)
    
  }
  
  df %>%
    select(all_of(position)) %>% 
    `colnames<-`(colNames) 
  
}



# clean_subscription: data cleaning for shopify_orders -------------------------------------
# App(s): Main Shares, Species Assignment
  
clean_subscription <- function(df) {
  
  df %>%
    mutate(location_abb = ifelse(str_detect(pickup_location, "Los Angeles"), "LA", 
                             ifelse(str_detect(pickup_location, "Santa Barbara"), "SB", pickup_location))) %>% 
    mutate(pickup_site_label = paste0(pickup_site, " ",location_abb)) %>%
    select(-location_abb) %>% 
    
    mutate(pickup_site = ifelse(str_detect(pickup_site, "Home Delivery|Topa"), pickup_site_label, pickup_site)) %>%
    
    #share_size
    mutate(share_size = ifelse(subscription_size == "XL", "ExtraLarge", subscription_size)) %>% 
    
    #delivery day
    mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>% 
    
    #opt-out
    mutate(opt_out = gsub("No ", "", opt_out)) %>%
    mutate(opt_out = gsub("\\|", ", ", opt_out)) %>% 
    
    #definte share type
    mutate(share_type = "Fillet") %>%
    
    #clean data
    mutate_at(vars(zip), ~as.numeric(.)) %>%
    mutate_if(is.character, str_trim) %>% 
    
    #select necessary columns
    select(
      order_id,
      order_date,
      order_time,
      customer_email,
      share_size,
      order_tag,
      customer_name,
      pickup_location,
      pickup_site,
      pickup_site_label,
      opt_out,
      delivery_day,
      address,
      city,
      state,
      zip,
      phone,
      delivery_notes,
      customer_tags,
      partner_email
    )
    
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


