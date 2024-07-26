# clean_date: Cleaning up and formatting phone numbers -----------------------------------------
clean_date <- function(df) {
  
  df %>% 
    separate(order_date, c("order_date","order_day"), sep = ',') %>% 
    mutate(order_date = as.Date(paste0(order_date, " ", format(Sys.Date(), "%Y")), format = "%B %d %Y")) %>%
    select(-order_day)
    
}

new_member_cutoff <- function(df) {
  
  df %>% 
    mutate(delivery_week = ifelse(str_detect(order_tags, "1st Order") & order_date > cutoff_date, "Next Week", ""))
}


clean_colnames_AD <- function(df_list){
  df_list %>% 
  imap(~ if(.y %in% paste0("Labels_", delivery_day_levels_abb)) {
    .x %>% 
      select(1:length(labels_colname)) %>% `colnames<-`(labels_colname) %>% mutate(across(everything(), as.character))
  } else {
    .x %>% 
      select(1:length(flashsales_colname)) %>% `colnames<-`(flashsales_colname) %>% mutate(across(everything(), as.character))
  })
}

# App(s): Main Shares, Species Assignment, Home Delivery
clean_colnames <- function(df, type) {
  
  if(type == "Orders") {
    
    colNames <- orders_colNames
    position <- c(1:length(orders_colNames))
    
    colNumeric <- c("customer_id", "quantity", "price", "size")
    
    colCharacter <- c("order_id","order_date","order_time","customer_name","customer_email","product_name","variant_name","vendor","deadline_type","inventory_type","size_unit","packing_method","checklist_notation","sku","order_tags","prod_day","product_tags")
    
  }
  else if(type == "Subscription") {
    
    colNames <- subscription_colNames
    
    position <- c(1:length(subscription_colNames))
    
    colNumeric <- c()
    
    colCharacter <- c("order_id","order_date","order_time","customer_email","subscription_size","product_tag","order_tag","customer_name")
  }
  
  # Order Sync Sheet
  else if(type == "Order_Status") {
    
    colNames <- order_status_colNames
    
    position <- c(1:length(order_status_colNames))
    
    colNumeric <- c()
    
    colCharacter <- c("order_id","orderCreatedAt","orderEmail","orderLineItems","order_tags","orderStatus","orderCancelledAt","orderLineItemsID","orderURL")
    
  }
  ## Customers
  else {
    colNames <- customers_colNames
    
    position <- c(1:length(customers_colNames))
    
    colNumeric <- c("customer_id", "order_count","email_verified")
    
    colCharacter <- c(
      "customer_email",
      "customer_name",
      "share_size",
      "pickup_location",
      "pickup_site",
      "opt_out",
      "delivery_day",
      "address1",
      "address2",
      "city",
      "state",
      "zip",
      "phone",
      "customer_tags",
      "partner_email", 
      "deliv_week_day",
      "homedelivery_instructions",
      "customer_created_at",
      "customer_updated_at"
    )
    
  }
  
  df %>%
    select(all_of(position)) %>% 
    `colnames<-`(colNames) %>% 
    mutate_at(colNumeric, as.numeric) %>% 
    mutate_if(negate(is.numeric), as.character)
  
}


clean_customer <- function(df) {
  
  df %>%
    
    #mutate(location_abb = ifelse(str_count(pickup_location) > 10, gsub("[^A-Z]","", pickup_location), pickup_location)) %>%
    mutate(location_abb = ifelse(str_detect(pickup_location, " "), gsub("[^A-Z]","", pickup_location), pickup_location)) %>%
    mutate(pickup_site = ifelse(str_detect(pickup_site, "Home Delivery"), paste0(pickup_site, " ",location_abb), 
                                  ifelse(str_detect(pickup_site, "Topa"), paste0("Topa Topa", " ",location_abb), 
                                         ifelse(str_detect(pickup_site, "Rincon"), paste(pickup_site, substr(pickup_location, 1, 4)), 
                                                                                         pickup_site)))) %>% 
      
    mutate(pickup_site_label = ifelse(str_detect(pickup_site, regex('santa monica', ignore_case = T)), word(pickup_site, 1,3),
                                        ifelse(str_detect(pickup_site, ":"), sub(":.*", "", pickup_site), pickup_site))) %>% 
    select(-location_abb) %>% 
    
    #delivery day
    mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>%
    mutate(delivery_day_abb = toupper(substr(delivery_day, 1, 3))) %>% 
    
    #opt-out
    mutate(opt_out = gsub("No ", "", opt_out)) %>%
    mutate(opt_out = gsub("\\|", ", ", opt_out)) %>% 
    
    #definte share type
    mutate(share_type = "Fillet") %>%
    
    #clean data
    mutate_at(vars(zip), ~as.numeric(.)) %>%
    mutate_if(is.character, str_trim) %>% 
    lapply(gsub, pattern = "&#039;", replacement = "'", fixed = TRUE) %>%
    lapply(gsub, pattern = "&quot;", replacement = "", fixed = TRUE) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    mutate(opt_out = ifelse(is.na(opt_out), "None",
                            ifelse(str_detect(opt_out, "try it all"), "None", opt_out))) %>% 
    mutate(address = paste0(address1, ", ", address2))
  
}

# clean_subscription: data cleaning for shopify_orders -------------------------------------
# App(s): Main Shares, Species Assignment
  
clean_subscription <- function(df) {
  
  df %>%
    uncount(as.integer(quantity), .remove = FALSE, .id = "id") %>% 
    #share_size
    mutate(share_size = str_extract(product_name,"(\\w+)")) %>% 
    mutate(share_size = ifelse(share_size == "XL", "ExtraLarge", share_size)) %>%
    mutate(delivery_notes = homedelivery_instructions) %>% 
    filter(str_detect(order_id, "#")) %>% 
    mutate(share_type1 = ifelse(!is.na(share_type1), share_type1, "")) %>% 
    
    #select necessary columns
    select(
      order_id,
      order_date,
      order_time,
      customer_email,
      share_size,
      id,
      order_tags,
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
      partner_email,
      price,
      share_type1
    )
    
}

# Convert shiny categories ---------------------------------------------
shiny_category <- function(df) {
  
  inventory_rep = c("Vendor Order" = "VO",
                    "Fresh Seafood" = "FS",
                    "Inventory Frozen" = "IF", 
                    "Inventory Dry" = "ID")
  
  deadline_rep = c("Early Deadline" = "ED",
                   "Special Order" = "SO")
  
  df %>% 
    mutate(deadline_abb = str_replace_all(deadline_type, deadline_rep)) %>% 
    mutate(inventory_abb = str_replace_all(inventory_type, inventory_rep)) %>% 
    mutate(shiny_category = ifelse(str_detect("SO|ED", deadline_abb), paste0(deadline_abb, "/", inventory_abb), NA))

}

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

shiny_category_short <- function(x, y) {
  
  inventory_rep = c("Vendor Order" = "VO",
                    "Fresh Seafood" = "FS",
                    "Inventory Frozen" = "IF", 
                    "Inventory Dry" = "ID")
  
  deadline_rep = c("Early Deadline" = "ED",
                    "Special Order" = "SO")
  
  # case_when(
  #   str_detect(x, "Vendor Order") ~ "VO",
  #   str_detect(x, "Fresh Seafood") ~ "FS",
  #   str_detect(x, "Inventory Frozen") ~ "IF", 
  #   str_detect(x, "Inventory Dry") ~ "ID",
  #   TRUE                ~ NA_character_)
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


