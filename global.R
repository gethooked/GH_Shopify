library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(janitor)
library(DT)
library(polished)
library(parallel)
library(readxl)

# 1. Configuration ==========================================================================

## Configure global sessions for `polished`
polished::global_sessions_config(
  app_name = "shopify",
  api_key = "4Ady0VxJ3fmqDvTaBdszpuaSUjGAOr9GDO")

## Configure ShinyApps account
rsconnect::setAccountInfo(
  name = 'beehoover',
  token = '4EFBDD20636816D472A997D3D76A6372',
  secret = 'JU+3ZGMGBL1IjlCfEsvVy0nYzDFglD4UDNPK8gWw')

# 2. Load Functions and Constant Values =====================================================
source(file.path("helpers", "functions.R"))
source(file.path("helpers", "data cleaning.R"))
source(file.path("www/data", "constants.R"))

# 3. Read Google Sheets =====================================================================

### Load Multiple worksheets from one document ----------------------------------------------
## sheet ids (the string of numbers after pub?gid=)305766482
Product_KEYS_id      <- c("633942957", "1819169532")
Active_Deliveries_id <- c("684811568", "346693116", "1627046312", "544947288",
                          "1544807263",  "1232985667",  "1443610327", "1160831953")

## Sheet names
Product_KEYS_names      <- c("Share_Size_Variant", "Species_Options")
Active_Deliveries_names <- c(paste0("Labels_",     delivery_day_levels_abb), 
                             paste0("Flashsales_", delivery_day_levels_abb))

## Read data
Product_KEYS      <- read_gs_para(ss = ss["product_keys"], Product_KEYS_id, Product_KEYS_names) 
Active_Deliveries <- read_gs_para(ss = ss["active_deliveries"], Active_Deliveries_id, Active_Deliveries_names)

## Only one worksheet is used --------------------------------------------------------------------

## Delivry Day (Shopify) - pickup_sites
delivery_day <- read_csv(gs_url(ss = ss["delivery_days"])) 

##Weekly order sync
order_sync   <- read_csv(gs_url(ss = ss["shopify"], sheets_id = "1184397264"), col_names = FALSE) 

##Shopify Customers sync
customer_sync <- read_csv(gs_url(ss = ss["shopify"], sheets_id = "1031783765"), col_names = FALSE) 


# 4. Data Cleaning ==============================================================================

##Assigning column names and data structure to avoid errors
shopify_orders  <- order_sync %>% 
  clean_colname(type = "Orders") %>% 
  mutate_at(vars(quantity, weight_lb, price), ~as.numeric(.))

shopify_customers <- customer_sync %>% 
  clean_colname(type = "Customer") %>% 
  mutate_at(vars(zip, phone), ~as.numeric(.)) 

##Active subscriptions for the week - used in mainshare & species assignment  
subscription <- shopify_orders %>% 
  clean_subscription() %>% 
  mutate(delivery_day_abb = toupper(substr(delivery_day, 1, 3)))

##Combining subscription info with all other member information
member_info <- shopify_customers %>% 
  left_join(subscription, by = "email") %>% 
  select(email, name, pickup_site_label, delivery_day, address, city, state, zip, phone, location, pickup_site)

pickup_sites_key <- subscription %>%
  select(location, pickup_site, pickup_site_label, delivery_day, production_day)
  

product_key <- shopify_orders %>% 
  select(product_name, product_tags) %>% 
  mutate(route_notation = ifelse(str_detect(product_tags, "Dry Goods"), "yes", "")) %>% 
  mutate(share_class = str_remove(product_tags, "Dry Goods Restriction, Pantry, ")) %>% 
  rename(share_type = product_name) %>% 
  unique()
  

# 4.1 Main Shares & Species Assignment ==========================================================

## * Share Size Variant & Species Options --------------------------------------------------------

share_size_variant <- Product_KEYS$Share_Size_Variant %>%
  filter(size_key == "share") %>%
  filter(size %in% c("Small", "Medium", "Large", "ExtraLarge"))%>%
  mutate(type = tolower(type)) %>%
  mutate(type_singular = singularize(type)) %>%
  mutate(share_size_estimate = ifelse(is.na(weight), pieces, weight + 0.02))%>%
  select(type, type_singular, size, share_size_estimate)%>%
  rename("share_size" = "size")

## Used in Main_Shares_Server and Species_Assignment_Server
type_list <- unique(c(share_size_variant$type,
                      share_size_variant$type_singular))%>% 
  paste(collapse = "|")


## Choices of "Select Species" drop-down list in the sidebar
species_options_raw <- Product_KEYS$Species_Options$options
share_size_variant_type <- str_to_title(unique(share_size_variant$type))
not_species         <- str_detect(tolower(species_options_raw),
                                  "\\bother species\\b|\\bnone\\b|all one species")

species_list         <- c(species_options_raw[!not_species], 
                          share_size_variant_type[share_size_variant_type!="fillet"]) %>% unique %>% sort

species_options     <- append(sort(species_options_raw[not_species]),
                              species_list, after = 1)

## Used in clean_subscription to specify opt outs
### Some opt outs correspond to multiple species in the species selection drop-down list
### (e.g. Shrimp -> Ridgeback Shrimp, Pink Shrimp, Spot Prawn)
opt_out_extra <- Product_KEYS$Share_Size_Variant %>% 
  select(type, opt_out_category) %>% unique() %>%
  mutate(opt_out_category = str_to_title(opt_out_category),
         type = str_to_title(type)) %>%
  filter(type != opt_out_category) %>%
  filter(opt_out_category != "") 



### Choices for "Select Delivery Locations" in the sidebar
sites <- subscription$pickup_site

### Split sites into a list by delivery day
sites_list <- split(sites, subscription$delivery_day_abb) 


## Core dataset for Main Shares app
weekly_species11 <- subscription %>% 
  mutate(species = "unassigned") %>% 
  left_join(member_info %>% select(email,name))
  

# * Tab: Share Pounds by Site ----------------------------------------------------------------
fillet_weight_by_site <- weekly_species11 %>% 
  group_by(share_size, delivery_day, pickup_site_label) %>% 
  tally() %>% 
  arrange(pickup_site_label) %>%
  left_join(share_size_variant %>% filter(type_singular == "fillet"),
            by = "share_size") %>%
  mutate(total_fillet = n * share_size_estimate)%>%
  group_by(delivery_day, pickup_site_label) %>%
  summarise(sum = sum(total_fillet))



# * Flash Orders -----------------------------------------------------------------------------
 flashsales_Main_shares <- shopify_orders %>%

  ## get main share orders
  filter(str_detect(tolower(product_tags), "main share upgrade")) %>%
  ## remove "pick for me" options
  filter(is.na(sku)) %>% 

   ## e.g. Fresh Halibut Fillet -> Halibut
  mutate(share_upgrade = str_remove_all(variant_name, "Fresh | Fillet")) %>%
  select(email, share_upgrade) %>%
  mutate(species_choice = share_upgrade) %>% 
  unique() 




 flash_list <- unique(flashsales_Main_shares$species_choice) %>%
   setdiff("") %>% c("None")

 flash_fillet <- setdiff(flash_list, c(species_options, "")) %>% tolower


# 4.2 Checklists ==============================================================================

 label_list <- lapply(delivery_day_levels, generate_checklists) %>%
   `names<-`(delivery_day_levels_abb)

wrong_list <- lapply(label_list, function(x) {x$wrong_list}) %>%
  do.call(rbind, .) %>% `row.names<-`(NULL)

# 4.3 Home Deliveries =========================================================================

 all_home_delivery <- subscription %>% 
   filter(pickup_site == "Home Delivery") %>% 
   select(email, location, delivery_day) %>% 
   left_join(shopify_customers, by = "email") %>% 
   select(name, address, city, state, zip, phone, note, email, delivery_day)

  

## Main Share Labels

labels <- rbind_active_deliveries(type = "Labels") %>% 
  mutate(type = "subscription",
         check = " ") %>% 
  mutate(name = as.character(name)) %>% 
  mutate(home_delivery_name = as.character(home_delivery_name)) %>% 
  mutate(name = ifelse(name == "Home Delivery", home_delivery_name, name)) %>% 
  mutate(name = as.character(name)) %>%
  mutate(pickup_site_label = str_trim(pickup_site_label, side = "both"))%>%
  mutate_all(as.character)


share <- labels %>% 
  group_by(delivery_day, name, species, share_size) %>% 
  summarise(species = toString(unique(species)))


## Flashsales

flashsales <- rbind_active_deliveries(type = "Flashsales") %>%
  mutate(share_type = as.character(share_type),
         share_size = as.character(share_size),
         Timestamp = as.character(Timestamp)) %>% 
  ## New member labels are sometimes pasted into Flashsalse spreadsheets for printing purposes.
  ## The first column will be ~ Welcome to Get Hooked ~. Filter out those labels according to these keywords
  filter(!str_detect(Timestamp, "~") | !Timestamp == "welcome" | is.na(Timestamp)) %>%
  mutate(delivery_day = as.character(delivery_day)) %>%
  group_by(delivery_day, name) %>%
  left_join(product_key,
            by = "share_type") %>%
  ungroup() %>% 
  mutate(check = " ") %>%
  mutate(name = as.character(name)) %>%
  filter(share_type != "")


has_drygoods <- flashsales %>%
  group_by(delivery_day, name, product_tags) %>%
  filter(!is.na(route_notation)) %>%
  tally() %>%
  mutate(dry_list = paste(n, product_tags, sep = " ")) %>%
  mutate(dry_list = ifelse(str_detect(dry_list, "Pick up"), "Pick up bags only", dry_list)) %>%
  group_by(delivery_day, name) %>%
  summarise(all_dry = toString(unique(dry_list)))

# All Home Deliveries *******************************************************************************

deliveries_to_complete <- rbind(labels     %>% select(name, pickup_site_label, check),
                                flashsales %>% select(name, pickup_site_label, check)) %>% #day, name, pickupsite label
  filter(str_detect(pickup_site_label, "Home Delivery")) %>%
  left_join(subscription %>% select(pickup_site_label, delivery_day)) %>% #pickup_day, restriction, production day
  left_join(share,             by = c("delivery_day", "name")) %>% #species, sharesize
  left_join(has_drygoods,      by = c("delivery_day", "name")) %>%
  left_join(all_home_delivery, by = c("delivery_day", "name")) %>% #customer details
  mutate(delivery_date = today()) %>%
  mutate(all_dry  = replace_na(all_dry, "")) %>%
  distinct()

# * Button: Download Delivery Route -> home_delivery_module -----------------------------------------
deliveries_label <- deliveries_to_complete %>%
  select(delivery_day, share_size, species, name, delivery_date, all_dry) %>%
  mutate(spacer_1 = "~", spacer_2 = "~", spacer_label = "~")
# 
# 
# # * Tab: Check Address ------------------------------------------------------------------------------
check_address <- deliveries_to_complete %>%
  filter(is.na(address) | address == "")
# 
# 
# # * Tab: Routesavvy File/Button: Download Routesavvy File ------------------------------------------
deliveries_routesavvy <- deliveries_to_complete %>%
  select(delivery_day, folder = delivery_date, name, address,
         city, state, zip, note2 = phone)



# 4.4 Early Deadline Orders & Special Orders ----------------------------------------------------------------

orders_ED_SO <- shopify_orders %>% 
  # filter out main share orders
  filter(!str_detect(tolower(product_tags), "main share")) %>% 
  filter(is.na(order_tags)) %>% 
  unite("timestamp", order_date, order_time, sep = " ") %>% 
  select(email, product_name, variant_name, sku, timestamp, quantity, order = line_item_name, vendor, price, weight_lb) %>%

  #turning multiples to individal lines except oysters
  mutate(variant_name = ifelse(str_detect(tolower(product_name), "oyster"), quantity, variant_name),
         quantity = ifelse(str_detect(tolower(product_name), "oyster"), 1, quantity)) %>% 
  uncount(quantity) %>% 
  
  #data cleaning
  mutate(share_type = replace_na(product_name, "") %>% trimws) %>% 
  mutate(share_size = replace_na(variant_name, "1") %>% trimws) %>% 
  mutate(source = "Store") %>%
  
  #create shiny_category with ED/SO, VO/FS/IF/ID 1487120EDFS
  mutate(shiny_category = str_sub(sku,8,11)) %>% 

  #join with member info
  left_join(member_info, by = "email")

ED_SO_category <- sort(unique(orders_ED_SO$shiny_category))
ED_category <- ED_SO_category[str_detect(ED_SO_category, "ED")] %>% str_remove("ED")


#data set used for ED/SO labels
all_ED_SO <- orders_ED_SO %>%
  mutate(spacer_1 = "~~~~~~~~",
         spacer_2 = "~~~~~~~~",
         date = get_delivery_date(delivery_day) %>% as.Date(format = "%m/%d/%y")) %>% 
  mutate(instructions = ifelse(str_detect(share_type, "Frozen"), 
                               "*Puncture package to thaw in refrigerator*", " ")) %>% 
  arrange(delivery_day, name, share_type, share_size) %>% 
  select(shiny_category, timestamp, email, name, spacer_1, share_type, share_size, spacer_2, 
         pickup_site_label, date, instructions, source, order, price, delivery_day)



# 4.4.1 Early Orders ----------------------------------------------------------------------------------------
title_ED <- title_with_date("Early Deadline Orders")


# * Tab: Early Order Tally/Button: Download Preorders -------------------------------------------------------
all_preorders_ED <- orders_ED_SO %>%
  filter(str_detect(shiny_category, "ED")) %>%
  group_by(shiny_category, delivery_day, vendor, share_type) %>% 
  mutate(share_size = str_remove_all(share_size, "[^0-9]")) %>% 
  mutate(share_size = as.numeric(share_size)) %>%
  summarise(share_size_total = sum(share_size)) %>% 
  ungroup()%>%
  mutate(multiplier = case_when(str_detect(share_type, "(two)") ~ 2,
                                str_detect(share_type, "(four)")  ~ 4,
                                TRUE ~ 1)) %>%
  mutate(total = share_size_total * multiplier) %>% 
  mutate(share_type = str_remove(share_type, "(two)|(four)") %>% trimws) %>%
  mutate(vendor = ifelse(shiny_category == "EDFS", "-", vendor))%>%
  select(shiny_category, vendor, delivery_day, share_type, total) %>% 
  arrange(vendor, delivery_day, share_type)


# 4.4.2 Special Orders --------------------------------------------------------------------------------------
title_SO <- title_with_date("Special Orders")


# * Tab: Fresh Product by Day -------------------------------------------------------------------------------
fresh_product_by_day <- orders_ED_SO %>% 
  filter(str_detect(shiny_category, "FS")) %>%
  mutate(delivery_day = factor(delivery_day, levels = delivery_day_levels)) %>%
  group_by(delivery_day, share_type) %>% 
  summarise(total_number = sum(weight_lb))

# * Tab: Fresh Product Needed -------------------------------------------------------------------------------
fresh_product_all <- fresh_product_by_day %>% 
  group_by(share_type) %>% 
  summarise(total_number = sum(total_number)) %>%
  arrange(share_type)
  
# * Tab: TUE/WED/THU/LA orders ------------------------------------------------------------------------------
all_preorders_SO <- orders_ED_SO %>%
  filter(str_detect(shiny_category, "SO")) %>%
  count(delivery_day, share_type, share_size) %>%
  split(.$delivery_day)

# * Button: Download TUE/WED/THU/LA Labels ------------------------------------------------------------------
special_orders_all <- all_ED_SO %>%
  filter(str_detect(shiny_category, "SO")) %>%
  mutate(shiny_category = shiny_category_full(shiny_category)) %>%
  split(.$shiny_category) %>%
  lapply(function(df) {
    df %>% select(-shiny_category)})

# * Buttons: Download WED/THU/LA Labels with routes ---------------------------------------------------------
route <-  read_csv(gs_url(ss = ss["home_delivery"], sheets_id = "2080672642")) %>% 
  select(name, driver_stop_route) %>%
  filter(name != "name")%>%
  filter(!str_detect(driver_stop_route, "~"))

dry_goods <- rbind_active_deliveries(type = "Flashsales") %>%
  filter(!str_detect(Timestamp, "~") | !Timestamp == "welcome" | is.na(Timestamp)) %>%
  filter(share_type != "") %>%
  mutate(share_type = ifelse(!is.na(order) & str_detect(order, "Fillet"), 
                             str_remove(order, " - .*"), share_type)) %>%
  mutate(instructions = "") %>% 
#  left_join(Product_KEYS$Product_List, by = "share_type") %>% 
#  filter(label_method == "label_route" | is.na(label_method)) %>% 
  left_join(route, by = "name") %>%
  replace_na(list(driver_stop_route = "")) %>%
  mutate(PSL = pickup_site_label) %>% 
  mutate(route_order = str_extract(driver_stop_route, "[0-9]+")) %>%
  mutate(route_driver = str_extract(driver_stop_route, "[a-zA-Z]+")) %>%
  mutate(pickup_site_label = ifelse(driver_stop_route == "", 
                                     pickup_site_label, driver_stop_route))%>%
  arrange(delivery_day, route_driver, as.numeric(route_order), 
          pickup_site_label, name, share_type) %>% 
  select(Timestamp, email, name, spacer_1, share_type, share_size, 
         spacer_2, pickup_site_label, date, instructions, source, 
         order, price, delivery_day, PSL)

route_labels <- dry_goods %>% 
  split(.$delivery_day)

# 5. Unlink temporary files ===============================================================
#unlink(Special_Order_temp)
#unlink(Home_Delivery_temp)


# 6. Import Modules =======================================================================

 source(file.path("modules", "main-shares-module.R"))
 source(file.path("modules", "species-assignment-module.R"))
 source(file.path("modules", "early-orders-module.R"))
source(file.path("modules", "special-orders-module.R"))
 source(file.path("modules", "checklists-module.R"))
 source(file.path("modules", "home-delivery-module.R"))
# source(file.path("modules", "bag-counts-module.R"))
source(file.path("modules", "submodules.R"))

