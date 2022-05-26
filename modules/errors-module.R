Errors_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    column(
      12,
      
      titlePanel("Check Me"),
      
      sidebarLayout(
        sidebarPanel(
          
          #        includeMarkdown("www/Rmd/early-special-orders-instructions.Rmd"),
          
          br(),
          
          h3("Missing Orders"),
          tags$div(
            "Potential missing order. Order ID in ",
            a("Shopify - Order Sync",
              href = "https://docs.google.com/spreadsheets/d/1QeWzX5iD70oQfWuLC1GI1xcIBwz78s2Q1Oi8sieWW98/edit#gid=0"),
            "but missing from ",
            a("Shopify Orders.",
              href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773")
          ),
          br(),
          
          h3("Check Orders"),
          tags$div(
            "Time stamp in orderCancelledAt or orderstatus is null in ",
            a("Shopify - Order Sync.",
              href = "https://docs.google.com/spreadsheets/d/1QeWzX5iD70oQfWuLC1GI1xcIBwz78s2Q1Oi8sieWW98/edit#gid=0")
          ),
          
          br(),
          
          h3("No Customer Match"),
          tags$div(
            "No match for customer associated with order in  ",
            a("Shopify Orders.",
              href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773")
          ),
          
          br(),
          
          h3("Double Order Check"),
          tags$div(
            "Customers with multiple of the same subsription order in  ",
            a("Shopify Orders.",
              href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773")
          ),
          
          br(),
          
          h3("No Subscription Order"),
          tags$div(
            "Customers placed flashsale order but no subscription order found."
          ),
          
          br(),
          
          h3("Missing Product Category"),
          tags$div(
            "Product is missing category (Inventory Type or Deadline Type)."
          ),
          
          

          
          br(),
          
          icon("info-circle"),
          span(strong("FS"), " = Fresh Seafood;",
               strong("VO"), " = Vendor Orders",
               class = "prompt-message")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Missing Orders",  dataTableOutput(ns("missing_orders"))),
            tabPanel("Check Orders", dataTableOutput(ns("check_orders"))),
            tabPanel("No Customer Match", dataTableOutput(ns("no_customer_match"))),
            tabPanel("Double Order Check", dataTableOutput(ns("double_order_check"))),
            tabPanel("No Subscription Order", dataTableOutput(ns("error_no_sub"))),
            tabPanel("Missing Product Category", dataTableOutput(ns("error_missing_category"))),
          )
        )
      )
    )
  )
}

Errors_Server <- function(id) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # * Find missing orders (orders that are in Shopify but not in Shopify orders)
      missing_orders <- active_week_orders %>% 
        left_join(orders_df %>% select(order_id, index)) %>% 
        filter(is.na(index))
      
      # * Orders placed this week that were either canceled or not fulfilled
      check_orders <- active_week_orders %>%
        filter(!is.na(orderCancelledAt)| orderStatus != "fulfilled")
      
      
      # * Missing Details Match
      no_customer_match <- orders_df %>% 
        left_join(order_details_df %>% select(index, match) %>% 
                    mutate(index = as.integer(index))) %>%
        filter(is.na(match))
      
      
      # * Flashsale order but no subscription order 
      error_no_sub <- all_shopify_orders %>% 
        left_join(shopify_subscription %>% select(customer_email) %>% 
                    mutate(source = "subscription") %>% unique()) %>% 
        filter(is.na(source)) %>% 
        mutate(error_type = "no subscription order") %>% 
        select(-source, -route_notation, -share_class, -shiny_category, -delivery_week)
      
      # * Product is missing category (Inventory Type or Deadline Type)
      error_missing_category <- order_details %>% 
        filter(is.na(inventory_type)|is.na(deadline_type)) %>%
        filter(!str_detect(product_name, "Delivery Fee|Distance Fee|(Test)")) %>%
            filter(!str_detect(replace_na(product_tags, ""), "Fee")) %>% 
        mutate(error_type = "missing category")
      
      
      # Tab: Missing Orders ---------------------------------------------
      output$missing_orders <- renderDataTable({
        datatable(missing_orders, options = datatable_options)
      })
      
      # Tab: Check Orders ----------------------------------------------
      output$check_orders <- renderDataTable({
        datatable(check_orders, options = datatable_options)
      })
      
      # Tab: No Customer Match ----------------------------------------------
      output$no_customer_match <- renderDataTable({
        datatable(no_customer_match, options = datatable_options)
      })
      
      # Tab: Double Order Check ----------------------------------------------
      output$double_order_check <- renderDataTable({
        datatable(double_order_check, options = datatable_options)
      })
      
      # Tab: No Subscription Order ----------------------------------------------
      output$error_no_sub <- renderDataTable({
        datatable(error_no_sub, options = datatable_options)
      })
      
      # Tab: Product Missing Category ----------------------------------------------
      output$error_missing_category <- renderDataTable({
        datatable(error_missing_category, options = datatable_options)
      })
      
      
      
    }
  )
}