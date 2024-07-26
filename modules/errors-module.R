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
          
          h4("Missing Orders"),
          tags$div(
            "Potential missing order. Order ID in ",
            a("Shopify - Order Sync",
              href = "https://docs.google.com/spreadsheets/d/1QeWzX5iD70oQfWuLC1GI1xcIBwz78s2Q1Oi8sieWW98/edit#gid=0"),
            "but missing from ",
            a("Shopify Orders.",
              href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773"),
            "OR in Shopify, but missing from Google Sheets.",
          ),
          br(),
          
          h4("Cancelled Orders"),
          tags$div(
            "Time stamp in orderCancelledAt or orderstatus is null in ",
            a("Shopify - Order Sync.",
              href = "https://docs.google.com/spreadsheets/d/1QeWzX5iD70oQfWuLC1GI1xcIBwz78s2Q1Oi8sieWW98/edit#gid=0")
          ),
          
          br(),
          
          h4("No Customer Match"),
          tags$div(
            "No match for customer associated with order in  ",
            a("Shopify Orders.",
              href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773")
          ),
          
          br(),
          
          h4("Double Order Check"),
          tags$div(
            "Customers with multiple of the same subsription order in  ",
            a("Shopify Orders.",
              href = "https://docs.google.com/spreadsheets/d/1SIOuuBBOXQ9-oCWbN-Hv9bHirxZH9S1RrJSGiaphK1Y/edit#gid=16853773")
          ),
          
          br(),
          
          h4("No Subscription Order"),
          tags$div(
            "Customers placed flashsale order but no subscription order found."
          ),
          
          br(),
          
          h4("Missing Product Category"),
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
            tabPanel("Cancelled Orders", dataTableOutput(ns("cancelled_orders"))),
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
      
      sequence <- order_sync %>% 
        mutate(id_number = as.numeric(str_extract(order_id, "[0-9]+"))) %>% 
        filter(!is.na(id_number))
      
      seq2 <- min(sequence$id_number):max(sequence$id_number)
      
      missing_id <- c(0, seq2[!seq2 %in% sequence$id_number])
      df_missing <- data.frame (order_id  = missing_id,
                                orderLineItems = "Check Shopify")
      
      missing_orders <- active_week_orders %>% 
        left_join(orders_df %>% select(order_id, index)) %>% 
        filter(is.na(index)) %>%
        mutate(order_id = as.numeric(str_extract(order_id, "[0-9]+"))) %>% 
        full_join(df_missing)
      
      # * Orders placed this week that were either canceled or not fulfilled
      cancelled_orders <- order_details_df %>%
        filter((!is.na(orderCancelledAt) & orderStatus != "fulfilled") | order_id == "CANCELLED")
      
      
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
#        datatable(missing_orders, options = datatable_options),
        datatable_export(missing_orders, title = title_with_date("Missing Orders"))
      })
      
      
      # Tab: Check Orders ----------------------------------------------
      output$cancelled_orders <- renderDataTable({
        #datatable(cancelled_orders, options = datatable_options)
        datatable_export(cancelled_orders, title = title_with_date("Cancelled Orders"))
      })

      
      # Tab: No Customer Match ----------------------------------------------
      output$no_customer_match <- renderDataTable({
        datatable_export(no_customer_match, title = title_with_date("No Customer Match"))
      })
      
      # Tab: Double Order Check ----------------------------------------------
      output$double_order_check <- renderDataTable({
        datatable_export(double_order_check, title = title_with_date("Double Order Check"))
      })
      
      # Tab: No Subscription Order ----------------------------------------------
      output$error_no_sub <- renderDataTable({
        datatable_export(error_no_sub, title = title_with_date("No Subscription Order"))
      })
      
      # Tab: Product Missing Category ----------------------------------------------
      output$error_missing_category <- renderDataTable({
        datatable_export(error_missing_category, title = title_with_date("Product Missing Category"))
      })
      
      
      
    }
  )
}