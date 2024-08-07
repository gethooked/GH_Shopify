Checklists_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        12,
        titlePanel("Checklists"),
        fluidRow(
          actionButton(ns("reloadChecklists"), "Reload Checklists Data",
                       icon = icon("sync-alt"), class = "btn-reload")
        ),
        
        uiOutput(ns('tabset_checklist'))
      )
    )
  )
}


Checklists_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # * Button: Reload Checklist Data Button -------------------------------------------------------------
      observeEvent(input$reloadChecklists, {
        showModal(modalDialog("Loading...", footer = NULL))
        
        Active_Deliveries <- read_gs_para(ss = ss["active_deliveries"], 
                                          Active_Deliveries_id, 
                                          Active_Deliveries_names) %>% 
                             clean_colnames_AD()
        
        label_list <- lapply(delivery_day_levels, generate_checklists) %>%
          `names<-`(delivery_day_levels_abb)
        
        removeModal()
        session$reload()
      })
      
      ## mapply to automatically generate tabset according to values in delivery_day & delivery_day_abb
      ## abb: abbreviation; type: kichen or list
      output$tabset_checklist <- renderUI({
        
        mapply(function(abb, type) {
          
          tabPanel_checklists_UI(
            id = session$ns(paste(abb, type, "tabset", sep = "_")),
            abb = abb, type = type, 
            choices = names(label_list[[abb]][[paste0(type, "_list")]])
            )
          },
          abb = c(sapply(delivery_day_levels_abb, rep, 2)),
          type = rep(c("Kitchen", "Site"), length(delivery_day_levels_abb)),
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE) %>%

           do.call(tabsetPanel, .)
      })

      ## mapply to execute module function ----------------------------------------------------
      mapply(function(abb, type) {
        
        tabPanel_checklists_Server(
          id = paste(abb, type, "tabset", sep = "_"),
          abb = abb, type = type,
          dt = label_list[[abb]][[paste0(type, "_list")]]
          )
        },
        abb = c(sapply(delivery_day_levels_abb, rep, 2)),
        type = rep(c("Kitchen", "Site"), length(delivery_day_levels_abb)),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE)
      
      }
    )
}
