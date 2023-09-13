# Post-Con Status App v 1.0.0
# Author: Farshad Ebrahimi
# Last changed: 9/6/2023

# SET UP
#0.0: load libraries --------------
  #shiny
  library(shiny)
  #pool for database connections
  library(pool)
  #odbc for database connections
  library(odbc)
  #tidyverse for data manipulations
  library(tidyverse)
  #shinythemes for colors
  library(shinythemes)
  #lubridate to work with dates
  library(lubridate)
  #shinyjs() to use easy java script functions
  library(shinyjs)
  #DT for datatables
  library(DT)
  #reactable
  library(reactable)
  #reactable for reactable tables
  library(reactable)
  #excel download
  library(xlsx)
#Not in logical
  `%!in%` <- Negate(`%in%`)

#0.1: database connection and global options --------

#set default page length for datatables
  options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
  poolConn <- dbPool(odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))


#disconnect from db on stop 
  onStop(function(){
    poolClose(poolConn)
  })

#js warning about leaving page
  jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

# load required tables here
#post-con status notes
  postcon_notes <- dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(note_date) as fq from fieldwork.tbl_postcon_notes")
  
#post-con status 
  postcon_status <- dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(status_date) as fq from fieldwork.tbl_postcon_status")
#current status
  postcon_status_current <- postcon_status %>%
    group_by(system_id) %>%
    summarise(postcon_status_uid, postcon_status_lookup_uid, status_date = max(status_date), fq)
  
  
#post-con status types
  postcon_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup")
  
# join status 
  postcon_status_dl <- postcon_status %>%
    inner_join(postcon_status_lookup, by = "postcon_status_lookup_uid")
  
#status list
  status_choice <- postcon_status_lookup %>%
    select(status) %>%
    distinct() %>%
    pull

#system ids
  # comprehensive list
  system_id_all <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
    dplyr::arrange(system_id) %>%  
    dplyr::pull()
  # those with post-con status
  system_id_postcon <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from fieldwork.tbl_postcon_status")) %>% 
    dplyr::arrange(system_id) %>%  
    dplyr::pull()
  
# Fiscal uarters 
  fq <- dbGetQuery(poolConn, "SELECT * FROM admin.tbl_fiscal_quarter_lookup")
  
# get the current fiscal year and list of all years from start of data (2012) to now
  current_fy <- lubridate::today() %m+% months(6) %>% year()
  start_fy <- 2012
  years <- start_fy:current_fy %>% sort(decreasing = TRUE)
  
  # Define UI
  ui <- navbarPage("Post-Construction Status", 
                   #1.1 Unmonitored Active SMPs -------
                   tabPanel("Post-Construction Status Table", value = "status", 
                            titlePanel("Current Post-Construction Status Table"),
                            sidebarLayout(
                              
                              sidebarPanel(
                                #selectInput("system_id_pc", "System ID", choices = system_id_postcon, selected = NULL),
                                selectInput("date_range", "Date Range", choices = c("To-Date", "Select Range")),
                                conditionalPanel(condition = "input.date_range == 'Select Range'", 
                                                 fluidRow(column(6,
                                                                 selectInput("start_fy", "Start Fiscal Year (FY)", choices = years)),
                                                          column(6,selectInput("start_quarter", "Start Fiscal Quarter", 
                                                                               choices = c("Q1" = "7/1", "Q2" = "10/1","Q3" = "1/1", "Q4" = "4/1")))),
                                                 fluidRow(column(6,
                                                                 selectInput("end_fy", "End Fiscal Year (FY)", choices = years)),
                                                          column(6,selectInput("end_quarter", "End Fiscal Quarter", 
                                                                               choices = c("Q1" = "9/30", "Q2" = "12/31","Q3" = "3/31", "Q4" = "6/30"))))
                                ), 
                                selectInput("status", "Post Construction Status", choices = c("", status_choice) , selected = ""),
                                #1.3 DL Button --------
                                downloadButton("download_table", "Download")
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                strong(span(textOutput("table_name"), style = "font-size:22px")),
                                reactableOutput("postcon_table")
                              )
                            )
                            )
  )
  
  # Server logic
  server <- function(input, output) {

    #initialzie reactive values
    rv <- reactiveValues()
    
    #create a date style for headers
    sf <- lubridate::stamp("March 1, 1999", orders = "%B %d, %Y")
    
    #convert FY/Quarter to a real date
    rv$start_date <- reactive(lubridate::mdy(paste0(input$start_quarter, "/", ifelse(input$start_quarter == "7/1" | input$start_quarter == "10/1", as.numeric(input$start_fy)-1,input$start_fy))))
    rv$end_date <- reactive(lubridate::mdy(paste0(input$end_quarter, "/", ifelse(input$end_quarter == "9/30" | input$end_quarter == "12/31", as.numeric(input$end_fy)-1,input$end_fy))))

    
    output$table_name <- renderText(ifelse(input$date_range == "To-Date", paste("Current Post-Con Status to Date:", input$status), paste("Current Post-Con Status", " Assigned between ", rv$start_date(), " and ", rv$end_date(),": ",input$status, sep = "")))
    
### First tab: Post-Construction Status Table
    # todate 
    rv$pc_status_todate <- reactive(
      if(input$status == ""){
        
       postcon_status_current %>%
        inner_join(postcon_status_lookup, by = "postcon_status_lookup_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq)
        
      } else{
        
       postcon_status_current %>%
          inner_join(postcon_status_lookup, by = "postcon_status_lookup_uid") %>%
          filter(status == input$status) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq)
        
      }
    )
    
    # quarter based 
    rv$pc_status__q <- reactive( 
      if(input$status == "") {
      postcon_status_current %>%
        inner_join(postcon_status_lookup, by = "postcon_status_lookup_uid") %>%
        filter(status_date >= as.Date(rv$start_date()) & status_date <= as.Date(rv$end_date())) %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq)
      
      } else {
        
      postcon_status_current %>%
          inner_join(postcon_status_lookup, by = "postcon_status_lookup_uid") %>%
          filter(status_date >= as.Date(rv$start_date()) & status_date <= as.Date(rv$end_date())) %>%
          filter(status == input$status) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq)
        
      }
    )
    
    
    rv$pc_status <- reactive(ifelse(input$date_range == "To-Date", return(rv$pc_status_todate()), return(rv$pc_status__q())))

    # output$postcon_table <- renderDataTable(rv$pc_status(),
    #                                         selection = 'single', 
    #                                         style = 'bootstrap',
    #                                         class = 'table-responsive, table-hover', 
    #                                         rownames = FALSE)
    # 
    # 
    output$postcon_table <- renderReactable(
      reactable(rv$pc_status(), 
                fullWidth = TRUE,
                selection = "single",
                searchable = TRUE,
                onClick = "select",
                selectionId = "status_selected",
                #searchable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25,
                height = 1000, 
                details = function(index) {
                  nested_notes <- postcon_notes[postcon_notes$system_id == rv$pc_status()$`System ID`[index], ] %>%
                    arrange(desc(note_date)) %>%
                    select(`Note Date`= note_date, Notes = notes)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes, columns = list(
                                   `Note Date` = colDef(width = 100),
                                    Notes = colDef(width = 1050)
                                 ), outlined = TRUE)
                  )
                }
                
      ))
    
    
    
    
    #download button
    output$download_table <- downloadHandler(
      
      filename = function() {
        paste("Post-Con Table", "_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(filename){
        
        df_list <- list(postcon_status_dl)
        write.xlsx(x = df_list , file = filename)
      }
    ) 
    
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  

  

  
  
  