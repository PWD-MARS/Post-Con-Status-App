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
  #reactable for reactable tables
  library(reactable)
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
  postcon_status_notes <- dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(date) as fq from fieldwork.tbl_postcon_status")
  
#post-con status types
  postcon_status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup")
  
#status list
  
  status_choice <- postcon_status_lookup %>%
    select(status) %>%
    distinct() %>%
    pull

#system ids
  system_id <- odbc::dbGetQuery(poolConn, paste0("select distinct system_id from external.mat_assets where system_id like '%-%'")) %>% 
    dplyr::arrange(system_id) %>%  
    dplyr::pull()
  
# Fiscal uarters 
  fq <- dbGetQuery(poolConn, "SELECT * FROM admin.tbl_fiscal_quarter_lookup")
  
# get the current fiscal year and list of all years from start of data (2012) to now
  current_fy <- lubridate::today() %m+% months(6) %>% year()
  start_fy <- 2012
  years <- start_fy:current_fy %>% sort(decreasing = TRUE)
  
  

  # Define UI
  ui <- fluidPage(
    
    # Application title
    titlePanel("Post construction Status App"),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
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
        selectInput("status", "Post Construction Status", choices = status_choice, selected = NULL),
        
        #1.3 DL Button --------
        downloadButton("download_table", "Download")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DTOutput("postcon_table")
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
    
    
    # Prep the post-con status table for mainpanel
    rv$postcon_table <- reactive({
      postcon_table <- postcon_status_notes %>%
        inner_join(postcon_status_lookup, by = "postcon_status_lookup_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = date, Quarter = fq, Notes = notes)
      
      return(postcon_table)
    })
    
    output$postcon_table <- renderDataTable(rv$postcon_table(),
                                            selection = 'single', 
                                            style = 'bootstrap',
                                            class = 'table-responsive, table-hover', 
                                            rownames = FALSE)
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  

  

  
  
  