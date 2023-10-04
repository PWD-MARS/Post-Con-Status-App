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
  library(DBI)
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

  status_lookup <- dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup")
  
#status list
  status_choice <- status_lookup %>%
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
  ui <- tagList(useShinyjs(), navbarPage("Post-Construction Status", id = "TabPanelID",
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
                              
                              mainPanel(
                                strong(span(textOutput("table_name"), style = "font-size:22px")),
                                reactableOutput("postcon_table")
                              )
                            )
                            ),
                   tabPanel("Add/Edit Post-Construction Status", value = "add_edit", 
                   titlePanel("Add/Edit Post-Construction Status and Notes"), 
                   sidebarLayout(
                     
                     sidebarPanel(
                       selectInput("system_id", "System ID", choices = c("", system_id_all) , selected = ""),
                       conditionalPanel(condition = "input.create_status === false", selectInput("status_edit", "Post Construction Status", choices = c("", status_choice) , selected = "")),
                       checkboxInput("create_status","Create New Post-Con Status?", 
                                     value = FALSE),
                       conditionalPanel(condition = "input.create_status === true", 
                                        textAreaInput("new_status", "New Post Construction Status:", height = '50px')),                       
                       dateInput("date",label = "Date",value = NULL),
                       textAreaInput("note", "Post-Construction Note:", height = '85px'),
                       disabled(actionButton("save_edit", "Save The Post-Con Status/Notes")),
                       actionButton("clear_pcs", "Clear All Fields")
                       
                     ),
                     mainPanel(
                       conditionalPanel(condition = "input.system_id",
                                        h4(textOutput("current_header")),
                                        reactableOutput("sys_current_pc_table"),
                                        h4(textOutput("past_header")), 
                                        reactableOutput("sys_past_pc_table"))
                     )
                   ))
  )
  )
  
  # Server logic
  server <- function(input, output, session) {

    #initialzie reactive values
    rv <- reactiveValues()
  
    # load required tables here
    #post-con status notes
    rv$postcon_notes <- reactive(dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(note_date) as fq from fieldwork.tbl_postcon_notes"))
    
    #post-con status 
    rv$postcon_status <- reactive(dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(status_date) as fq from fieldwork.tbl_postcon_status"))
    
    #current status
    rv$postcon_status_current <- reactive(rv$postcon_status() %>%
      group_by(system_id) %>%
      dplyr::summarise(status_date = max(status_date)) %>%
      ungroup %>%
      inner_join(rv$postcon_status(), by = c("system_id"="system_id","status_date"="status_date")))
    
    
    # Most recent note of most recent stats
    rv$recent_notes <- reactive(rv$postcon_status_current() %>%
      inner_join(rv$postcon_notes(), by = "postcon_status_uid") %>%
      select(system_id, note_date, notes) %>%
      arrange(desc(note_date)) %>%
      group_by(system_id) %>%
      summarise(notes = notes[1]))
    
    # Date of Most recent note of most recent stats
    rv$recent_notes_date <- reactive(rv$postcon_status_current() %>%
      inner_join(rv$postcon_notes(), by = "postcon_status_uid") %>%
      select(system_id, note_date, notes) %>%
      arrange(desc(note_date)) %>%
      group_by(system_id) %>%
      summarise(note_date = note_date[1]))
    
    #post-con status types
    rv$postcon_status_lookup <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup"))
    
    # join status 
    rv$postcon_status_dl <- reactive(rv$postcon_status() %>%
      inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid"))
    
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
        
       rv$postcon_status_current() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq, postcon_status_uid)
        
      } else{
        
       rv$postcon_status_current() %>%
          inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
          filter(status == input$status) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq, postcon_status_uid)
        
      }
    )
    
    # quarter based 
    rv$pc_status__q <- reactive( 
      if(input$status == "") {
      rv$postcon_status_current() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
        filter(status_date >= as.Date(rv$start_date()) & status_date <= as.Date(rv$end_date())) %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq, postcon_status_uid)
      
      } else {
        
        rv$postcon_status_current() %>%
          inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
          filter(status_date >= as.Date(rv$start_date()) & status_date <= as.Date(rv$end_date())) %>%
          filter(status == input$status) %>%
          select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq, postcon_status_uid)
        
      }
    )
    
    rv$pc_status <- reactive(ifelse(input$date_range == "To-Date", return(rv$pc_status_todate()), return(rv$pc_status__q())))

    output$postcon_table <- renderReactable(
      reactable(rv$pc_status() %>% select(-postcon_status_uid), 
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
                  nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$pc_status()$postcon_status_uid[index], ] %>%
                    arrange(desc(note_date)) %>%
                    select(`Note Date`= note_date, Notes = notes)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes, columns = list(
                                   `Note Date` = colDef(width = 100),
                                    Notes = colDef(width = 1000)
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
        
        df_list <- list(rv$postcon_status_dl())
        write.xlsx(x = df_list , file = filename)
      }
    )
### Second tab: ADD/EDIT Post-Construction Status 
    
    # Create a reactiveVal to store the selected system_id
    selected_system_id <- reactiveVal(NULL)
    selected_status <- reactiveVal(NULL)
    selected_date <- reactiveVal(NULL)
    selected_note <- reactiveVal(NULL)
  
    observeEvent(input$status_selected, {
      if (!is.null(input$status_selected)) {
        # Get the selected System ID from the clicked row
        selected_system_id(rv$pc_status()$`System ID`[input$status_selected])
        # selected_status(rv$pc_status()$`Post Construction Status`[input$status_selected])
        # selected_note(postcon_notes %>%
        #                 filter(postcon_status_uid == rv$pc_status()$postcon_status_uid[input$status_selected]) %>%
        #                 filter(note_date == max(note_date)) %>%
        #                 select(notes) %>%
        #                 pull)
        # Switch to the "Add/Edit Post-Construction Status" tab
        updateTabsetPanel(session, "TabPanelID", selected = "add_edit")
      }
    })

    #Modify the 'system_id' select input to use the reactiveVal
    observe({
      if (!is.null(selected_system_id())) {
        updateSelectInput(session, "system_id", selected = selected_system_id())
        # updateSelectInput(session, "status_edit", selected = selected_status())
        # updateSelectInput(session, "date", selected = Sys.Date())
        # updateTextAreaInput(session, "note", value = selected_note())
        
        
      }
    })
  
    #table header-current
    output$current_header <- renderText(
      paste("Current Post Construction Status for System ", input$system_id)
    )
    #table header-past
    output$past_header <- renderText(
      paste("Previous Post Construction Status for System ", input$system_id)
    )
    
    # current Post-con of a system
    rv$Current_sys_status <- reactive(
      
      rv$postcon_status_current() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq, postcon_status_uid) %>%
        filter(`System ID` == input$system_id)
      
    )
    
    # all post-cons of a system
    rv$all_sys_status <- reactive(
      
      rv$postcon_status() %>%
        inner_join(rv$postcon_status_lookup(), by = "postcon_status_lookup_uid") %>%
        select(`System ID` = system_id, `Post Construction Status` = status, `Date Assigned` = status_date, Quarter = fq, postcon_status_uid) %>%
        filter(postcon_status_uid %!in% rv$postcon_status_current()$postcon_status_uid) %>%
        filter(`System ID` == input$system_id) %>%
        arrange(desc(`Date Assigned`))
      
    )
    
    # current table 
    output$sys_current_pc_table <- renderReactable(
      reactable(rv$Current_sys_status()  %>%
                  select(-postcon_status_uid),
                fullWidth = TRUE,
                selection = "single",
                searchable = TRUE,
                onClick = "select",
                selectionId = "current_status_selected",
                #searchable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25,
                height = 400, 
                details = function(index) {
                  nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$Current_sys_status()$postcon_status_uid[index], ] %>%
                    arrange(desc(note_date)) %>%
                    select(`Note Date`= note_date, Notes = notes)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes,
                                           columns = list(
                                                          `Note Date` = colDef(width = 100),
                                                           Notes = colDef(width = 1000)
                                 ), 
                                 outlined = TRUE)
                  )
                }
                
      ))
      
    # past table
    output$sys_past_pc_table <- renderReactable(
      reactable(rv$all_sys_status() %>% 
                  select(-postcon_status_uid),
                fullWidth = TRUE,
                selection = "single",
                searchable = TRUE,
                onClick = "select",
                selectionId = "past_status_selected",
                #searchable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(25, 50, 100),
                defaultPageSize = 25,
                height = 400, 
                details = function(index) {
                  nested_notes <- rv$postcon_notes()[rv$postcon_notes()$postcon_status_uid == rv$all_sys_status()$postcon_status_uid[index], ] %>%
                    arrange(desc(note_date)) %>%
                    select(`Note Date`= note_date, Notes = notes)
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_notes, 
                                           columns = list(
                                                     `Note Date` = colDef(width = 100),
                                                      Notes = colDef(width = 1000)
                                                       ), 
                                           outlined = TRUE)
                  )
                }
                
      ))
    
    # when a system id is selected 
    observeEvent(input$system_id, {

      # reset("status_edit")
      # reset("date")
      # reset("note")
      
   
      updateSelectInput(session, "status_edit", selected = "")
      updateSelectInput(session, "date", selected = Sys.Date())
      updateTextAreaInput(session, "note", value = "")
      reset("create_status")
      reset("new_status")
        
    })
    
    # reset status if new status is being created
    observeEvent(input$create_status == TRUE,{
      reset("status_edit")
      disable("save_edit")
      })
    
    
    # system_id, data, and post_con status field can't remain blank
    observe(toggleState(id = "save_edit", (input$system_id != "" 
                                           & length(input$date) > 0
                                           & (input$status_edit != "" |(input$create_status == TRUE & input$new_status !="" )))))
    
    
    #when a row any of the tables in add/edit is clicked
    observeEvent(input$current_status_selected, {
      #deselect from other tables
      updateReactable("sys_past_pc_table", selected = NA)
      selected_status(rv$Current_sys_status()$`Post Construction Status`[input$current_status_selected])
      selected_date(rv$recent_notes_date() %>%
                      filter(system_id == input$system_id) %>%
                      select(note_date) %>%
                      pull)
      selected_note(rv$recent_notes() %>%
                      filter(system_id == input$system_id) %>%
                      select(notes) %>%
                      pull) 
      
      updateSelectInput(session, "status_edit", selected = selected_status())
      updateTextAreaInput(session, "note", value = selected_note())
      updateSelectInput(session, "date", selected = selected_date())
      reset("create_status")
      reset("new_status")      

    })

    
    #when a row any of the tables in add/edit is clicked
    observeEvent(input$past_status_selected, {
      #deselect from other tables
      updateReactable("sys_current_pc_table", selected = NA)
      
      # selected_system_id(rv$all_sys_status()$`System ID`[input$past_status_selected])
      selected_status(rv$all_sys_status()$`Post Construction Status`[input$past_status_selected])
      # selected_date(rv$all_sys_status()$`Date Assigned`[input$past_status_selected])
      selected_date(rv$postcon_notes() %>%
                      filter(postcon_status_uid == rv$all_sys_status()$postcon_status_uid[input$past_status_selected]) %>%
                      arrange((desc(note_date))) %>%
                      select(note_date) %>%
                      pull %>%
                      .[1])
      selected_note(rv$postcon_notes() %>%
                      filter(postcon_status_uid == rv$all_sys_status()$postcon_status_uid[input$past_status_selected]) %>%
                      arrange((desc(note_date))) %>%
                      select(notes) %>%
                      pull %>%
                      .[1])
      
      # updateSelectInput(session, "system_id", selected = selected_system_id())
      updateSelectInput(session, "status_edit", selected = selected_status())
      updateTextAreaInput(session, "note", value = selected_note())
      updateSelectInput(session, "date", selected = selected_date())
      reset("create_status")
      reset("new_status")      
      
    })
    
    
    observeEvent(input$clear_pcs, {
      showModal(modalDialog(title = "Clear All Fields", 
                            "Are you sure you want to clear all fields on this tab?", 
                            modalButton("No"), 
                            actionButton("confirm_clear_pcs", "Yes")))
    })
    
    
    observeEvent(input$confirm_clear_pcs, {
      reset("system_id")
      reset("status_edit")
      reset("date")
      reset("note")
      reset("current_header")
      reset("sys_current_pc_table")
      reset("past_header")
      reset("sys_past_pc_table")
      reset("create_status")
      reset("new_status")
      removeModal()
    })
    
    
    #add/edit button toggle
    rv$label <- reactive(if(length(input$current_status_selected) == 0 & length(input$past_status_selected) == 0) "Save The Post-Con Status/Notes" else "Edit Selected")
    observe(updateActionButton(session, "save_edit", label = rv$label()))
    
    
    
    
    ### On click "save_edit"
    
    observeEvent(input$save_edit, {

      
      
      # get the uid
      pc_uid <-  rv$postcon_status() %>%
        select(postcon_status_uid) %>%
        pull %>%
        max + 1
      
      if(length(input$current_status_selected) == 0 & length(input$past_status_selected) == 0){
        if(input$create_status == FALSE){
          
          rv$new_status <- reactive(data.frame(system_id = input$system_id,
                                   postcon_status_lookup_uid = rv$postcon_status_lookup() %>%
                                     filter(status == input$status_edit) %>%
                                     select(postcon_status_lookup_uid) %>%
                                     pull,
                                   status_date = input$date,
                                   postcon_status_uid = pc_uid))
          
          rv$new_note <- reactive(data.frame(note_date = input$date,
                                 notes = input$note,
                                 postcon_status_uid = pc_uid))
          
          
          odbc::dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_status"), rv$new_status(), append= TRUE, row.names = FALSE )
          odbc::dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_notes"), rv$new_note(), append= TRUE, row.names = FALSE )
          
          # rerun queries
          #post-con status notes
          rv$postcon_notes <- reactive(dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(note_date) as fq from fieldwork.tbl_postcon_notes"))
          
          #post-con status 
          rv$postcon_status <- reactive(dbGetQuery(poolConn, "select *, data.fun_date_to_fiscal_quarter(status_date) as fq from fieldwork.tbl_postcon_status"))
          
          #post-con status types
          rv$postcon_status_lookup <- reactive(dbGetQuery(poolConn, "select * from fieldwork.tbl_postcon_status_lookup"))
          
          reset("status_edit")
          reset("date")
          reset("note")
          reset("current_header")
          reset("sys_current_pc_table")
          reset("past_header")
          reset("sys_past_pc_table")
          reset("create_status")
          reset("new_status")
          
            
          
          
          
        }
      } else if(length(input$current_status_selected) != 0){
        if(input$create_status == FALSE){
          
          # get the uid 
          pc_lookup_uid_current <- rv$postcon_status_lookup() %>%
            filter(status == rv$Current_sys_status()$`Post Construction Status`[input$current_status_selected]) %>%
            select(postcon_status_lookup_uid) %>%
            pull
          
          pc_uid_current <- rv$Current_sys_status()$postcon_status_uid[input$current_status_selected]
          
          edit_status_current <- paste0(
            "Update fieldwork.tbl_postcon_status SET system_id ='", input$system_id,"', postcon_status_lookup_uid = ", pc_lookup_uid_current,
            ", status_date ='", input$date,"' where postcon_status_uid = ", pc_uid_current)
            
            
          
          cat(edit_status_current)
          
        
        }
      }
    }
    )
      


  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  

  

  
  
  