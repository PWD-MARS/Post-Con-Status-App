## Preprocessing the post-con notes

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
library(readxl)
#Not in logical
`%!in%` <- Negate(`%in%`)

poolConn <- dbPool(odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

# Parse dates

notes <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\07 Databases and Tracking Spreadsheets\\11 Post-Construction Status-Tracking Table\\Post-Con Status Table.xlsx", sheet = 4)

df <- str_split_fixed(notes$Notes, ": ", 2)
df <- as.data.frame(df)

df_db <- bind_cols(notes, df)


df_db <- df_db %>%
  mutate(date = case_when (nchar(V1)<11 ~ mdy(V1)))


df_db <- df_db %>% 
  mutate(notes = ifelse(V2 == "", V1, V2))

df_db_final <- df_db %>%
  select(system_id = `System ID`, status = `Post-Construction Status`, status_date = date, notes)


postcon_status_lookup <- df_db_final %>%
  select(status) %>%
  distinct()
postcon_status_lookup["postcon_status_lookup_uid"] <- 1:8

df_db_final <- df_db_final %>%
  inner_join(postcon_status_lookup, by = "status") %>%
  select(-status)




### Tables for DB

#df_db_final["postcon_status_uid"] <- 1:nrow(tbl_postcon_status)
  
tbl_postcon_status <- df_db_final %>%
  arrange(desc(status_date)) %>%
  group_by(system_id) %>%
  summarise(status_date = status_date[1], postcon_status_lookup_uid = max(postcon_status_lookup_uid)) %>%
  mutate(postcon_status_uid = 1:456)


tbl_postcon_notes <- df_db_final %>%
  select(system_id, note_date = status_date, notes) %>%
  inner_join(tbl_postcon_status, by = "system_id") %>%
  select(note_date, notes, postcon_status_uid)
  


dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_status_lookup"), postcon_status_lookup, append = TRUE)
dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_status"), tbl_postcon_status, append = TRUE)
dbWriteTable(poolConn, SQL("fieldwork.tbl_postcon_notes"), tbl_postcon_notes, append = TRUE)

