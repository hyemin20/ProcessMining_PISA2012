###' ###########################################################################'
###' 
###' Project(project name): Process Mining_PISA2012
###' 
###' Category(stage in the project): 4_Analysis
###' 
###' Task(specific task in the category):descriptive statistics
###' 
###' data(data source): `PISA 2012 CBA 038q01`
###' 
###' date: 2022.10.22
###' 
###' Author: Hyemin Park(`hyemin.park@snu.ac.kr`)
###' 
###'

###' ###########################################################################'
###' 
###' Basic settings
###' 
###' 

### Start with clean state
gc(); rm(list=ls())


### Call libraries
library(rsconnect); library(readr); library(tidyverse); library(shiny); library(shinydashboard); library(quantmod);library(plyr)
library(bupaR); library(edeaR); library(reshape); library(stringr); library(stringi); library(processmapR); library(petrinetR); library(DiagrammeR)
library(pm4py); library(dplyr); library(reticulate); library(quantmod); library(eventdataR); library(RColorBrewer)


install.packages('processmapR')

###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
original <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_11_logdata_cluster.csv",
                     header = TRUE, stringsAsFactors = FALSE)



###' ###########################################################################'
###' 
###' Data for event log
###' 
###' 


names(original)
names(df)

a <- original %>%
  filter(cluster == 1)
summary(a)
lapply(a, mean)

### merge
df_A <- original

# df_D %>%
#   filter(is.na(cluster)) %>%
#   distinct(ID)

df_D <- df_A[!is.na(df_A$cluster),]
df_D

table(df$cluster)


### Timestamp 
names(df_D)
df_D <- df_D[,-17]

minute100 <- df_D$time%/%60
second100 <- df_D$time%%60
second100 <- floor(second100)

minute200 <- str_pad(minute100, 2, pad = "0")
second200 <- str_pad(second100, 2, pad = "0")
minutesecond100 <- paste0("14", ":", minute200,":", second200)
df_D$timestamp <- paste("2021-12-22", minutesecond100)
df_D$timestamp <- strptime(df_D$timestamp, "%Y-%m-%d %H:%M:%S", tz="EST5EDT")
class(df_D$timestamp)
df_D$timestamp <- as.POSIXct(df_D$timestamp)
class(df_D$timestamp)

sum(is.na(df_D$timestamp))
df_D <- df_D[!is.na(df_D$timestamp),]

table(df_D$credit)


### New Instance ID
df_D$new_event_number <- str_pad(df_D$number, 5, pad = "0")
df_D$new_instance_id <- paste0(df_D$ID, "-", df_D$new_event_number)


### make event_log
event_log <- eventlog(df_D,
                      case_id = "ID", 
                      activity_id = "event_value", 
                      activity_instance_id = "new_instance_id", 
                      lifecycle_id = "event",
                      timestamp = "timestamp", 
                      resource_id = "cnt")


event_log %>%
  filter(cluster == 1) %>%
  filter_trace_frequency(percentage = 0.8) %>%
  processmap(type = frequency("relative_case", color_edges = "Accent"), 
              type_nodes = frequency("relative_case", color_scale = "Blues"),
              type_edges = performance(mean, "secs", color_edges = "Accent", flow_time = "idle_time"))




###' ###########################################################################'
###' 
###' Shiny: ui
###' 
###' 


### Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("PISA 2012 CPS Process Data"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("cluster", "Select a Country", 
                  choices = distinct(df_D, cluster),
                  selected = 1),
      width = 3,
      
    ),
    
    
    mainPanel(
      fluidRow(splitLayout(grVizOutput("plot1"))),
    ))
)


###' ###########################################################################'
###' 
###' Shiny: server
###' 
###' 


### Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##1
  output$plot1 <- renderGrViz({
    
    event_log %>%
      filter(cluster == input$cluster) %>%
      filter_trace_frequency(percentage = 0.8) %>%
      process_map(type = frequency("relative_case", color_edges = "Accent"), 
                  type_nodes = frequency("relative_case", color_scale = "Blues"),
                  type_edges = performance(mean, "secs", color_edges = "Accent", flow_time = "idle_time"))
    
    
    #process_map(type_nodes = frequency("relative_case", color_scale = "Blues"),
    #            type_edges = performance(mean, "secs", color_edges = "Accent", flow_time = "idle_time"))
    
    #process_map(type = frequency("relative_case", color_edges = "Accent"), 
    #            type_nodes = frequency("relative_case", color_scale = "Blues"))
    
    
    # process_map(type = frequency("relative"),
    #             type_nodes = frequency("relative", color_scale = "Blues"),
    #             type_edges = frequency(color_edges = "Accent"))
    # 
    
    
    # 
    # process_map(type_nodes = frequency("relative_case", color_scale = "Blues"),
    #             type_edges = performance(mean, "secs", color_edges = "Accent", flow_time = "idle_time"))
    
    
    
  })
  
}


###' ###########################################################################'
###' 
###' Shiny: run
###' 
###' 


### Run the application 
shinyApp(ui = ui, server = server)

