########################
#
# Combined Version
#
# shiny-dashboard.R:
# 1.28 - 1.31: plan what I want to show on my shiny app, prototype is done
#
# 2.1 - 2.7: try to show the output on the box of data dictionary
#
# 2.8 - 2.14: keep trying to show the output, and write two functions for later use
#
# shiny-test.R:
# 2.15 - 2.21: change show variables and data set in just Shiny App, and figure out show output
#
# 2.22 - 2.28: TO DO ---- SHOW DATATABLE
#              DID   ---- try to show datatable and do the updateRadioButton, but not work
#
# 3.1 - 3.7: TO DO ---- Show datatable; and update the radio button
#            DID   ---- show datatable and update the radio button
#
# 3.22 - 3.28: TO DO ---- Make a confirmed cases plot
#
# Shiny
#
########################
# outside sources

source('read_all_data_file.R')


#######################

library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)

#### show variables with key words
var_word <- column(3,
                   radioButtons("keyWordsVar", "List of key words:",
                                c("covid"="covid",
                                  "average"='avg',
                                  "rate"='rate',
                                  "case"='case',
                                  "death"="death",
                                  "test"="test",
                                  "positive"="positive",
                                  "initiated"="initiated",
                                  "completed"="completed")),
                   textOutput("var")
            )
var_show <- column(9,
                   verbatimTextOutput('showvar'))
vars <- fluidRow(var_word, var_show)

#### show datasets with key words
set_word <- column(3,
                   radioButtons("keyWordsSet", "List of key words:",
                                c('county'='county',
                                  'district'='district',
                                  'individual'='individual',
                                  'metro_all'='metro_all',
                                  'region'='region',
                                  'state'='state',
                                  'zip'='zip')),
                   textOutput("set")
            )

set_show <- column(5,
                   verbatimTextOutput('showset'))
sets <- fluidRow(set_word, set_show)

#### show data table of chosen dataset
dt_folder <- column(3,
                 radioButtons("datafolder", "Choose one folder:",
                              c('county'='county',
                                'district'='district',
                                'individual'='individual',
                                'metro_all'='metro_all',
                                'region'='region',
                                'state'='state',
                                'zip'='zip'))
                 )

dt_set <- column(3,
                 radioButtons('dataset', 'Choose one dataset:',
                              c('d1','d2','d3')),
                 textOutput('df'),
                 dataTableOutput('ds')
                 )
datatables <- fluidRow(dt_folder, dt_set)


#### make a confirmed cases plot
datafolder <- column(3,
                     radioButtons("datafolder2", "Choose one folder:",
                                  c('county'='county',
                                    'district'='district',
                                    'individual'='individual',
                                    'metro_all'='metro_all1',
                                    'region'='region',
                                    'state'='state',
                                    'zip'='zip')),
                     textOutput('df2')
                     )

datachoice <- column(3,
                     radioButtons('dataset2', 'Choose one dataset:',
                                  c('c1','c2','c3')),
                     textOutput('ds2')
                     )

datavariable <- column(3,
                       radioButtons('datavariable2', 'Choose one variable:',
                                    c('v1','v2','v3')),
                       textOutput('dv2'),
                       plotOutput('cases')
                       )
dataplots <- fluidRow(datafolder, datachoice, datavariable)

#####################
# UI
#####################

ui <- fluidPage(
        titlePanel("COVID Data Dashboard"),
        navlistPanel(
          id = 'tabset',
          "Data Dictionary",
          tabPanel("Variables Include Key Words", vars),
          tabPanel('Datasets Include Key Words', sets),
          tabPanel('Datatable',datatables),
          "Confirmed Cases",
          tabPanel("Confirmed Cases Plot", dataplots),
          "Vaccination",
          tabPanel('Vaccination Rate Over Time')
        )
      )

#####################
# SERVER
#####################

server <- function(input, output, session) {
  
  #### show variables with key words
  output$var <- renderText(
    paste('You selected: ', input$keyWordsVar)
  )
  output$showvar <- renderPrint({
    printoutVariables(input$keyWordsVar)
  })
  
  #### show datasets with key words
  output$set <- renderText(
    paste('You selected: ', input$keyWordsSet)
  )
  output$showset <- renderPrint({
    for (i in printoutDatasets(input$keyWordsSet)){
      print(i)
    }
  })
  
  #### show the chosen data table
  output$df <- renderPrint({
    input$datafolder
  })
  
  observeEvent(eventExpr = paste(input$datafolder, input$dataset), {
    x <- input$datafolder
    y <- printoutDatasets(x)
    
    updateRadioButtons(
      session,
      "dataset",
      #label = paste('in folder', y),
      choices = y,
      ### TRY DIFFERENT SELECTED ### ????????????????? ### still not figure out
      selected = y
    )
  })
  
  
  output$ds <- renderDataTable({
    datatable(read.csv(paste('/Users/jiayingliang/Desktop/data4961/data/MO_HEALTH_Covid_Tracking/data/', 
                             input$datafolder,
                             '/', 
                             #eval(parse(text = paste0(input$datafolder, "[1]"))),
                             input$dataset,
                             sep = ''
                             )))
  })
  
  
  #### show the plots for chosen variables
  output$df2 <- renderPrint({
    input$datafolder2
  })
  
  output$ds2 <- renderPrint({
    input$dataset2
  })
  
  output$dv2 <- renderPrint({
    input$datavariable2
  })
  
  observeEvent(eventExpr = paste(input$datafolder2, input$dataset2, input$datavariable2), {
    x1 <- input$datafolder2
    y1 <- printoutDatasets(x1)
    
    updateRadioButtons(
      session,
      'dataset2',
      #choices = y1,
      selected = y1,
      choiceNames = y1,
      choiceValues = y1
    )
    
#    x2 <- read.csv(paste('/Users/jiayingliang/Desktop/data4961/data/MO_HEALTH_Covid_Tracking/data/', 
#                        input$datafolder2,
#                        '/', 
#                        #eval(parse(text = paste0(input$datafolder, "[1]"))),
#                        input$dataset2,
#                        sep = ''))
#    y2 <- colnames(x2)
#
#    updateRadioButtons(
#      session,
#      'datavariable2',
#      choices = y2,
#      selected = y2
#    )
    
  })
  
#  observeEvent(input$dataset2,{
#    session$reload
#  })
  
  
#  eventReactive(eventExpr = paste(input$datafolder2, input$dataset2, input$datavariable2),{
#    x <- input$dataset2
#    x <- read.csv(paste('/Users/jiayingliang/Desktop/data4961/data/MO_HEALTH_Covid_Tracking/data/', 
#                        input$datafolder2,
#                        '/', 
#                        #eval(parse(text = paste0(input$datafolder, "[1]"))),
#                        input$dataset2,
#                        sep = ''
#                        )
#                  )
#    y <- colnames(x)
#    
#    updateRadioButtons(
#      session,
#      'datavariable2',
#      choices = y,
#      selected = y
#    )
#  })

}


shinyApp(ui, server)