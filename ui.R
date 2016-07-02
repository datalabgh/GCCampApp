library(shiny)
suppressMessages(library(shinyjs))
suppressMessages(library(dygraphs))
suppressMessages(library(DT))

shinyUI(
  bootstrapPage(#theme = "theme.css",
    title = "GC CAMP 2016 App",
    tags$style(type = "text/css", "html, body {width:100%;height:100%; margin-top:5px}"),
    tags$style(type = "text/css", ".well {background-color:#fff}"),
    
    shinyjs::useShinyjs(),
    
    sidebarPanel(width = 3,
      tags$p(shinyjs::disabled(selectInput(inputId = "selInpt_column",
                         label = "",
                         choices = c("Select Field to Build Cloud" = "",
                                     "RESIDENCE", "CHURCH", "MINISTRY", "EXPECTATIONS")))),
      tags$p(img(src = "logo.jpg", width = "100%"))
    ),
    mainPanel(
      tabsetPanel(id = "tabst_panels", type = "pills",
        tabPanel("WordClouds",
          fluidRow(
            column(2),
            column(4,
              shinyjs::disabled(
                   tags$p(sliderInput(inputId = "sldInpt_cloud_scale_a",
                                      label = "",
                                      min = 4,
                                      max = 10,
                                      value = 4,
                                      ticks = TRUE)
                                      #animate = TRUE)
                   )
              )
            ),
            column(4,
              shinyjs::disabled(
                   tags$p(sliderInput(inputId = "sldInpt_cloud_scale_b",
                                      label = "",
                                      min = 0.2,
                                      max = 1.0,
                                      value = 0.3,
                                      step = 0.1,
                                      ticks = TRUE)
                   )
              )
            ),
            column(2)
          ),
          plotOutput("gc_camp_clouds", height = "600px")
        ),
        tabPanel("Trends",
                 dygraphOutput("gc_camp_dygraph", height = "500px")
        ),
        tabPanel("Stats",
                 plotOutput("gc_camp_charts", height = "500px")
        ),
        tabPanel("Church & Ministry",
                  DT::dataTableOutput("gc_camp_raw_data")         
        )
      )
    )
  )
)