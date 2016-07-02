library(shiny)
library(googlesheets)
library(RColorBrewer)
suppressMessages(library(dplyr))
suppressMessages(library(tm))
library(wordcloud)
suppressMessages(library(ggplot2))
library(stringr)
suppressMessages(library(shinyjs))
library(dygraphs)
suppressMessages(library(data.table))
suppressMessages(library(DT))

shinyServer(function(input, output, session) {
  responses <- reactivePoll(3.6 * 10^9, # 1 hr refresh
                            session,
                            checkFunc = function(){
                                            gs.profile <- gs_user()

                                            if (is.null(gs.profile)){
                                              googlesheets::gs_auth(token = "gc_camp_gs_token.rds")
                                            }
                                        },
                            valueFunc = function(){
                                            withProgress(message = 'Retrieving data',
                                                         detail = 'May take a while...', value = 0, {
                                             for (i in 1:15) {
                                               incProgress(1/15)
                                               Sys.sleep(0.25)
                                             }
                                           })
                                            ss <- googlesheets::gs_title("GC CAMP 2016 (Responses)") %>%
                                                    gs_read_csv()
                                        }
                )

  # sheet <- read.csv("gc_camp2016.csv", stringsAsFactors = FALSE)

  cloudData <- eventReactive(input$selInpt_column, {
    
    columns <- responses()
    
    # print(columns)
    
    data <- as.character(columns[,input$selInpt_column])
    
    # Remove possible empty strings
    data <- data[data != ""]
    # To lower case
    data <- str_to_lower(data)

    corp <- Corpus(VectorSource(data))
    corp <- tm_map(corp, PlainTextDocument)
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, removeWords, stopwords("english"))

    tdm <- TermDocumentMatrix(corp)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)

  }, ignoreNULL = FALSE)
  
  dtData <- reactive({
    responses() %>% select(c(Timestamp, CHURCH, MINISTRY))
  })
  
  output$gc_camp_clouds <- renderPlot({
    pal <- brewer.pal(9,"Greens")
    pal <- pal[-(1:4)]

    if (!is.null(input$selInpt_column) && input$selInpt_column != ""){
      d <- cloudData()
      
      withProgress(message = 'Buildig cloud...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                     }
      })
      
      wordcloud(d$word,
                d$freq,
                scale = c(input$sldInpt_cloud_scale_a, 
                          input$sldInpt_cloud_scale_b),
                min.freq = 1,
                colors = pal,
                random.color = TRUE
      )
    }
  })
  
  output$gc_camp_charts <- renderPlot({
    statData <- responses()
    
    statData <- statData %>% 
                select(GENDER) %>%
                group_by(GENDER) %>% 
                summarise(Count = n())
    
    withProgress(message = 'Buildig stats',
                 detail = 'May take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
    })
    
    ggplot(statData, aes(x=GENDER, y=Count)) +
      geom_bar(stat = "identity", fill="darkgreen") +
      geom_text(aes(label=paste(Count, " regs.")), vjust=2.5, colour="white", size=24) +
      geom_text(aes(label=paste(Count/sum(Count) * 100, "%")), 
                vjust=4.5, colour="white", size=24) +
      xlab("") + ylab("") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border =  element_blank(),
            text = element_text(colour="forestgreen"))
  })

  output$gc_camp_dygraph <- renderDygraph({
    dyData <- responses()
    
    dyData$Timestamp <- as.Date(dyData$Timestamp, "%m/%d/%Y")
    
    dyData <- dyData %>% group_by(Timestamp) %>% summarise(Count = n())
    
    tot_reg <- sum(dyData$Count)
    
    dyData <- data.table(Date = dyData$Timestamp,
                         Count = dyData$Count)
    
    ts <- as.xts.data.table(dyData)
    
    withProgress(message = 'Buildig graph',
                 detail = 'May take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
    })
    
    dygraph(ts, main = "Registrations") %>%
      dyAxis("y", label = "") %>%
      dyRangeSelector(height = 20, 
                      strokeColor = "", 
                      fillColor = "forestgreen") %>%
      dyOptions(colors = "forestgreen",
                axisLineColor = "forestgreen",
                axisLabelColor = "forestgreen",
                drawAxesAtZero = FALSE) %>%
      dyHighlight(highlightCircleSize = 6) %>%
      dyAnnotation("2016-06-25", 
                   text = paste("Total: ", tot_reg), 
                   attachAtBottom = TRUE, 
                   width = 70,
                   height = 20,
                   tooltip = "Total Registrations")
  })
  
  output$gc_camp_raw_data <- DT::renderDataTable({
    
    withProgress(message = 'Buildig table',
                 detail = 'May take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
    })
    
    DT::datatable(dtData(), colnames = c("Day of Registration", "Church", "Ministry"),
                  options = list(
                    sDom  = '<"top">ft<"bottom">ip', # remove search filter
                    pageLength = 20,
                    initComplete = JS(# change background of tables
                      "function(settings, json) {",
                      "$(this.api().table().header())
                                .css({'background-color': 'forestgreen', 'color': '#fff'});",
                      "}"
                    )
                  )
                ) %>%
      formatDate(columns = "Timestamp", method = "toDateString")
  })
  
  observeEvent(input$selInpt_column, {
    shinyjs::toggleState(id = "sldInpt_cloud_scale_a", 
                         condition = nchar(input$selInpt_column) != 0)
    
    shinyjs::toggleState(id = "sldInpt_cloud_scale_b",
                         condition = nchar(input$selInpt_column) != 0)
  })
  
  observe({
    shinyjs::toggleState(id = "selInpt_column",
                         condition = input$tabst_panels == "WordClouds")
  })
})