library(shiny)
library(tigris)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(shinythemes)
library(shinycssloaders)
library(ggrepel)
library(plotly)
library(ggplot2)

options(tigris_use_cache = TRUE)
#setwd('D:/ASA_Chapter_Health_Database/Code/ShinyApp') #comment out when publishing app

#load in data
mainData <- paste('./data/','ASA_Chapter_History_Final.RDA', sep = '')
featuresCategoryMap <- paste('./data/','Feature_Category_Map.RDA', sep = '')
chapLatLong <- paste('./data/','ASA_CHAP_LAT_LONG.csv', sep = '')
#stimFundsTable2 <- paste('./data/','ASA_Stimulus_Fund_Tables.RDA', sep = '')
stimFundsTable <- paste('./data/','ASA_Stimulus_Fund_Plots.RDA', sep = '')
fullPlotData <- paste('./data/','FullStatePlotData.RDA', sep = '')
ASA_Chap_History <- readRDS(mainData)
Feature_Category_Map <- readRDS(featuresCategoryMap)
chap_Name_Lat_Long <- data.table::fread(chapLatLong)
#Stimulus_Funds_Tables <- readRDS(stimFundsTable2) #could be used for 'stimulus spending' table
stimulus_stacked <- readRDS(stimFundsTable)
fullPlotData <- readRDS(fullPlotData)

#create district and region dataframe to use for selection
regionReps <- rep(c(1,2,3), each = 2)
districtReps <- c(1,2,3,4,5,6)
regionDistrictTable <- as.data.frame(cbind(regionReps, districtReps))


# Define UI ----
ui = fluidPage(
  navbarPage("ASA Historical Health Data", theme = shinytheme("lumen"),
        tabPanel("Chapter Finder", fluid = TRUE, icon = icon("globe-americas"),
                 sidebarLayout(
                   sidebarPanel(
                     titlePanel("Desired Chapter Characteristics"),
                     fluidRow(
                       column(6,
                              selectInput("metricType", label = "Metric Type", choices=unique(Feature_Category_Map$Category),
                                          selected = "Chapter Business"),
                              selectInput("chapMetrics", label = "Chapter Metrics", choices=colnames(ASA_Chap_History),
                                          selected = "ASA Member Count")
                              
                       ),
                       column(6,
                              checkboxGroupInput("regions", label = "Regions", choices=unique(regionDistrictTable$regionReps),
                                          selected = c(1, 2, 3)),
                              checkboxGroupInput("districts", label = "Districts", choices=regionDistrictTable$districtReps,
                                          selected = c(1, 2, 3, 4, 5, 6)),
                       )
                     ),
                     fluidRow(
                       column(12,
                              selectInput("chapters", label = "Chapters", choices=unique(ASA_Chap_History$`Chapter Name`),
                                          selected = NULL, multiple = TRUE)       
                       )
                     )
                   ),
                   mainPanel(
                     withSpinner(plotOutput(outputId = "state_map",
                                            click = clickOpts(id = "state_click")
                                 )
                     ),
                     fluidRow(
                       column(2, offset = 10,
                              uiOutput("state_tooltip"),
                              radioButtons(inputId = "show_NamesFinder",
                                           label = "Display Chapter Names",
                                           choices = c("Yes", "No"),
                                           selected = "No")
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(width = 9,
                              plotOutput(outputId = "metricTimeGraph",),
                              uiOutput("my_tooltip"),
                              downloadButton("downloadPlot", "Download Plot")
                       ),


                       ),
                   )
                 )
        ),
        tabPanel("Table", fluid = TRUE, icon = icon("list-alt"),
          fluidRow(
            column(11,
                   radioButtons("selAll", label = "Select All Chapters", choices = c("Yes", "No"),
                                selected = "No", inline = TRUE),
                   tags$h5("Note: Total chapters filtered on districts selected in 'Chapter Finder' tab"),
                   
            ),

            column(1,
                   div(style="display:inline-block; float:right",downloadButton('downloadTable', 'Download Table'))
            ),
          ),
          fluidRow(
            column(width = 12,
                   dataTableOutput('table')
            )
          )
        ),
        tabPanel("Summary", fluid = TRUE, icon = icon("list-alt"),
                 fluidRow(
                   column(4,
                          checkboxGroupInput("districtsSummary", label = "Districts", choices = c(1, 2, 3, 4, 5, 6),
                                             selected = c(1, 2, 3, 4, 5, 6), inline = TRUE),
                   ),
                   column(4, 
                          selectInput("yearSummary", label = "Year", choices = seq(2011, 2020, 1),
                                      selected = 2020)
                   ),
                   column(4,
                          div(style="display:inline-block; float:right",downloadButton("downloadSummary", "Download Summary Report")),
                   )
                  ),
                 fluidRow(
                       h2("Summary Statistics"),
                       column(width = 6,
                              h3("Member Counts Summary"),
                              tableOutput("chapSum"),
                              h3("Officer Information"),
                              tableOutput("officerSum"),
                       ),
                       column(width = 6,
                              h3("Chapter Activites"),
                              tableOutput("eduSum"),
                       )

                     ),
        )
  )
)
  

# Define server logic ----
server <- function(input, output, session) {
  
  ###               Data cutting, graphs, and table               ###
  ###################################################################
  
  #plots and tables to be saved
  printOuts <- reactiveValues()
  
  #filter chapter metrics based on metric type
  metricsCut <- reactive({
    Feature_Category_Map %>%
      filter(Category %in% input$metricType)
  })
  
  #change available options based on above filter
  observeEvent(metricsCut(), {
    updateSelectInput(session = session, inputId = "chapMetrics", choices = metricsCut()$Feature,
                      selected = "ASA Member Count")
  })
  
  
  #filter district based on region
  regionsCut <- reactive({
    regionDistrictTable %>%
      filter(regionReps %in% input$regions) 
  })
  #change available options based on above filter
  observeEvent(regionsCut(), {
    updateCheckboxGroupInput(session = session, inputId = "districts", choices = regionsCut()$districtReps,
                             selected = c(1,2,3,4,5,6))
  })
  
  
  #filter chapter based on district
  chaptersCut <- reactive({
    ASA_Chap_History %>%
      filter(District %in% input$districts) 
  })
  #change available options based on above filter
  observeEvent(chaptersCut(), {
    updateSelectInput(session = session, inputId = "chapters", choices = chaptersCut()$`Chapter Name`,
                       selected = NULL)
  })
  
  
  #filter data down to selected chapters
  chapsCut <- reactive({
      selChapData <- chaptersCut() %>% filter(`Chapter Name` %in% input$chapters)
  })
  
  #chaps cut based on stimulus data (only for stimulus graph)
  stimChapsCut <- reactive({
    fund_cat_chapter <- as.data.frame(table(stimulus_stacked$`Chapter Name`, stimulus_stacked$`Event Funded`, stimulus_stacked$`Report Year`))
    names(fund_cat_chapter) <- c('Chapter Name', 'Event Funded', 'Report Year', 'count')
    stimChaps <- fund_cat_chapter %>% filter(`Chapter Name` %in% input$chapters)
  })

  

  #graph
  output$metricTimeGraph = renderPlot({
    if (input$chapMetrics == "Stimulus Spending"){
      validate(
        need(length(stimChapsCut()[['Chapter Name']]) > 0, 'Please select chapters with stimulus spending')
      )
      metricPlot <- stimChapsCut() %>% ggplot(aes(fill=`Chapter Name`, y=count, x=`Event Funded`)) +
        geom_bar(position="stack", stat="identity") +
        theme(axis.text.x = element_text(angle = 90)) +
        facet_wrap(~`Report Year`)

      printOuts$metricPlot <- metricPlot

      print(metricPlot)
    }
    else if (input$chapMetrics == "Received Stimulus Funds"){
      validate(
        need(length(stimChapsCut()[['Chapter Name']]) > 0, 'Please select chapters who recieved stimulus funds')
      )
      stimChapYearCut <- chapsCut() %>% filter(`Report Year` >= 2019)
      metricPlot <- stimChapYearCut %>%
        ggplot(aes(x = `Report Year`, y = `Chapter Name`, fill = .data[[input$chapMetrics]], width=0.8, height=0.8)) +
        geom_tile() + labs(fill = input$chapMetrics) +
        scale_fill_manual(drop=FALSE, values = c("#006164", "#EDA247", "#DB4325", "#B5AFD4")) +
        scale_x_continuous(breaks = c(2019, 2020))
      printOuts$metricPlot <- metricPlot
      metricPlot
    }
    else if (input$chapMetrics == "Last Election"){
      validate(
        need(length(chapsCut()[['Chapter Name']]) > 0, 'Please select a chapter')
      )
      metricPlot <- chapsCut() %>%
        ggplot(aes(x = `Report Year`, y = `Chapter Name`, fill = chapsCut()[['withinYear']], width=0.8, height=0.8)) +
        geom_tile() + labs(fill = 'Last Election Within Year', caption = "Note: The numbers correspond to the month the election occured") +
        scale_x_continuous(breaks = seq(2011, 2020, by = 1)) + geom_text(aes(label=chapsCut()[['lastElecMonth']], colour = '#FFFFFF')) +
        scale_fill_manual(values = c("#006164", "#EDA247", "#DB4325", "#B5AFD4")) +
        scale_colour_manual(values = "#FFFFFF") + guides(color = "none")
      printOuts$metricPlot <- metricPlot
      print(metricPlot)
      #EBB207
    }
    else if (class(chapsCut()[[input$chapMetrics]]) == 'factor') {
      validate(
        need(length(chapsCut()[['Chapter Name']]) > 0, 'Please select a chapter')
      )
      metricPlot <- chapsCut() %>%
        ggplot(aes(x = `Report Year`, y = `Chapter Name`, fill = chapsCut()[[input$chapMetrics]], width=0.8, height=0.8)) +
            geom_tile() + labs(fill = input$chapMetrics) +
            scale_fill_manual(drop=FALSE, values = c("#006164", "#EDA247", "#DB4325", "#B5AFD4")) +
            scale_x_continuous(breaks = seq(2011, 2020, by = 1))
      printOuts$metricPlot <- metricPlot
      print(metricPlot)
    }
    else if (input$chapMetrics %in% c("ASA Member Count", "Non ASA Member Count", "Student Member Count")){
      par( mai=c(0,0,0,0))
      validate(
        need(length(chapsCut()[['Chapter Name']]) > 0, 'Please select a chapter')
      )
      selResponseMetric <- chapsCut() %>% select(input$chapMetrics, 'Chapter Name', 'Report Year', 'COMMCODE')
      metricPlot <- ggplot(chapsCut(), aes(chapsCut()[['Report Year']], selResponseMetric[[input$chapMetrics]],
                                           color = selResponseMetric[['Chapter Name']])) +
        geom_point() + geom_line() + labs(x = 'Report Year', y = input$chapMetrics, color = "Chapters") +
        scale_x_continuous(breaks = seq(2011, 2020, by = 1))
      printOuts$metricPlot <- metricPlot
      metricPlot
    }
    else {
      par( mai=c(0,0,0,0))
      numberTiled <- c('Social Events', 'Outside Speakers', 'Short Courses', 'Career days', 'Science Fairs')
      validate(
        need(length(chapsCut()[['Chapter Name']]) > 0, 'Please select a chapter')
      )
      if (input$chapMetrics %in% numberTiled){
        legend <- paste0('Held ', input$chapMetrics)
        metricPlot <- chapsCut() %>% 
          ggplot(aes(x = `Report Year`, y = `Chapter Name`, fill = ifelse(chapsCut()[[input$chapMetrics]] < 1, '1', '0'), width=0.8, height=0.8)) +
          geom_tile() + labs(fill = legend, caption = "Note: The numbers correspond to the number of events") +
          scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
          geom_text(aes(label=ifelse(chapsCut()[[input$chapMetrics]] != 0, chapsCut()[[input$chapMetrics]], NA), colour = '#FFFFFF')) +
          scale_fill_manual(values = c("#006164", "#EDA247"), labels = c("Yes", "No")) +
          scale_colour_manual(values = "#FFFFFF") + guides(color = "none")

        printOuts$metricPlot <- metricPlot
        print(metricPlot)
      }
      else {
        ylab = paste0('Number of ', input$chapMetrics)
        selResponseMetric <- chapsCut() %>% select(input$chapMetrics, 'Chapter Name', 'Report Year', 'COMMCODE')
        metricPlot <- ggplot(chapsCut(), aes(chapsCut()[['Report Year']], selResponseMetric[[input$chapMetrics]],
                                             color = selResponseMetric[['Chapter Name']])) +
          geom_point() + geom_line() + labs(x = 'Report Year', y = ylab, color = "Chapters") +
          scale_x_continuous(breaks = seq(2011, 2020, by = 1))
        printOuts$metricPlot <- metricPlot
        print(metricPlot)
      }
    }

  })
  
  #cut data for table
  metricTableCut <- reactive({
    if (input$selAll == 'Yes'){
      arrange(chaptersCut()) %>% mutate(inputVar = chaptersCut()[[input$chapMetrics]]) %>% select(inputVar, 'Chapter Name', 'Report Year') %>% 
        pivot_wider(names_from = sort('Report Year'), values_from = inputVar, names_sort = TRUE)
    }
    else {
      arrange(chapsCut()) %>% mutate(inputVar = chapsCut()[[input$chapMetrics]]) %>% select(inputVar, 'Chapter Name', 'Report Year') %>% 
        pivot_wider(names_from = sort('Report Year'), values_from = inputVar, names_sort = TRUE)
    }
  })
  
  #filter chapter based on district
  chaptersCut2 <- reactive({
    ASA_Chap_History %>%
      filter(District %in% input$districts) 
  })
  
  #table
  output$table = renderDataTable({
    if (input$chapMetrics == 'Stimulus Spending'){
      # metricTable <- stimChapsCut() %>% filter(stimChapsCut()[['count']] == 1) %>%
      #   pivot_wider(names_from = sort('Report Year'), values_from = 'Event Funded', names_sort = TRUE) %>% select(-'count')
      # printOuts$metricTable <- metricTable
      metricTable <- stimChapsCut() %>% filter(stimChapsCut()[['count']] == 1)
      
      printOuts$metricTable <- metricTable
  
    } 
    else {
      metricTable <- metricTableCut()
      printOuts$metricTable <- metricTable
    }
    })
  
  #download plot
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$chapMetrics, ' Plot.pdf', sep = '')},
    content = function(filename){
      pdf(filename)
      title = paste0("ASA Historical Health Data Viewer ", Sys.Date())
      print(printOuts$metricPlot + labs(title = title))
      dev.off()
    })
  
  
  #download table
  output$downloadTable <- downloadHandler(
    filename = function(){paste(input$chapMetrics, ' Table ', 
                                'ASA Historical Health Data Viewer ', 
                                Sys.Date(), '.csv', sep = '')},
    content = function(filename){
      write.csv(printOuts$metricTable, filename, row.names = FALSE)
    })
  
  ###               Data cutting, graphs, and table               ###
  ###################################################################
  
  
  ###                  States and Chapters Map                    ###
  ###################################################################
  
  
  #filter states to plot based on selected districts
  statesPlotCut <- reactive({
    filter(fullPlotData, district %in% input$districts)
  })
  
  #filter colors based on districts so plotting colors doesn't change
  plotColorsCut <- reactive({
    colors <- c("purple1","plum3", "bisque","honeydew2", "mediumseagreen", "palegreen4")
    dist = c(1, 2, 3, 4, 5, 6)
    colorSet <- as.data.frame(cbind(colors, dist))
    filter(colorSet, dist %in% input$districts)
  })
  
  #selected chapters to plot (will plot different color)
  chapsPlotCut <- reactive({
    filter(chap_Name_Lat_Long, chapterNames %in% input$chapters)
  })
  
  #chapter dots to plot
  chapsNotSelCut <- reactive({
    filter(chap_Name_Lat_Long, chapterNames %in% chaptersCut()[['Chapter Name']])
  })
  
  #find clicked chapter and select/deselect it 
  observeEvent(input$state_click, {
    chapClicked <- nearPoints(chapsNotSelCut(), input$state_click, xvar = "longitude", yvar = "latitude", threshold = 5)
    chapClickedName <- chapClicked$chapterNames
    selectedChaps <- input$chapters
    if (length(chapClickedName) > 0){
      if (chapClickedName %in% selectedChaps){
        updatedSelChaps <- str_remove(selectedChaps, chapClickedName)
      }
      else{
        updatedSelChaps <- c(selectedChaps, chapClickedName)
      }
      updateSelectInput(session = session, inputId = "chapters", choices = chaptersCut()$`Chapter Name`,
                        selected = updatedSelChaps)
    }
  })
  
  #state map
  output$state_map = renderPlot({
    par(mai=c(0,0,0,0))
    
    ggplot() +
      geom_polygon(data = statesPlotCut(), aes(x = long, y = lat, group = group, fill = as.factor(statesPlotCut()[['district']])), color = "black") +
      scale_fill_manual(values=plotColorsCut()[['colors']]) +
      geom_point(data = chapsNotSelCut(), aes(x = longitude, y = latitude), shape = 21, size = 2.2, fill = 'black') +
      geom_point(data = chapsPlotCut(), aes(x = longitude, y = latitude), col ='black', fill = '#EDA247', shape = 22, size = 3) +
      {if(input$show_NamesFinder == "Yes") geom_text_repel(data = chapsPlotCut(), aes(x = longitude, y = latitude, label = chapterNames))} +
      coord_quickmap() + theme_void() + theme(legend.position = "none") 
  })
  
  ###                  States and Chapters Map                    ###
  ###################################################################
  
  
  ###                       Summary Output                        ###
  ###################################################################
  
  #output var to be used for download later
  summaryOut <- reactiveValues()
  
  #filter data based on selected district/ year
  chaptersSumCut <- reactive({
    ASA_Chap_History %>%
      filter(District %in% input$districtsSummary & `Report Year` %in% input$yearSummary)
  })
  
  #create chapter summary table
  output$chapSum <- renderTable({
    
    attr(ASA_Chap_History$`ASA Member Count`,'label') <- 
      "Chapter Members - ASA"
    attr(ASA_Chap_History$`Non ASA Member Count`,'label') <-
      "Chapter Members - Non-ASA"
    attr(ASA_Chap_History$`Student Member Count`,'label') <-
      "Student Members"
    memtab <- 
      arsenal::tableby(~ `ASA Member Count` +
                         `Non ASA Member Count` +
                         `Student Member Count`, 
                       numeric.stats=c("median",
                                       "q1q3","range"),
                       data = chaptersSumCut())
    summaryOut$chapSum <- memtab
    memberSumTitle <- paste0("Chapters Membership Stats in ", input$yearSummary)
    as.data.frame(summary(memtab, text = TRUE,
            title = memberSumTitle), sanitize.text.function = function(x) x)
    
  })
  
  #create officer info summary table
  output$officerSum <- renderTable({
    
    attr(ASA_Chap_History$`Received Stimulus Funds`,'label') <- 
      "Received Stimulus Funds"
    attr(ASA_Chap_History$`Attended JSM or COCBM`,'label') <-
      "Attended JSM or COCBM"
    memtab <- 
      arsenal::tableby(~ `Received Stimulus Funds` +
                         `Attended JSM or COCBM`,
                       numeric.stats=c("Yes",
                                       "No"),
                       data = chaptersSumCut())
    summaryOut$officerSum <- memtab
    officerTitleYear <- paste0("Officer Information from ", input$yearSummary)
    as.data.frame(summary(memtab, text = TRUE,
            title = officerTitleYear), sanitize.text.function = function(x) x)
  })

  #create edu activies summary table
  output$eduSum <- renderTable({
    
    attr(ASA_Chap_History$`Business Meetings`,'label') <- 
      "Business Meetings"
    attr(ASA_Chap_History$`Outside Speakers`,'label') <-
      "Outside Speakers"
    attr(ASA_Chap_History$`Short Courses`,'label') <-
      "Short Courses"
    attr(ASA_Chap_History$`Career days`,'label') <-
      "Career Days"
    attr(ASA_Chap_History$`Short Courses`,'label') <-
      "Short Courses"
    attr(ASA_Chap_History$`Science Fairs`,'label') <-
      "Science Fairs"
    attr(ASA_Chap_History$`Travel Courses`,'label') <-
      "Travel Courses"
    attr(ASA_Chap_History$`Visitation Programs`,'label') <-
      "Visitation Programs"
    attr(ASA_Chap_History$`Other Activities`,'label') <-
      "Other Activities"
    memtab <- 
      arsenal::tableby(~ `Business Meetings` +
                         `Outside Speakers` +
                         `Short Courses` + 
                         `Career days` +
                         `Short Courses` +
                         `Science Fairs` +
                         `Travel Courses` + 
                         `Visitation Programs` + 
                         `Other Activities`,
                       numeric.stats=c("median",
                                       "q1q3","range"),
                       data = chaptersSumCut())
    summaryOut$eduSum <- memtab
    titleWYear <- paste0("Chapter Activites from ", input$yearSummary)
    as.data.frame(summary(memtab, text = TRUE,
            title = titleWYear), sanitize.text.function = function(x) x)
  })
  
  #download function when button is pressed
  output$downloadSummary <- downloadHandler(
    filename = function(){paste(input$yearSummary, ' Summary Report ',
                                          'ASA Historical Health Data Viewer ',
                                          Sys.Date(), '.html', sep = '')},

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "summaryReport.Rmd")
      file.copy("summaryReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(chapSum = summaryOut$chapSum, officerSum = summaryOut$officerSum, eduSum = summaryOut$eduSum)

      on.exit(unlink(tempReport), add = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ###                       Summary Output                        ###
  ###################################################################

}

# Run the app ----
shinyApp(ui = ui, server = server)
