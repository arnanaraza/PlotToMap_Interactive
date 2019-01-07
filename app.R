options(shiny.maxRequestSize = 30*1024^2)

# packages
library(shiny)
library(DT)
library(rgdal)
library(raster)
library(plyr)
library(dplyr)
library(foreach)
library(parallel)
library(doParallel)
library(plotrix)

# global variables
dataDir <- "M:/BiomassCCI/Shiny_ROG/data"
scriptsDir <- "M:/BiomassCCI/Shiny_ROG/"
mainDir <- "M:/BiomassCCI_2019"
outDir <- "M:/BiomassCCI/Shiny_ROG/temp"
agbTilesDir <- "M:/BiomassCCI/data/agb"
treeCoverDir <- '//GRS_NAS_01/GRSData/global_products/Hansen/treecover_2010/treecover2010_v3' # make sure of folder access
SRS <- CRS("+init=epsg:4326")
forestTHs <- 10 # for

# functions
setwd(scriptsDir) 
source('Overview.R')
source('TempFixed.R')
source('TempEffect.R')
source('MakeBlockPolygon.R')
source('TileNames.R')
source('BlockMeans.R')
source('invDasymetry.R')
source('OnePlot_450.R')


#ui
ui <- fluidPage(
  #title
  titlePanel(p("Validation app", style = "color:#3474A7")),
  
  #sides
  sidebarLayout(
    sidebarPanel(width=2,
      fileInput(inputId = "filedata", label = "Upload data. Choose csv file",
                accept = c(".csv")),
      
      # scale options
      selectInput(inputId = "scale", label = "Plot AGB is intercontinental?",
                  choices = c("local", "global")),
      
      
          # Only show this panel if plot data is intercontinental
          conditionalPanel(
            condition = "input.scale == 'global'",
            selectInput(inputId = "global", label = "What scale?",
                        c('country', 'biome')),# multiple = TRUE),
            verbatimTextOutput("global")
          ),
          
          # If intercontinental, chose between continents and biomes
          conditionalPanel(
            condition = "input.scale == 'global'",
            selectInput(inputId = "subglobal", label = "What sub-scale?", ""), #will be updated after plot upload
                       
            verbatimTextOutput("subglobal")
          ),
        
      
      # plot overview
      conditionalPanel(
        condition = "input.scale == 'global'",
        selectInput(inputId = "overview", label = "Overview field",
                    choices = c("fez", "year")),
        verbatimTextOutput("overview")
      ),
      
      # see temporal fix effect 
      radioButtons(inputId = "temporal", label = "Apply Temporal fix?",
                  choices = c("yes", "no"),selected = character(0)),
      radioButtons(inputId = "fraction", label = "Apply Forest Fraction?",
                   choices = c("yes", "no"),selected = character(0)),

      # aggregation level
      radioButtons(inputId = "aggregation", label = "Apply Aggregation?",
                   choices = c("yes", "no"),selected = character(0)),
      
          # Only show this panel if to apply aggregation
          conditionalPanel(
            condition = "input.aggregation == 'yes'",
            numericInput("obs1", "Aggregation:", 0.1, min = 0.001, max = 1),
            verbatimTextOutput("agg")
          )
    ),
    
  #body
  mainPanel(
      
    tabsetPanel(
      #overview
      tabPanel("Overview",
               plotOutput("graph"),
               DTOutput("table")),
      
      #add tab for temporal fix
      tabPanel("Temporal fix",
                  plotOutput("hist"),
                  DTOutput("table1")),
      
      #add tab for aggregation and forest fraction
      tabPanel("Main",
               plotOutput("valid1")
            #   DTOutput("accuracy1"),
             #  plotOutput("valid2"),
              # DTOutput("accuracy2"),
               #plotOutput("valid3"),
               #DTOutput("accuracy3")
               )
      )
    )
  )
)

# server()
server <- function(input, output, session) {
  #make table reactive
  data <- reactive({
    req(input$filedata)
    df <-read.csv(input$filedata$datapath)
    
    #update subscale inputs
    # retrieve zoning groups
    continent <- unique(na.omit(df$ZONE))
    biome <- unique(na.omit(df$GEZ))  
    
    if (input$global == 'country'){
    updateSelectInput(session, inputId = 'subglobal', label = "What sub-scale?",
                      choices = continent, selected = "")}
    else{
      updateSelectInput(session, inputId = 'subglobal', label = "What sub-scale?",
                        choices = biome, selected = "")
    }
    return(df)
    
    })
  
  #loads table
  output$table <- renderDT(data())
  
  #plots graph reactive to selected input
  output$graph <- renderPlot(
    if(input$overview == 'fez'){
      FezPie(data())}
    else{
      YearPie(data())}
    )
  
  # TEMPORAL FIX -----------------------------------------------------------------------
  

  # creates histogram and change table of pre and post temporal fix
  output$hist <- renderPlot({
    # apply growth data to whole plot data by identifying AGB map year
    plots <- data() #should be inside!
    
    gez <- sort(as.vector((unique(plots$GEZ)))) #gets unique eco-zones without NAs
    plotsNew <- ldply(lapply (1:length(gez), function(x) 
      TempFixed(plots, gez[[x]], 2010)), data.frame) #2010 = GlobBiomass year
    
    # add plots with 1-2 NA from the "uniques" (it's just 0.06% of the total dataset)
    plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))
    
    HistoShift(plots, plotsNew)
    
  })
  output$table1 <- renderDT({
    # apply growth data to whole plot data by identifying AGB map year
    plots <- data() #should be inside!
    
    gez <- sort(as.vector((unique(plots$GEZ)))) #gets unique eco-zones without NAs
    plotsNew <- ldply(lapply (1:length(gez), function(x) 
      TempFixed(plots, gez[[x]], 2010)), data.frame) #2010 = GlobBiomass year
    
    # add plots with 1-2 NA from the "uniques" (it's just 0.06% of the total dataset)
    plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))
    
    ChangeTable(plots, plotsNew)
    
    # export new AGB data according to date generated
   # write.csv(plotsNew, paste0('GlobBiomass_validation_data_600_TempFixed_',Sys.Date(),'.csv'), row.names=FALSE)
    
  })
  
  # INVDASYMETRY -----------------------------------------------------------------------
  
  # aggregation
#  output$agg <- reactive({input$obs1})
  
  # TF only ---------------------------------------
  output$valid1 <- renderPlot({
    if(input$temporal == 'yes'){
      plots <- data() 
      gez <- sort(as.vector((unique(plots$GEZ))))
      plotsNew <- ldply(lapply (1:length(gez), function(x) 
        TempFixed(plots, gez[[x]], 2010)), data.frame) 
      
      plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))
      
    }else{
      plotsNew <- data()
    }
    
    #Invdasymetry

    if(input$scale == 'global' & input$global == 'country'){
      AGBdata <- invDasymetry(plotsNew, "ZONE",  input$subglobal, input$obs1, 5)
    }
    else{
      AGBdata <- invDasymetry(plotsNew, "GEZ", input$subglobal, input$obs1, 5)}
    
    #one plot
    OnePlot(AGBdata$plotAGB_10, AGBdata$mapAGB,'Sample Plots')
    
  })
  
  
  # TF + Forest Scaling ----------------------------
  #output$valid2 <- renderPlot({
#    if(input$temporal == 'yes' && input$fraction == 'yes'){
 #   }#two plots
    
    
    
  #})
  # TF + Forest Scaling + Aggregation
  #output$valid3 <- renderPlot({
   # if(input$agg == 'yes' && input$fraction == 'yes'){
    #}#three plots
    
    
    
#  })
  
}

shinyApp(ui, server)
