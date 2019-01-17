rm(list=ls())
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
library(Metrics)
library(data.table)
library(shinyFiles)

# global variables
dataDir <- "M:/BiomassCCI/Shiny/data"
scriptsDir <- "M:/BiomassCCI/Shiny/R"
mainDir <- "M:/BiomassCCI_2019"
outDir <- "M:/BiomassCCI/Shiny/results"
resultsDir <- "M:/BiomassCCI/Shiny/results/S.America"

#agbTilesDir <- "M:/BiomassCCI/data/agb"
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
source('OnePlot.R')
source('Accuracy.R')
source('CreateDir.R')
source('ThreePlots.R')
source('TwoPlots.R')

#ui
ui <- fluidPage(
  #title
  titlePanel(p("Validation app", style = "color:#3474A7")),
  
  #sides
  sidebarLayout(
    sidebarPanel(width=2,
      fileInput(inputId = "filedata", label = "Upload plot data (csv)",
                accept = c(".csv")),
  
      
      # scale options
      selectInput(inputId = "scale", label = "Plot AGB is intercontinental?", selected=NULL,
                  choices = c("local", "global")),
      
      
          # Only show this panel if plot data is intercontinental
          conditionalPanel(
            condition = "input.scale == 'global'",
            selectInput(inputId = "global", label = "What scale?",
                        c('country', 'biome')),# multiple = TRUE),
            verbatimTextOutput("global")
          ),
      
          # plot overview
          conditionalPanel(
            condition = "input.scale == 'global'",
            selectInput(inputId = "overview", label = "Overview field",
                        choices = c("fez", "year")),
            verbatimTextOutput("overview")
          ),
              
          # If intercontinental, chose between continents and biomes
          conditionalPanel(
            condition = "input.scale == 'global'",
            selectInput(inputId = "subglobal", label = "What sub-scale?", ""), #will be updated after plot upload
                       
            verbatimTextOutput("subglobal")
          ),
        
      
      # agb directory
      fluidRow(
               column(6,div(style = "height:50px;width:170px;background-color: green;",
                            sidebarPanel(
                 shinyDirButton("dir", "Map AGB directory", "Upload"))))),
      
      # see temporal fix effect 
      radioButtons(inputId = "temporal", label = "Apply Temporal fix?",
                  choices = c("yes", "no"),selected = character(0)),
      
      # aggregation level
      radioButtons(inputId = "aggregation", label = "Apply Aggregation?",
                   choices = c("yes", "no"),selected = character(0)),
      
          # Only show this panel if to apply aggregation
          conditionalPanel(
            condition = "input.aggregation == 'yes'",
            numericInput("obs1", "Aggregation:", 0.1, min = 0.001, max = 1),
            verbatimTextOutput("agg")
          ),
      
      # run another output
      radioButtons(inputId = "comparison", label = "show effects?",
                   choices = c("yes", "no"),selected = character(0))
      
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
               plotOutput("valid1"),
               DTOutput("accuracy1"),
               plotOutput("valid2"),
                DTOutput("accuracy2")
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
    if (input$scale == 'global'){
      if(input$overview == 'fez'){
        FezPie(data())}
      else{
        YearPie(data())}
      
    }else{print('overview is for global datasets only')}

    )
  
  volumes <- getVolumes()
  shinyDirChoose(input, 'dir', roots=volumes, session=session)
  path <- reactive({
    return(parseDirPath(volumes, input$dir))
  })
  
  # TEMPORAL FIX -----------------------------------------------------------------------
  

  # creates histogram and change table of pre and post temporal fix
  output$hist <- renderPlot({
    req(input$temporal)
    if(input$temporal == 'yes'){
      
    # apply growth data to whole plot data by identifying AGB map year
    plots <- data() #should be inside!
    
    gez <- sort(as.vector((unique(plots$GEZ)))) #gets unique eco-zones without NAs
    plotsNew <- ldply(lapply (1:length(gez), function(x) 
      TempFixed(plots, gez[[x]], 2010)), data.frame) #2010 = GlobBiomass year
    
    # add plots with 1-2 NA from the "uniques" (it's just 0.06% of the total dataset)
    plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))
    
    HistoShift(plots, plotsNew)
    } else {print('no temporal adjustment set')}
  })
  output$table1 <- renderDT({
    req(input$temporal)
    
    if(input$temporal == 'yes'){
    
    # apply growth data to whole plot data by identifying AGB map year
    plots <- data() #should be inside!
    
    gez <- sort(as.vector((unique(plots$GEZ)))) #gets unique eco-zones without NAs
    plotsNew <- ldply(lapply (1:length(gez), function(x) 
      TempFixed(plots, gez[[x]], 2010)), data.frame) #2010 = GlobBiomass year
    
    # add plots with 1-2 NA from the "uniques" (it's just 0.06% of the total dataset)
    plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))
    
    ct <- ChangeTable(plots, plotsNew)
    DT::datatable(ct, options = list(dom = 't'))
    } else {print('no temporal adjustment set')}
    # export new AGB data according to date generated
    # write.csv(plotsNew, paste0('GlobBiomass_validation_data_600_TempFixed_',Sys.Date(),'.csv'), row.names=FALSE)
    
  })
  
  # INVDASYMETRY -----------------------------------------------------------------------
  
  # TF only ---------------------------------------
  output$valid1 <- renderPlot({
    req(input$temporal)
    req(input$scale)
    req(input$global)
    req(input$aggregation)
    
    agbTilesDir <<- path()
    
    if(input$temporal == 'yes'){
      plots <- data() 
      gez <- sort(as.vector((unique(plots$GEZ))))
      plotsNew <- ldply(lapply (1:length(gez), function(x) 
        TempFixed(plots, gez[[x]], 2010)), data.frame) 
      
      plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))
      
    }else{
      plotsNew <- data()
    }
    
    #Invdasymetry-------------------------------------
    
    #COUNTRY
    if(input$scale == 'global' & input$global == 'country'){
      
      #yes TF, yes agg
      if (input$aggregation == 'yes' & input$temporal == 'yes'){
        AGBdata <- invDasymetry(plotsNew, "ZONE",  input$subglobal, input$obs1, 5)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("agg01_", input$subglobal, ".Rdata")))
      }
      #no TF, yes agg
      if (input$aggregation == 'yes' & input$temporal == 'no'){
        AGBdata <- invDasymetry(plotsNew, "ZONE",  input$subglobal, input$obs1, 5)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("agg01x_", input$subglobal, ".Rdata")))
      } 
      #yes TF, no agg
      if (input$aggregation == 'no' & input$temporal == 'yes'){
        AGBdata <- invDasymetry(plotsNew, "ZONE", input$subglobal, wghts = T)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("InvDasyPlotx_", input$subglobal, ".Rdata")))
      } 
      # no processing at all
      if (input$aggregation == 'no' & input$temporal == 'no'){
        AGBdata <- invDasymetry(plotsNew, "ZONE", input$subglobal, wghts = T)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("InvDasyPlot_", input$subglobal, ".Rdata")))
      }
    }
  
    #BIOMES
    if(input$scale == 'global' & input$global == 'biome'){
      #conditions---------
      if (input$aggregation == 'yes' & input$temporal == 'yes'){
        AGBdata <- invDasymetry(plotsNew, "GEZ",  input$subglobal, input$obs1, 5)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("agg01_", input$subglobal, ".Rdata")))
      }
      #no TF, yes aggregation
      if (input$aggregation == 'yes' & input$temporal == 'no'){
        AGBdata <- invDasymetry(plotsNew, "GEZ",  input$subglobal, input$obs1, 5)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("agg01x_", input$subglobal, ".Rdata")))
      } 
      #yes TF, no aggregation
      if (input$aggregation == 'no' & input$temporal == 'yes'){
        AGBdata <- invDasymetry(plotsNew, "GEZ", input$subglobal, wghts = T)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir, 
                                       paste0("InvDasyPlotx_", input$subglobal, ".Rdata")))
      } 
      # if there's no processing at all
      if (input$aggregation == 'no' & input$temporal == 'no'){
        AGBdata <- invDasymetry(plotsNew, "GEZ", input$subglobal, wghts = T)
        CreateDir(outDir, input$subglobal)
        resultsDir <- paste0(outDir,'/', input$subglobal)
        save(AGBdata, file = file.path(resultsDir,  
                                       paste0("InvDasyPlot_", input$subglobal, ".Rdata")))
      }
    }
    
    #one plot
    OnePlot(AGBdata$plotAGB_10, AGBdata$mapAGB, input$subglobal)
    resultsDir <<- resultsDir #make it a global variable
  })
  
  # add accuracy table
  output$accuracy1 <- renderDT({
    
    #COUNTRY
    if(input$scale == 'global' & input$global == 'country'){
      
      #yes TF, yes agg (ideal)
      if (input$aggregation == 'yes' & input$temporal == 'yes'){
        Rdata <- list.files(resultsDir, pattern='agg01_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'agg_tf')
      }
      #no TF, yes agg
      if (input$aggregation == 'yes' & input$temporal == 'no'){
        Rdata <- list.files(resultsDir, pattern='agg01x_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'agg')
      } 
      #yes TF, no agg
      if (input$aggregation == 'no' & input$temporal == 'yes'){
        Rdata <- list.files(resultsDir, pattern='InvDasyPlotx_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'tf')
      } 
      # no processing at all
      if (input$aggregation == 'no' & input$temporal == 'no'){
        Rdata <- list.files(resultsDir, pattern='InvDasyPlot_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'none')
      }
      DT::datatable(acc, options = list(dom = 't'))
      
    }
    
    
    #BIOME
    if(input$scale == 'global' & input$global == 'biome'){
      
      #yes TF, yes agg
      if (input$aggregation == 'yes' & input$temporal == 'yes'){
        Rdata <- list.files(resultsDir, pattern='agg01_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'agg_tf')
      }
      #no TF, yes agg
      if (input$aggregation == 'yes' & input$temporal == 'no'){
        Rdata <- list.files(resultsDir, pattern='agg01x_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'agg')
      } 
      #yes TF, no agg
      if (input$aggregation == 'no' & input$temporal == 'yes'){
        Rdata <- list.files(resultsDir, pattern='InvDasyPlotx_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'tf')
      } 
      # no processing at all
      if (input$aggregation == 'no' & input$temporal == 'no'){
        Rdata <- list.files(resultsDir, pattern='InvDasyPlot_')
        AGBdata <-get(load(paste0(resultsDir, '/', Rdata)))
        acc <- Accuracy(AGBdata, 6, 'none')
      }
      
      
      DT::datatable(acc, options = list(dom = 't'))
      
    }
    DT::datatable(acc, options = list(dom = 't'))
    
  })
    
  # > 1 plots  ------------------------------------------------------------------
    output$valid2 <- renderPlot({
      if (input$comparison == 'yes'){
        # if baseline exist...
        RdBase <- list.files(resultsDir, pattern='InvDasyPlot_')
        if(length(RdBase) == 1){
          
          #get data
          RdTemp <- list.files(resultsDir, pattern='InvDasyPlotx_')
          RdAgg <- list.files(resultsDir, pattern='agg01x_')
          RdBoth <- list.files(resultsDir, pattern='agg01_')
          
          #both TF and Agg effects
          if(length(RdBoth) == 1 & length(RdTemp) == 1){
            #load only when file exist
            AGBBase <-get(load(paste0(resultsDir, '/', RdBase)))
            AGBTemp <-get(load(paste0(resultsDir, '/', RdTemp)))
            AGBBoth <-get(load(paste0(resultsDir, '/', RdBoth)))
            
            #three plots
            ThreePlots(AGBBase$orgPlotAGB, AGBBase$mapAGB, AGBTemp$plotAGB_10,AGBTemp$mapAGB, 
                       AGBBoth$plotAGB_10,AGBBoth$mapAGB, input$subglobal,
                       fname=file.path(resultsDir, paste0("EffectsBoth_",Sys.Date(),".png")), title='comp')
          }
          #TF effect, no Agg
          if(length(RdTemp) == 1){
            #load only when file exist
            AGBBase <-get(load(paste0(resultsDir, '/', RdBase)))
            AGBTemp <-get(load(paste0(resultsDir, '/', RdTemp)))
            
            
            #two plots
            TwoPlots(AGBBase$plotAGB_10, AGBBase$mapAGB, AGBTemp$plotAGB_10,AGBTemp$mapAGB, 
                     input$subglobal, fname=file.path(resultsDir, paste0("EffectsTF_",Sys.Date(),".png")), title='tf')
          }
          #Agg effect, no TF
          if(length(RdAgg) == 1){
            #load only when file exist
            AGBBase <-get(load(paste0(resultsDir, '/', RdBase)))
            AGBAgg <-get(load(paste0(resultsDir, '/', RdAgg)))
            
            #two plots
            TwoPlots(AGBBase$plotAGB_10, AGBBase$mapAGB, AGBAgg$plotAGB_10,AGBAgg$mapAGB, 
                     input$subglobal, fname=file.path(resultsDir, paste0("EffectsAgg_",Sys.Date(),".png")), title='agg')
          }
          
          
        }
        
        else{print('run baseline first (no TF - no Aggregation)')}
        
      } else{print('run validation first')}
      
        
      })
    
    output$accuracy2 <- renderDT({
      if (input$comparison == 'yes'){
        # if baseline exist...
        RdBase <- list.files(resultsDir, pattern='InvDasyPlot_')
        
        if(length(RdBase) == 1){
          
          #get data
          RdTemp <- list.files(resultsDir, pattern='InvDasyPlotx_')
          RdAgg <- list.files(resultsDir, pattern='agg01x_')
          RdBoth <- list.files(resultsDir, pattern='agg01_')
          
          #TF effect, no Agg
          if(length(RdTemp) == 1 & length(RdAgg) == 0){

            #load only when file exist
            AGBBase <-get(load(paste0(resultsDir, '/', RdBase)))
            AGBTemp <-get(load(paste0(resultsDir, '/', RdTemp)))
            
            #metrics per bin
            pre <- Accuracy(AGBBase, 6) #data, number of bins
            post <- Accuracy(AGBTemp, 6)
            
            #combine pre and post
            acc0 <- cbind(post[1], post[2], pre[3], post[c(3,4)],pre[5], post[5],
                          pre[6], post[6],pre[7], post[7])
            ###gives the ff: bins   plots_count  plot_pre    plot_post     rmse pre post     rrmse pre post
            
            #add bias column
            acc1 <- cbind (acc0, old.bias = acc0[5] - acc0[3])
            acc1 <- cbind (acc1, new.bias = acc1[5] - acc1[4])
            #rename and arrange data frames
            
            acc2 <- acc1 [,c(1,12,13)]
            names(acc2) <- c('bins', 'bias_pre_TF', 'bias_post_TF')

            DT::datatable(acc2, options = list(dom = 't'))
            
          }
          
          #Agg effect, no TF
          if(length(RdAgg) == 1 & length(RdTemp) == 0){

            #load only when file exist
            AGBBase <-get(load(paste0(resultsDir, '/', RdBase)))
            AGBAgg <-get(load(paste0(resultsDir, '/', RdAgg)))
            
            #metrics per bin
            pre <- Accuracy(AGBBase, 6) #data, number of bins
            post <- Accuracy(AGBAgg, 6)

            #combine pre and post
            acc0 <- cbind(post[1], post[2], pre[3], post[c(3,4)],pre[5], post[5],
                          pre[6], post[6],pre[7], post[7])
            ###gives the ff: bins   plots_count  plot_pre    plot_post     rmse pre post     rrmse pre post
            
            #add bias column
            acc1 <- cbind (acc0, old.bias = acc0[5] - acc0[3])
            acc1 <- cbind (acc1, new.bias = acc1[5] - acc1[4])
            
            #rename and arrange data frames
            acc2 <- acc1 [,c(1,12,13)]
            names(acc2) <- c('bins', 'bias_pre_Agg', 'bias_post_Agg')
            
          }
          
          #both TF and Agg effects
          if(length(RdBoth) == 1 & length(RdTemp) == 1){
            #load only when file exist
            AGBBase <-get(load(paste0(resultsDir, '/', RdBase)))
            AGBAgg <-get(load(paste0(resultsDir, '/', RdAgg)))
            AGBTemp <-get(load(paste0(resultsDir, '/', RdTemp)))
            
            #metrics per bin
            pre <- Accuracy(AGBBase, 6) #data, number of bins
            post1 <- Accuracy(AGBTemp, 6)
            post2 <- Accuracy(AGBAgg, 6)
            
            #combine pre and post
            acc0 <- cbind(post[1], post1[2], pre[3], post1[3], post2[c(3,4)], pre[5], post1[5], post2[5],
                          pre[6], post1[6], post2[6],pre[7], post1[7],post2[7])
            #add bias column
            acc1 <- cbind (acc0, old.bias = round(acc0[6] - acc0[3],2))
            acc1 <- cbind (acc1, new.bias1 = round(acc1[6] - acc1[4],2))
            acc1 <- cbind (acc1, new.bias2 = round(acc1[6] - acc1[5],2))

            acc2 <- acc1[,c(1,16,17,18)] #get last 3 columns (biases)

            #rename and arrange data frames
            cols <- c('bins', 'bias_pre', 'bias_post_TF', 'bias_post_Agg')
            names(acc2) <- cols
           
           # DT::datatable(acc1, options = list(dom = 't'))
            
          }
        DT::datatable(acc2, options = list(dom = 't'))
          
        }else{print('run baseline first (no TF - no Aggregation)')}
        
        
      }else{print('run validation first')}
    })
    

}

shinyApp(ui, server)
