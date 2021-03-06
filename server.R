####################################################################################
####### BFAST
####### SEPAL shiny application
####### FAO Open Foris SEPAL project
####### remi.dannunzio@fao.org - yelena.finegold@fao.org
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or
# software or in the documentation accompanying it, for program maintenance and
# upgrading as well as for any # damage that may arise from them. FAO also declines
# any responsibility for updating the data and assumes no responsibility for errors
# and omissions in the data provided. Users are, however, kindly asked to report any
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2018/01/18
## bfast / server
####################################################################################


####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "Français") {
      source("text_french.R", local = TRUE, encoding = "UTF-8")
      #print("fr")
    }
    if (input$language == "Español") {
      source("text_spanish.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("sp")
    }
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store filepath    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  
  volumes <- c('Home' = Sys.getenv("HOME"),
               volumes)
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  
  ##################################################################################################################################
  ## Allow to download test data
  output$dynUI_download_test <- renderPrint({
    req(input$download_test_button)
    
    dir.create(file.path("~", "bfast_data_test"),showWarnings = F)
    
    withProgress(message = paste0('Downloading data in ', dirname("~/bfast_data_test/")),
                 value = 0,
                 {
                   system("wget -O ~/bfast_data_test/bfast_data_test.zip  https://github.com/openforis/data_test/raw/master/bfast_data_test.zip")
                   system("unzip -o ~/bfast_data_test/bfast_data_test.zip  -d ~/bfast_data_test/ ")
                   system("rm ~/bfast_data_test/bfast_data_test.zip")
                 })
    
    list.files("~/bfast_data_test/")
  })
  
  ##################################################################################################################################
  ############### Select input file (raster OR vector)
  shinyDirChoose(
    input,
    'time_series_dir',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ################################# Data file path
  data_dir <- reactive({
    req(input$time_series_dir)
    validate(need(input$time_series_dir, "Missing input: Please select time series folder"))
    df <- parseDirPath(volumes, input$time_series_dir)
    
  })
  
  ################################# Display tiles inside the DATA_DIR
  output$outdirpath = renderPrint({
    list.files(data_dir())
  })
  
  ################################# Setup from the archives the Date Range
  list_year <- reactive({
    req(data_dir())
    data_dir <- data_dir()
    list <- list.files(data_dir,pattern = "_stack.tif",recursive = T)
    
    unlist(lapply(list,function(x){unlist(strsplit(x,split = "_"))[length(unlist(strsplit(x,split = "_")))-1]}))
    })
  
  ################################# Take the minimum as beginning Date
  beg_year <- reactive({
    req(list_year())
    min(list_year())
  })
  
  ################################# Take the maximum as ending Date
  end_year <- reactive({
    req(list_year())
    max(list_year())
  })

  
  ##################################################################################################################################
  ############### Option buttons --> KEEP IF ARCHIVE READING IS NOT OPTIMAL
  # output$ui_option_h_beg <- renderUI({
  #   req(input$time_series_dir)
  #   selectInput(inputId = 'option_h_beg',
  #               label = "Historical year beginning",
  #               choices = 2000:2020,
  #               selected = as.numeric(beg_year())
  #               )
  # })
  # 
  # output$ui_option_m_end <- renderUI({
  #   req(input$time_series_dir)
  #   selectInput(inputId = 'option_m_end',
  #               label = "Monitoring year end",
  #               choices = as.numeric(input$option_h_beg):2020,
  #               selected = as.numeric(end_year())
  #   )
  # })
  
  ################################# Take the average date for the beginning of monitoring period
  output$ui_option_m_beg <- renderUI({
    req(input$time_series_dir)
    
    sliderInput(inputId = 'option_m_beg',
                label = textOutput("text_option_date_break"),
                min = as.numeric(beg_year()),
                max = as.numeric(end_year()),
                value = (as.numeric(beg_year()) + as.numeric(end_year()))/2
    )
  })
  
 
  output$ui_option_order <- renderUI({
      req(input$time_series_dir)
      selectInput(inputId = 'option_order',
                  label = "Order parameter in BFAST",
                  choices = 1:5,
                  selected = 3
      )
    })
  
  output$ui_option_history <- renderUI({
    req(input$time_series_dir)
    selectInput(inputId = 'option_history',
                label = "History parameter in BFAST",
                choices = c("ROC", "BP", "all",as.numeric(beg_year())),
                selected = "ROC"
    )
  })
  
  
  ##################################################################################################################################
  ############### Insert the start button
  output$StartButton <- renderUI({
    req(input$time_series_dir)
    actionButton('bfastStartButton', textOutput('start_button'))
  })
  
  ##################################################################################################################################
  ############### Run BFAST
  bfast_res <- reactive({
    req(input$time_series_dir)
    req(input$bfastStartButton)
    
    data_dir            <- paste0(data_dir(),"/")
    print(data_dir)
    
    historical_year_beg <- as.numeric(beg_year())
    monitoring_year_end <- as.numeric(end_year())
    
    monitoring_year_beg <- as.numeric(input$option_m_beg)
    order               <- as.numeric(input$option_order)
    history             <- as.character(input$option_history)
    
    print(order)
    print(history)
    
    title <- paste0("o_",order,"_h_",paste0(history,collapse = "-"))
    
    for(the_dir in list.dirs(data_dir, recursive=FALSE)){
      
    withProgress(message = paste0('BFAST running for ',the_dir),
                 value = 0,
                 {
                   setProgress(value = .1)
                   source("www/scripts/bfast_run.R",echo=T,local=T)
                 })
    }
    
    #############################################################
    ### MERGE AS VRT
    system(sprintf("gdalbuildvrt %s %s",
                   paste0(data_dir,"/bfast_",title,"_threshold.vrt"),
                   paste0(data_dir,"/*/results/bfast_",title,"/bfast_",title,"_threshold.tif")
    ))
    print(paste0(data_dir,"/bfast_",title,"_threshold.vrt"))
    
    raster(paste0(data_dir,"/bfast_",title,"_threshold.vrt"))
    
  })
  
  ##################################################################################################################################
  ############### Processing time as reactive
  process_time <- reactive({
    req(bfast_res())
    log_filename <- list.files(data_dir(),pattern="log",recursive = T)[1]
    print(paste0(data_dir(),"/",log_filename))
    readLines(paste0(data_dir(),"/",log_filename))
    })

  ############### Display the results as map
  output$display_res <- renderPlot({
    req(bfast_res())
    print('Check: Display the map')
    plot(bfast_res(), axes = FALSE)
    })
  
  ##################################################################################################################################
  ############### Display time
 output$message <- renderText({
   req(bfast_res())
   print("processing time")
   process_time()
 })
  

  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
