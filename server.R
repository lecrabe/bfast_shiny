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
  
  ################################# Display the file path
  data_dir <- reactive({
    req(input$time_series_dir)
    validate(need(input$time_series_dir, "Missing input: Please select time series folder"))
    df <- parseDirPath(volumes, input$time_series_dir)
    
  })
  
  ################################# Output directory path
  outdir <- reactive({
    req(input$time_series_dir)
    dir <- parseDirPath(volumes, input$time_series_dir)
    subDir <- paste0(dir,"/","bfast_results")
    dir.create(subDir)
    subDir
    })
  
  ################################# Display output directory path
  output$outdirpath = renderPrint({
    outdir()
  })
  
  list_year <- reactive({
    req(data_dir())
    data_dir <- data_dir()
    list <- list.files(data_dir,pattern = "_stack.tif",recursive = T)
    
    unlist(lapply(list,function(x){unlist(strsplit(x,split = "_"))[length(unlist(strsplit(x,split = "_")))-1]}))
    })
  
  beg_year <- reactive({
    req(list_year())
    min(list_year())
  })
  
  end_year <- reactive({
    req(list_year())
    max(list_year())
  })

  
  
  
  ##################################################################################################################################
  ############### Option buttons
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
    
    #validate(need(input$bfastStartButton,"Click "))
    print(" my reactives")
    data_dir            <- paste0(data_dir(),"/")
    output_directory    <- paste0(outdir(),"/")
    
    print(data_dir)
    print(output_directory)
    
    historical_year_beg <- as.numeric(beg_year())
    monitoring_year_end <- as.numeric(end_year())
    
    monitoring_year_beg <- as.numeric(input$option_m_beg)
    order               <- as.numeric(input$option_order)
    history             <- as.character(input$option_history)
    print(order)
    print(history)
    
    withProgress(message = 'BFAST running',
                 value = 0,
                 {
                   setProgress(value = .1)
                   source("www/scripts/bfast_run.R",echo=T,local=T)
                 })
    raster(outputfile)
  })
  
  ##################################################################################################################################
  ############### Processing time as reactive
  process_time <- reactive({
    req(bfast_res())
    data_dir            <- paste0(data_dir(),"/")
    output_directory    <- paste0(outdir(),"/")
    
    print(data_dir)
    print(output_directory)
    
    historical_year_beg <- as.numeric(beg_year())
    monitoring_year_end <- as.numeric(end_year())
    
    monitoring_year_beg <- as.numeric(input$option_m_beg)
    order               <- as.numeric(input$option_order)
    history             <- as.character(input$option_history)
    title <- paste0("o_",order,"_h_",paste0(history,collapse = "-"))
    
    results_directory <- file.path(outdir(),"/",paste0("bfast_",title,'/'))
    
    log_filename <- list.files(results_directory,pattern="log")[1]
    readLines(paste0(results_directory,log_filename))
    
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
   print(process_time())
 })
  
  ##################################################################################################################################
  ############### Enable to download the CE file (csv)
  output$ui_download_csv <- renderUI({
    req(input$time_series_dir)
    downloadButton('download_csv',
                   label = textOutput('download_csv_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file (csv)
  output$download_csv <- downloadHandler(
    filename = function() {
      "zonal_stats.csv"
    },
    content  = function(xx) {
      to_export <- zonal()
      write.csv(to_export, xx, row.names = FALSE)
    }
  )
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
