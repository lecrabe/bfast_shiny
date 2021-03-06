## modified from https://github.com/rosca002/FAO_Bfast_workshop/tree/master/tutorial



the_dir <- paste0(data_dir,basename(the_dir),"/")
print(the_dir)

if(file.exists(paste0(the_dir,'/','stack.vrt'))){
  
  print(paste0('Running BFAST from: ', the_dir))
  
  output_directory <- paste0(the_dir,"/results/")
  
  dir.create(output_directory, recursive = T,showWarnings = F)
  
  dates <- unlist(read.csv(paste0(the_dir,'/','dates.csv'),header = FALSE))
  
  data_input_vrt <- paste0(the_dir,'/','stack.vrt')
  data_input     <- paste0(the_dir,'/','stack.tif')
  the_stack      <- brick(data_input_vrt) 
  
  results_directory <- file.path(output_directory,
                                 paste0("bfast_",title,'/'))
  
  dir.create(results_directory,recursive = T,showWarnings = F)
  
  log_filename <- file.path(results_directory, 
                            paste0(format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),"_bfast_", title, ".log"))
  start_time   <- format(Sys.time(), "%Y/%m/%d %H:%M:%S")
  
  result       <- file.path(results_directory, paste0("bfast_",title, ".tif"))
  print(result)
  outputfile <- paste0(results_directory,"bfast_",title,'_threshold.tif')
  
  if(!file.exists(result)){
    
    time <- system.time(bfmSpatial(the_stack, 
                                   start = c(monitoring_year_beg, 1),
                                   dates = dates,
                                   formula = response ~ harmon,
                                   order = order, 
                                   history = history,
                                   filename = result,
                                   mc.cores = detectCores()))
    
    write(paste0("This process started on ", start_time,
                 " and ended on ",format(Sys.time(),"%Y/%m/%d %H:%M:%S"),
                 " for a total time of ", time[[3]]/60," minutes"), log_filename, append=TRUE)
    
    ## Post-processing ####
    # calculate the mean, standard deviation, minimum and maximum of the magnitude band
    # reclass the image into 10 classes
    # 0 = no data
    # 1 = no change (mean +/- 1 standard deviation)
    # 2 = negative small magnitude change      (mean - 2 standard deviations)
    # 3 = negative medium magnitude change     (mean - 3 standard deviations)
    # 4 = negative large magnitude change      (mean - 4 standard deviations)
    # 5 = negative very large magnitude change (mean - 4+ standard deviations)
    # 6 = postive small magnitude change       (mean + 2 standard deviations)
    # 7 = postive medium magnitude change      (mean + 3 standard deviations)
    # 8 = postive large magnitude change       (mean + 4 standard deviations)
    # 9 = postive very large magnitude change  (mean + 4+ standard deviations)
    tryCatch({
      
      
      means_b2   <- cellStats( raster(result,band=2) , "mean") 
      mins_b2    <- cellStats( raster(result,band=2) , "min")
      maxs_b2    <- cellStats( raster(result,band=2) , "max")
      stdevs_b2  <- cellStats( raster(result,band=2) , "sd")
      
      system(sprintf("gdal_calc.py -A %s --A_band=2 --co=COMPRESS=LZW --type=Byte --overwrite --outfile=%s --calc=\"%s\"",
                     result,
                     paste0(results_directory,"tmp_bfast_",title,'_threshold.tif'),
                     paste0('(A<=',(maxs_b2),")*",
                            '(A>',(means_b2+(stdevs_b2*4)),")*9+",
                            '(A<=',(means_b2+(stdevs_b2*4)),")*",
                            '(A>',(means_b2+(stdevs_b2*3)),")*8+",
                            '(A<=',(means_b2+(stdevs_b2*3)),")*",
                            '(A>', (means_b2+(stdevs_b2*2)),")*7+",
                            '(A<=',(means_b2+(stdevs_b2*2)),")*",
                            '(A>', (means_b2+(stdevs_b2)),")*6+",
                            '(A<=',(means_b2+(stdevs_b2)),")*",
                            '(A>', (means_b2-(stdevs_b2)),")*1+",
                            '(A>=',(mins_b2),")*",
                            '(A<', (means_b2-(stdevs_b2*4)),")*5+",
                            '(A>=',(means_b2-(stdevs_b2*4)),")*",
                            '(A<', (means_b2-(stdevs_b2*3)),")*4+",
                            '(A>=',(means_b2-(stdevs_b2*3)),")*",
                            '(A<', (means_b2-(stdevs_b2*2)),")*3+",
                            '(A>=',(means_b2-(stdevs_b2*2)),")*",
                            '(A<', (means_b2-(stdevs_b2)),")*2")
                     
      ))
      
    }, error=function(e){})
    ####################  CREATE A PSEUDO COLOR TABLE
    cols <- col2rgb(c("white","beige","yellow","orange","red","darkred","palegreen","green2","forestgreen",'darkgreen'))
    pct <- data.frame(cbind(c(0:9),
                            cols[1,],
                            cols[2,],
                            cols[3,]
    ))
    
    write.table(pct,paste0(results_directory,"color_table.txt"),row.names = F,col.names = F,quote = F)
    
    
    ################################################################################
    ## Add pseudo color table to result
    system(sprintf("(echo %s) | oft-addpct.py %s %s",
                   paste0(results_directory,"color_table.txt"),
                   paste0(results_directory,"tmp_bfast_",title,'_threshold.tif'),
                   paste0(results_directory,"tmp_colortable.tif")
    ))
    ## Compress final result
    system(sprintf("gdal_translate -ot byte -co COMPRESS=LZW %s %s",
                   paste0(results_directory,"tmp_colortable.tif"),
                   outputfile
    ))
    ## Clean all
    system(sprintf(paste0("rm ",results_directory,"tmp*.tif")))
  }
}



