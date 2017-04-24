PHENmodel

################################################################################################################### create logical for winter by sunlight and precipitation
################################################################################################################### ##########################################################

create_logical_winter_summer_raster_stacks <- function(master_stack_dir_value, precipitation_threshold_value = c("mean", 
    "quantile_3"), model_value, out_dir_name_value) {
    
    # required libraries
    
    require("raster")
    
    # from user
    
    rasterpath <- master_stack_dir_value
    model <- model_value
    threshold <- precipitation_threshold_value
    out_path_common_dir <- out_dir_name_value
    
    master_directory <- getwd()
    
    # get list of files
    
    sunlight_file <- list.files(path = rasterpath, pattern = "^sunlight_.*.gri$", full.names = T, 
        ignore.case = T)
    
    if (model == "present") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_present_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    if (model == "2050_26_rpc") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_2050_26_rpc_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    if (model == "2050_45_rpc") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_2050_45_rpc_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    if (model == "2050_85_rpc") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_2050_85_rpc_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    if (model == "2070_26_rpc") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_2070_26_rpc_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    if (model == "2070_45_rpc") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_2070_45_rpc_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    if (model == "2070_85_rpc") {
        prec_file <- list.files(path = rasterpath, pattern = "^prec_2070_85_rpc_.*.gri$", full.names = T, 
            ignore.case = T)
    }
    
    
    # dir.create
    
    dir.create(out_path_common_dir, showWarnings = F)
    setwd(out_path_common_dir)
    
    # read rasters
    
    sunlight_stack <- stack(sunlight_file)/100  # needs to be divided by 100
    monthly_prec <- stack(prec_file)
    
    # raster extent
    
    raster_extend <- extent(sunlight_stack)
    
    # resolution name
    
    if (round(res(sunlight_stack)[1], 4) == 0.0083) {
        resolution_name = "30_sec"
    }
    if (round(res(sunlight_stack)[1], 4) == 0.0417) {
        resolution_name = "2_5_min"
    }
    if (round(res(sunlight_stack)[1], 4) == 0.0833) {
        resolution_name = "5_min"
    }
    if (round(res(sunlight_stack)[1], 4) == 0.1667) {
        resolution_name = "10_min"
    }
    
    ################# calculate by sunlight
    
    #### for stack 12 bands for sunlight
    
    sunlight_seq_year_list <- list()
    
    for (j in 1:12) {
        sun_bin <- as.matrix(sunlight_stack[[j]])
        (sun_bin[sun_bin < -998] <- NA)  #assign -999 to missing
        sunlight_seq_year_list[[j]] <- sun_bin
    }
    rm(j)
    
    #### for stack 12 bands for sunlight -- logical
    
    sunlight_matrix_logical <- list()  #create an empty list
    
    for (j in 1:12) {
        # 1st to 12th month
        temp <- sunlight_seq_year_list[[j]] >= 12  #more or equal to 12 hours of sunlight is consider summer
        sunlight_matrix_logical[[j]] <- temp
        rm(temp)
    }
    rm(j)
    
    # loop per month to create stack of logicals
    
    list_of_logicals_months <- list()
    
    for (jj in 1:12) {
        sunlight_logical_raster <- stack(raster(sunlight_matrix_logical[[jj]], xmn = raster_extend[1], 
            xmx = raster_extend[2], ymn = raster_extend[3], ymx = raster_extend[4], crs = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(sunlight_logical_raster) <- paste0("WinterSolarLogic_", jj)
        list_of_logicals_months[[jj]] <- sunlight_logical_raster
    }
    rm(jj)
    
    sunlight_logical_winter_stack <- stack(list_of_logicals_months)
    
    ################# calculate by precipitation divide months by highest to lowest precipitation to set winter or
    ################# summer ####
    
    if (threshold == "mean") {
        prec_raster_thres <- calc(monthly_prec, mean)
    }
    
    if (threshold == "quantile_3") {
        prec_raster_mean <- calc(monthly_prec, mean)
        prec_raster_sd <- calc(monthly_prec, sd)
        prec_raster_3Q <- prec_raster_mean + (0.675) * prec_raster_sd
        prec_raster_thres <- prec_raster_3Q
    }
    
    prec_mean_matrix <- as.matrix(prec_raster_thres)
    
    # to create logical PART 1
    
    prec_raster_matrix_1_list <- list()
    
    for (j in 1:12) {
        prec_bin <- as.matrix(monthly_prec[[j]])
        (prec_bin[prec_bin < -998] <- NA)
        prec_raster_matrix_1_list[[j]] <- prec_bin
    }
    rm(j)
    
    #### for stack 12 bands for winter -- logical PART 2
    
    prec_matrix_winter_logical <- list()  #create an empty list
    
    for (j in 1:12) {
        # 1st to 12th month
        temp <- prec_raster_matrix_1_list[[j]] >= prec_mean_matrix  #more or equal to 12 hours of sunlight is consider summer
        prec_matrix_winter_logical[[j]] <- temp
        rm(temp)
    }
    rm(j)
    
    # loop per month to create stack of logicals
    
    list_of_logicals_prec_months <- list()
    
    for (jj in 1:12) {
        prec_logical_raster <- stack(raster(prec_matrix_winter_logical[[jj]], xmn = raster_extend[1], 
            xmx = raster_extend[2], ymn = raster_extend[3], ymx = raster_extend[4], crs = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
        names(prec_logical_raster) <- paste0("WinterPrecLogic_", jj)
        list_of_logicals_prec_months[[jj]] <- prec_logical_raster
    }
    rm(jj)
    
    prec_logical_winter_stack <- stack(list_of_logicals_prec_months)
    
    # names to stack relative
    
    names(sunlight_logical_winter_stack) <- c("WinterSolarLogic_1", "WinterSolarLogic_2", "WinterSolarLogic_3", 
        "WinterSolarLogic_4", "WinterSolarLogic_5", "WinterSolarLogic_6", "WinterSolarLogic_7", 
        "WinterSolarLogic_8", "WinterSolarLogic_9", "WinterSolarLogic_10", "WinterSolarLogic_11", 
        "WinterSolarLogic_12")
    
    names(prec_logical_winter_stack) <- c("WinterPrecLogic_1", "WinterPrecLogic_2", "WinterPrecLogic_3", 
        "WinterPrecLogic_4", "WinterPrecLogic_5", "WinterPrecLogic_6", "WinterPrecLogic_7", "WinterPrecLogic_8", 
        "WinterPrecLogic_9", "WinterPrecLogic_10", "WinterPrecLogic_11", "WinterPrecLogic_12")
    
    # write.raster
    
    writeRaster(sunlight_logical_winter_stack, filename = paste("WinterSolarLogical_", model, 
        "_res_", resolution_name, sep = ""), datatype = "INT2S", overwrite = TRUE)
    writeRaster(prec_logical_winter_stack, filename = paste("WinterPrecLogical_", model, "_res_", 
        resolution_name, sep = ""), datatype = "INT2S", overwrite = TRUE)
    
    # output grid
    
    projection_raster_stack <- capture.output(crs(sunlight_logical_winter_stack))
    
    cat("------------- Raster file properties ------------\n")
    cat("grid names: ", names(sunlight_logical_winter_stack), "\n")
    cat("grid units: logical 1-cell is winter or 0-cell is summer \n")
    cat("resolution: ", resolution_name, "\n")
    cat("maximum values: ", maxValue(sunlight_logical_winter_stack), "\n")
    cat("minimum values: ", minValue(sunlight_logical_winter_stack), "\n")
    cat("file_name_cropped_written: ", paste("WinterSolarLogical_", model, "_res_", resolution_name, 
        sep = ""), "\n")
    cat(projection_raster_stack, "\n")
    cat("----------------------------------------\n")
    
    cat("------------- Raster file properties ------------\n")
    cat("grid names: ", names(prec_logical_winter_stack), "\n")
    cat("grid units: logical 1-cell is winter or 0-cell is summer \n")
    cat("resolution: ", resolution_name, "\n")
    cat("maximum values: ", maxValue(prec_logical_winter_stack), "\n")
    cat("minimum values: ", minValue(prec_logical_winter_stack), "\n")
    cat("file_name_cropped_written: ", paste("WinterPrecLogical_", model, "_res_", resolution_name, 
        sep = ""), "\n")
    cat(projection_raster_stack, "\n")
    cat("----------------------------------------\n")
    
    ## 
    setwd(master_directory)
    
    return(list(sunlight_logical_winter_stack, prec_logical_winter_stack))
    
}
