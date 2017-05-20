# Occurpoints


setwd("~/Desktop/Luisa_data/aa_cleanpoints")

rvertnet_download_for_cleanpoints <- function(genus_value, species_value, max_number_of_records_value, 
    country_value = NULL, province_state_value = NULL, only_with_lat_log_logical, out_dir_value = NULL) {
    
    require(rvertnet)
    
    # input from user
    
    genus_name <- genus_value
    species_name <- species_value
    max_number <- max_number_of_records_value
    only_with_lat_log <- only_with_lat_log_logical
    country_name <- country_value
    province_state_name <- province_state_value
    
    out_dir <- out_dir_value
    
    master_directory <- getwd()
    
    ## only by species and genus
    
    term_tibble_raw <- searchbyterm(specificepithet = species_name, genus = genus_name, cntry = country_name, 
        stateprovince = province_state_value, mappable = only_with_lat_log, limit = max_number)
    
    ## post process after rvernet
    
    term_tibble_data <- term_tibble_raw$data
    tibble_data_df_raw <- data.frame(genus = term_tibble_data$genus, species = term_tibble_data$specificepithet, 
        Lon = term_tibble_data$decimallongitude, Lat = term_tibble_data$decimallatitude, stringsAsFactors = FALSE)
    tibble_data_df_1 <- tibble_data_df_raw[!duplicated(tibble_data_df_raw), ]
    tibble_data_df_1$genus_species <- paste0(tibble_data_df_1$genus[1], "_", tibble_data_df_1$species[1])
    tibble_data_df_2 <- tibble_data_df_1[c("genus_species", "Lon", "Lat")]
    names(tibble_data_df_2) <- c("species", "Lon", "Lat")
    rownames(tibble_data_df_2) <- NULL
    
    species_name_ref <- paste0(tibble_data_df_1$genus[1], "_", tibble_data_df_1$species[1])
    n_entries <- nrow(tibble_data_df_2)
    
    cat("--------------------------       unique vernet records       --------------------------\n\n")
    cat("species name: ", species_name_ref, " \n")
    cat("unique records: ", n_entries, " \n")
    cat("----------------------------------------------------------------------------------------\n\n")
    
    # dir.create
    
    if (is.null(out_dir) == FALSE) {
        dir.create(out_dir, showWarnings = F)
        setwd(out_dir)
        write.table(tibble_data_df_2, file = paste0(species_name_ref, "_total_", n_entries, "_unique_vernet.txt"), 
            sep = "\t", row.names = FALSE)
        setwd(master_directory)
        return(tibble_data_df_2)
    }
    
    setwd(master_directory)
    return(tibble_data_df_2)
    
}



################################################################################################################### cleanpoints_expanded##############################################################################################

# requires: alt_30s_bil/alt.bil

setwd("~/Desktop/Luisa_data/aa_cleanpoints")


cleanpoints_expanded <- function(spcoord, altpath, km_merge = 2, minimum_number_entries_value = 5, 
    alt_range_in_meters_lower_value = NULL, alt_range_in_meters_higher_value = NULL, IUCN_range_raster_map_value = NULL, 
    plot_clean_data_value = TRUE, out_dir_value = NULL) {
    
    require("raster", quietly = T)
    require("geosphere", quietly = T)
    require("maptools", quietly = T)
    require("dismo", quietly = T)
    require("rworldmap", quietly = T)
    data(wrld_simpl)
    data(countriesCoarse)
    
    # input from user
    
    species_data_raw <- spcoord
    alt_data_raster <- altpath
    y <- (km_merge)/100  # 1 km ~ 0.01 degrees (also 0.05 degrees ~ 5 km)
    minimum_number_entries <- minimum_number_entries_value
    alt_range_in_meters_lower <- alt_range_in_meters_lower_value
    alt_range_in_meters_higher <- alt_range_in_meters_higher_value
    
    IUCN_range_raster_map <- IUCN_range_raster_map_value
    
    plot_clean_data <- plot_clean_data_value
    out_dir <- out_dir_value
    
    master_directory <- getwd()
    
    # dir.create
    
    if (is.null(out_dir) == FALSE) {
        dir.create(out_dir, showWarnings = F)
        setwd(out_dir)
    }
    
    
    ########### preprocessing split file of species distributions to vectors ###########
    
    # read alt raster
    
    alt_data <- raster(alt_data_raster)
    
    # read coordinates file if not data frame
    
    if (class(spcoord) == "data.frame") {
        spcoord_table <- spcoord
    } else {
        if (grepl(pattern = "*.csv$", spcoord) == TRUE) {
            spcoord_table <- read.table(file = spcoord, header = TRUE, sep = ",", stringsAsFactors = FALSE)
        }
        if (grepl(pattern = "*.txt$", spcoord) == TRUE) {
            spcoord_table <- read.table(file = spcoord, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        }
    }
    
    # change name of species if not 'species'
    
    if ("species" %in% names(spcoord_table) == FALSE) {
        change_names <- names(spcoord_table)
        correct_names <- gsub("Species", "species", change_names)
        if ("species" %in% correct_names == FALSE) {
            correct_names <- gsub("genus_species", "species", change_names)
        }
        if ("species" %in% correct_names == FALSE) {
            correct_names <- gsub("Genus_species", "species", change_names)
        }
        if ("species" %in% correct_names == FALSE) {
            correct_names <- gsub("Genus_Species", "species", change_names)
        }
        if ("species" %in% correct_names == FALSE) {
            correct_names <- gsub("genus_species", "species", change_names)
        }
        if ("species" %in% correct_names == FALSE) {
            stop("please check that species names as -species- column exist")
        }
        names(spcoord_table) <- correct_names
    }
    
    # split file by species
    
    species_split <- try(split(spcoord_table, spcoord_table$species), silent = FALSE)
    
    if (class(species_split) == "try-error") {
        stop("the input species coordinates file or data frame is incorrect")
    }
    
    # summary of entries in the original dataset
    
    names_species_split <- as.data.frame(rownames(summary(species_split)))
    
    n_entries <- list()
    for (i in 1:nrow(names_species_split)) {
        n_entries[[i]] <- nrow(species_split[[i]])
    }
    
    n_entries_df <- as.data.frame(unlist(n_entries))
    names_species_split_df <- cbind(names_species_split, n_entries_df)
    names(names_species_split_df) <- c("species", "n_entries_raw")
    
    # from list to vectors
    
    Y <- lapply(seq_along(species_split), function(x) as.data.frame(species_split[[x]]))  #as dataframes
    Z_all <- lapply(seq_along(Y), function(x) unique(Y[[x]]))  #unique data values per dataframe
    
    ########################################################################################### 
    
    Z_one <- list()
    counter_1 <- 0
    
    for (i in 1:length(Z_all)) {
        
        if (nrow(Z_all[[i]]) > minimum_number_entries) {
            counter_1 <- counter_1 + 1
            Z_one[[counter_1]] <- Z_all[[i]]
        }
    }
    
    ## exclude if all the same
    
    Z_two <- list()
    counter_2 <- 0
    
    # Z_one[[3]]
    
    for (i in 1:length(Z_one)) {
        
        are_unique_rows <- unique(Z_one[[i]][, c("Lon", "Lat")])
        are_unique_lat <- unique(Z_one[[i]][, c("Lat")])
        are_unique_lon <- unique(Z_one[[i]][, c("Lon")])
        
        
        
        if (!nrow(are_unique_rows) <= 1 && !length(are_unique_lat) <= 1 && !length(are_unique_lon) <= 
            1) {
            counter_2 <- counter_2 + 1
            Z_two[[counter_2]] <- Z_one[[i]]
        }
        
    }
    
    Z <- Z_two
    
    
    ########### Start: processing loop for all species in the species file ###########
    
    summary_clean_list <- list()
    
    for (ww in 1:length(Z)) {
        
        # ww <- 1
        
        # processing
        
        Z_2 <- Z[[ww]]
        
        # get species name
        
        Z_2_species <- unique(Z_2$species)
        
        # subset Lon and Lat
        
        Z_2_Lon_Lat <- subset(Z_2, select = c(Lon, Lat))
        
        # get hr_res to for resolution to less than an hour
        
        Z_2_area_not_round <- (areaPolygon(Z_2_Lon_Lat))/1e+06  #area in square Km round 
        Z_2_area <- round(Z_2_area_not_round, 0)  #area in square Km round to integer
        Z_2_area_hr_res <- 10 - (round(log10(ifelse(Z_2_area + 1 > 1e+07, 1e+07, Z_2_area + 1)), 
            0) + 1)  #this function assing an interger from 2 to 9 (for really small area 1 km2) based on the area in km2. However, it forces to 2 for extremely large distributions > 10 000 000 km2
        
        # process to reduce redundant localities
        
        coordinates(Z_2) <- ~Lon + Lat  #set spatial coordinates to create a Spatial object, or retrieve spatial coordinates from a Spatial object
        crs(Z_2) <- crs(wrld_simpl)  #Get or set the coordinate reference system (CRS) of a Raster* object.
        Z_2_r <- try(raster(Z_2))
        res(Z_2_r) <- y  # y = 0.05 then 5 km ~0.05 degrees resolution
        Z_2_r_e <- extend(Z_2_r, extent(Z_2_r) + 1)
        Z_2_r_e_acsel <- gridSample(Z_2, Z_2_r_e, n = 1)
        Z_2_r_e_acsel_df <- data.frame(Z_2_r_e_acsel)
        
        # exclude localities in the ocean
        
        georef <- Z_2_r_e_acsel_df
        georef_alt <- cbind(georef, alt = extract(alt_data, georef, method = "bilinear"))
        species_selected_coordinates <- georef_alt[complete.cases(georef_alt$alt), ]
        
        # if IUCN_range_raster_map are present
        
        if (!is.null(IUCN_range_raster_map)) {
            iucn_raster <- raster(IUCN_range_raster_map)
            georef_iucn <- cbind(georef_alt, iucn = extract(iucn_raster, georef, method = "bilinear"))
            species_selected_coordinates <- subset(georef_iucn, iucn == 1)
        }
        
        # subset if altitude range is included
        
        if (is.null(alt_range_in_meters_lower) == FALSE) {
            species_selected_coordinates <- subset(species_selected_coordinates, alt > alt_range_in_meters_lower)
        }
        if (is.null(alt_range_in_meters_higher) == FALSE) {
            species_selected_coordinates <- subset(species_selected_coordinates, alt < alt_range_in_meters_higher)
        }
        
        # add other species info
        
        species_selected_coordinates$species <- Z_2_species
        rownames(species_selected_coordinates) <- NULL
        
        ## for summary
        
        n_entries_species <- nrow(Z_2_Lon_Lat)
        n_entries_clean <- nrow(species_selected_coordinates)
        summary_cleaning_df <- data.frame(species = Z_2_species, n_entries_raw = n_entries_species, 
            n_entries_clean = n_entries_clean, km_resolution_merging = y * 100, stringsAsFactors = FALSE)
        
        ## for summary
        
        summary_clean_list[[ww]] <- summary_cleaning_df
        
        # if plot is requested
        
        if (plot_clean_data == TRUE) {
            if (!is.null(IUCN_range_raster_map)) {
                plot(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch = 1, 
                  cex = 0.75, ylab = "latitude", xlab = "longitude")
                plot(iucn_raster, add = TRUE, legend = FALSE)
                points(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch = 1, 
                  cex = 0.75)
                plot(countriesCoarse, add = T, border = "blue", lwd = 1)
                title(main = Z_2_species)
                
                pdf(paste0(Z_2_species, "_total_", n_entries_clean, "_clean_points_res_", y * 
                  100, "_km2.pdf"))
                plot(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch = 1, 
                  cex = 0.75, ylab = "latitude", xlab = "longitude")
                plot(iucn_raster, add = TRUE, legend = FALSE)
                points(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch = 1, 
                  cex = 0.75)
                plot(countriesCoarse, add = T, border = "blue", lwd = 1)
                title(main = Z_2_species)
                dev.off()
            }
            
            if (plot_clean_data == FALSE) {
                plot(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch = 1, 
                  cex = 0.75, ylab = "latitude", xlab = "longitude")
                plot(countriesCoarse, add = T, border = "blue", lwd = 1)
                title(main = Z_2_species)
                dev.off()
                pdf(paste0(Z_2_species, "_total_", n_entries_clean, "_clean_points_res_", y * 
                  100, "_km2.pdf"))
                plot(species_selected_coordinates$Lon, species_selected_coordinates$Lat, pch = 1, 
                  cex = 0.75, ylab = "latitude", xlab = "longitude")
                plot(countriesCoarse, add = T, border = "blue", lwd = 1)
                title(main = Z_2_species)
                dev.off()
            }
        }
        
        # write clean points
        
        write.table(species_selected_coordinates, file = paste0(Z_2_species, "_total_", n_entries_clean, 
            "_clean_points_res_", y * 100, "_km2.txt"), sep = "\t", row.names = FALSE)
        
    }
    
    ########################################################################################## 
    
    # bind summaries
    
    n_entries_clean_bind <- do.call(rbind, summary_clean_list)
    
    # return list
    
    print(n_entries_clean_bind)
    setwd(master_directory)
    return(species_selected_coordinates)
    
}

########################## END OF FUNCTION ###################
