
  Perf_rasters <- function(raster_source, Perf_args, separator, PerfFUN) {

    if (class(raster_source[[1]]) == 'RasterStack') {

      raster_list <-
        raster_source

    }

  args_rasters <-
    Perf_args %>%
    sapply(., function(x) class(x) == "character") %>%
    `[`(Perf_args, .)

  args_constant <-
    Perf_args %>%
    sapply(., function(x) class(x) == "numeric") %>%
    `[`(Perf_args, .)

    Perf_list <-
      lapply(raster_list, function(x) {

        raster_by_arg <-
          lapply(args_rasters, function(y) {

            x %>%
              names() %>%
              grep(paste("^", y, sep = ""), .) %>%
              `[[`(x, .)

          } # close function
          ) # close alply

        raster_by_arg_by_rep <-
          lapply(raster_by_arg, function(x){

            repeated_names <-
              x %>%
              names() %>%
              strsplit(separator) %>%
              lapply(function(x) `[`(x, length(x)) ) %>%
              unlist()

            repeated_list <- raster::unstack(x)

            names(repeated_list) <- repeated_names

            repeated_list

          } # close function
          ) # close lapply

        reversed_list <-
          raster_by_arg_by_rep %>%
          purrr::transpose()

        Perf_rasters <- lapply(reversed_list, function(x){

          call_list <-
            x %>%
            append(args_constant)

          prediction <- do.call(PerfFUN, args = call_list)

          prediction

        } # close function
        ) # close lapply

        names(Perf_rasters) <- paste("Perf", names(Perf_rasters), sep = separator)

        raster::stack(Perf_rasters)

      } # close function
      ) # close lapply

    return(Perf_list)
  }
