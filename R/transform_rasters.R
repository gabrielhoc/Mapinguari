#' Transform raster values using custom calls.
#'
#' \code{transform_rasters} Applies custom expressions to transform the values of spatial rasters in a stack, taking into account temporal repetition of those rasters.
#'
#' @importFrom magrittr "%>%"
#'
#' @param raster_stack RasterStack. Stack with environmental layers.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param ncores integer. Number of cores to use in parallel processing.
#' @param ... New rasters created.
#'
#' @return Returns a RasterStack with layers for the predictions required.
#'
#' @examples
#' \dontrun{
#' FulanusEcoRasters_present <-
#'   get_rasters(
#'     var = c('prec', 'tmax', 'tmin'),
#'     scenarios = 'present',
#'     source = "C:/Users/gabri/Dropbox/Mapinguari/global_grids_10_minutes",
#'     ext = FulanusDistribution[c(2,3)],
#'     margin = 5,
#'     reorder = c(1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9))
#'
#' # You can apply any function to subsets of rasters in the stack,
#' # by selecting the layers with double brackets.
#'
#' transform_rasters(raster_stack = FulanusEcoRasters_present$present,
#'     total_1sem = sum(tmax[1:6]),
#'     mean_1sem = mean(tmax[1:6]),
#'     sd_1sem = sd(tmax[1:6]),
#'     total_2sem = sum(tmax[7:12]),
#'     mean_2sem = mean(tmax[7:12]),
#'     sd_2sem = sd(tmax[7:12]))
#'}
#' @export

transform_rasters <-
  function(raster_stack,
           separator = '_',
           ncores = 1,
           ...){

    transform_expr <- as.list(substitute(list(...))[-1])

    template_raster <- raster_stack[[1]]
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))

    split_vars <-
      raster_stack %>%
      names() %>%
      stringr::str_split(separator) %>%
      lapply(`[`, 1)

    split_ind <-
      raster_stack %>%
      names() %>%
      stringr::str_split(separator) %>%
      lapply(`[`, 2)

    unique_split_vars <-
      unique(split_vars) %>%
      unlist

    unique_split_ind <-
      unique(split_ind) %>%
      unlist %>%
      `[`(., !is.na(.))

    separate_list <-
      lapply(unique_split_vars, function(y){

        which(split_vars == y) %>%
          raster_stack[[.]] %>%
          raster::stack()
      })

    raster_value_list <- list(lapply(separate_list, raster::values))

    output <-
      lapply(1:length(transform_expr), function(ww){

        current_call <- transform_expr[[ww]]

        call_obj_in <-
          as.list(current_call)

        call_obj_out <- NULL

        success <- FALSE

        while (!success) {

          call_obj_out <-
            call_obj_in %>%
            lapply(as.list) %>%
            unlist

          success <- identical(call_obj_in, call_obj_out)

          call_obj_in <- call_obj_out

        }

        call_obj_name <-
          call_obj_out %>%
          lapply(function(yy){
            class(yy) == 'name'
          }) %>%
          unlist %>%
          call_obj_out[.]

        exist_ind <-
          call_obj_name %>%
          lapply(function(yy){
            try(eval(yy), silent = TRUE) %>%
              class %>%
              `!=`(., "try-error")
          }) %>%
          unlist

        call_obj <-
          call_obj_name[exist_ind] %>%
          lapply(paste) %>%
          unlist

          lapply(raster_value_list, function(z){

            parallel::clusterExport(cl, varlist = c(call_obj, "current_call", "unique_split_vars", "z"), envir = environment())

            output_values <-
              parallel::parSapply(cl, 1:nrow(z[[1]]), function(i){

                for (j in 1:length(unique_split_vars)) {
                  assign(unique_split_vars[j], z[[j]][i,], inherits = TRUE)
                }

                eval(current_call)

              })

            if(is.matrix(output_values)) {

              template_raster <- raster::stack(replicate(nrow(output_values), template_raster))

              output_values_df <- data.frame(t(output_values))

              for(nl in 1:nrow(output_values)) raster::values(template_raster[[nl]]) <- output_values_df[[nl]]

              names(template_raster) <- paste(names(transform_expr)[ww], unique_split_ind, sep = separator)

            } else {

            raster::values(template_raster) <- output_values
            names(template_raster) <- names(transform_expr)[ww]

            }

            template_raster
          })

      })

    return(raster::stack(unlist(output)))

  }

utils::globalVariables(".")
