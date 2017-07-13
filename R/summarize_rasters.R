#' Groups rasters by duration of phenological event.
#'
#' \code{summarize_rasters} Summarize rasters by duration of phenological event.
#'
#' @param raster_stack RasterStack.
#' @param seasons numerical vector or function. Months at the beggining and end of phenological events, or function relating phenological event to environmental conditions. If NULL, no summarizing is made (remains by month).
#' @param season_args named list. Correspondence between 'seasons' arguments and raster names.
#' @param summaryFUN function. Function used to summarize months inside seasons.
#' @param summary_args named list. Additional arguments for summarizing function.
#' @param separator character. Character that separates variable names, years and scenarios.
#' @param time_res integer. How many layers do time varying variables have? Default is 12, as in 12 months in a year, the time resolution in WorldClim.
#'
#' @return Returns a list of raster stacks for the variables required, organized by year/scenario combination.
#'
#' @examples
#'
#' Fulanus_Ecorasters_download <-
#'   get_rasters(
#'     ext = FulanusDistribution,
#'     margin = 5,
#'     non_fixed_var = c('prec', 'tmin', 'tmax'),
#'     years = c("present", '2050', '2070'),
#'     scenarios = c('rcp26', 'rcp45', 'rcp85'),
#'     alert = 6)
#'
#' Phenology_mean_sd <-
#'   lapply(Fulanus_Ecorasters_download, summarize_rasters,
#'     seasons = list(breeding = c(3:8), non_breeding = c(9:12, 1, 2)),
#'     summaryFUN = list(tmax = c("mean", "sd"), tmin = c("mean", "sd"), prec = "sum"))
#'
#' Phenology_weighted_mean <-
#'   lapply(Fulanus_Ecorasters_download, summarize_rasters,
#'     seasons = list(breeding = c(3:8), non_breeding = c(9:12, 1, 2)),
#'     summaryFUN = "weighted.mean",
#'     summary_args = list(w = c(0.5, rep(1, 4), 0.5))
#'   )
#'
#' PhenFUN <- function(x) 1/(1 + exp((150 - x)/2))
#'
#' Phenology_by_precFUN <-
#'   lapply(Fulanus_Ecorasters_download, summarize_rasters,
#'     seasons = list(rainy_season = PhenFUN),
#'     seasons_args = list(x = 'prec'),
#'     summaryFUN = "raster::weighted.mean",
#'     summary_args = list(w = "rainy_season")
#'   )
#'
#' @export
summarize_rasters <- function(raster_stack,
  seasons = list(year = c(1:12)),
  seasons_args = NULL,
  summaryFUN = "mean",
  summary_args = NULL,
  separator = '_',
  time_res = 12) {

  # split stack by variable

  split_vars <-
    raster_stack %>%
    names() %>%
    stringr::str_split(separator) %>%
    lapply(`[`, 1)

  unique_split_vars <- unique(split_vars)

  separate_list <-
    lapply(unique_split_vars, function(y){

      which(split_vars == y) %>%
        raster_stack[[.]] %>%
        raster::stack()
    }
    )

  names(separate_list) <- unique_split_vars

  # separate fixed from non fixed variables

  non_fixed_logical <-
    lapply(separate_list, function(u) length(names(u)) == time_res) %>%
    unlist()

  non_fixed_list <- separate_list[which(non_fixed_logical)]

  non_fixed_names <- names(separate_list)[which(non_fixed_logical)]

  names(non_fixed_list) <- non_fixed_names

  fixed_list <- separate_list[which(!non_fixed_logical)]

  fixed_names <- names(separate_list)[which(!non_fixed_logical)]

  names(fixed_list) <- fixed_names

  seasons_mod <- lapply(seasons, function(y){

    if (class(y) == 'numeric' | class(y) == 'integer') {

      y

    } else if (class(y) == 'function') {

#      pred_raster <-
#        seasons_args %in% split_vars %>%
      #   which() %>%
      #   `[[`(seasons_args, .)
      #
      # vars_only <-
      #   raster_stack[[which(unlist(split_vars) == pred_raster)]]
      #
      # ncores <- parallel::detectCores() - 1
      # raster::beginCluster(ncores, type = 'SOCK')
      #
      # Phen_rasters <- raster::clusterR(vars_only, raster::overlay, args = list(fun = y))
      #
      # raster::endCluster()
      #

      Phen_rasters <- transform_rasters(raster_stack = raster_stack, transformFUN = y, transformFUN_args = seasons_args, separator = separator, time_res = time_res)

      names(Phen_rasters) <- paste("phen", 1:time_res, sep = separator)

      Phen_rasters

      }

  }
  )

  separated_rasters <-
    lapply(non_fixed_list, function(x){
      lapply(seasons_mod, function(y){

        if (class(y) == 'integer' | class(y) == 'numeric') {

          raster::subset(x, y) %>%
            raster::stack()

        } else if (class(y) == 'RasterStack' | class(y) == 'RasterBrick') {

          z <- x*y
          raster::stack(z)

        }

      } # close function
      ) # close lapply
    } # close function
    ) # close lapply

  is_Phen_stack <-
    lapply(seasons_mod, function(x){

      class(x) == 'RasterStack' | class(x) == 'RasterBrick'

    }
    )

  Phen_stacks <- seasons_mod[unlist(is_Phen_stack)]

  if (length(Phen_stacks) > 0) {
  separated_rasters <- append(separated_rasters,list(phen = Phen_stacks))
  }

    if (class(summaryFUN) != "list") {

      summaryFUN_list <-
        as.list(rep(summaryFUN, length(separated_rasters)))
      names(summaryFUN_list) <- names(separated_rasters)

    } else {
      summaryFUN_list <- summaryFUN
}

    if (!is.null(summary_args)) {

    unique_summary_FUN <- unique(unlist(summaryFUN_list))

    if (class(summary_args[[1]]) != "list") {

      summary_args_list <-
        lapply(unique_summary_FUN, function(x){

          summary_args

        }
        )

      names(summary_args_list) <- unique_summary_FUN

    } else {
      summary_args_list <- summary_args
    }

    summary_args_list_sub <-
    lapply(summary_args_list, function(x){

      lapply(x, function(y){

        if (length(y) == 1) {
        if (y %in% names(separated_rasters$phen)) {
          separated_rasters$phen[[y]]
        }
        } else {
          y
        }

      }
        )

    }
      )

    }

    summarized_rasters <-
      lapply(separated_rasters, function(x){

        var_name <-
          names(x[[1]]) %>%
          `[`(1) %>%
          strsplit(separator) %>%
          `[[`(1) %>%
          `[`(1)

        combinations <-
          paste(var_name, names(x), sep = separator) %>%
          expand.grid(summaryFUN_list[[var_name]])

        layer_names <-
          paste(combinations[[1]], combinations[[2]], sep = separator)

        if (is.null(summary_args)) {

        output_list <-
          lapply(summaryFUN_list[[var_name]], function(y){
            lapply(x, function(z){

            result_raster <- try(
              raster::calc(z, fun = match.fun(y))
            )

          } # close function
          ) # close lapply

          } # close function
          ) # close lapply

        } else {

          output_list <-
            lapply(summaryFUN_list[[var_name]], function(y){

              summaryFUN_call <-
                function(w){

                  n <- length(summary_args_list[[y]])

                  summary_args_list_int <- summary_args_list_sub[[y]]

                  summary_args_list_int <-
                    summary_args_list_int %>%
                    append(list(w))

                  names(summary_args_list_int)[n + 1] <- formalArgs(pander::evals(y)[[1]]$result)[1]

                  do.call(what = pander::evals(y)[[1]]$result, args = summary_args_list_int)
                }

              lapply(x, function(z){

              result <- try(raster::calc(z, summaryFUN_call), silent = TRUE)

              if (class(result) == 'try-error') {

              result <- summaryFUN_call(z)

              }

              return(result)

              }
              )

            } # close function
            ) # close lapply

        }

        output_stack <- raster::stack(unlist(output_list))

        names(output_stack) <- layer_names

        return(output_stack)

      } # close function
      ) # close rapply

    summarized_stack <- raster::stack(summarized_rasters)

    final_stack <- raster::stack(summarized_stack, fixed_list)

    return(final_stack)

}
