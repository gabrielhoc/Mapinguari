missing_vars = absent_vars

derive_rasters <- function(missing_vars, resolution, ext, projection_model) {

  if (length(missing_vars) == 0) return(0)

vars_for_derivation <-
  match(missing_vars$vys_vars, derivable_vars) %>%
  missing_vars[.,]

derived_rasters <-
 plyr::alply(1:nrow(vars_for_derivation), 1, function(i){

   table_slice <-
     vars_for_derivation %>%
     dplyr::slice(i)

   if (any(match(table_slice$vys_vars, latitude_aliases))) {}

   if (any(match(table_slice$vys_vars, longitude_aliases))) {}

   if (any(match(table_slice$vys_vars, hours_of_sunlight_aliases))) {}

   if (any(match(table_slice$vys_vars, slope_aliases))) {}

   if (any(match(table_slice$vys_vars, aspect_aliases))) {}

   if (any(match(table_slice$vys_vars, solar_radiation_aliases))) {}

   if (any(match(table_slice$vys_vars, PET_aliases))) {}

   if (any(match(table_slice$vys_vars, AET_aliases))) {}

   if (any(match(table_slice$vys_vars, CWD_aliases))) {}

} # close function
  ) # close alply

return(derived_rasters)

}
