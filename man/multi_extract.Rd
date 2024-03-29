% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_extract.R
\name{multi_extract}
\alias{multi_extract}
\title{Gets values from multiple rasters.}
\usage{
multi_extract(
  raster_path,
  coord,
  folders = NULL,
  files = NULL,
  layers = NULL,
  ncores = 1
)
}
\arguments{
\item{raster_path}{character. Path to the folder with raster folders.}

\item{coord}{data.frame or matrix. Longitude and Latitude from where to extract raster values.}

\item{folders}{character. folders from which to get rasters for extraction. If NULL, all folders are selected.}

\item{files}{numeric. Index for raster files to be extracted from each folder. If NULL, all files are selected.}

\item{layers}{numeric. Index for layers to be extracted from each raster file. If NULL, all layers are selected.}

\item{ncores}{integer. Number of cores to use in parallel processing.}
}
\value{
Data frame with extracted values from multiple rasters
}
\description{
\code{multi_extract} Extract values of multiple spatial rasters for a set of geographical coordinates.
}
\examples{

\dontrun{
# replace rasterpath with the directory on your computer containing worldclim data

temp_pres <-
  multi_extract(raster_path = "mydir/rasters/worldclim/global_rasters_10min/",
                coord = TtorquatusDistribution[-1],
                folders = c("tmax_present", "tmin_present"))
                }

}
