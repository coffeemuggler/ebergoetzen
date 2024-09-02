#' Extract subsets or time series from local binary RADOLAN files
#'
#' The function allows to extract either precipitation time series from
#' points or spatially extensive areas of interest, or cropping the RADOLAN
#' raster data to single pixels or areas of interest.
#'
#' @param start \code{POSIXct} value, start time from which on the data shall
#' be examined If no POSIXct format is provided, the function will try to
#' convert the text string to POSIXct, assuming UTC as underlying time zone.
#'
#' @param stop \code{POSIXct} value, stop time unti which on the data shall
#' be examined If no POSIXct format is provided, the function will try to
#' convert the text string to POSIXct, assuming UTC as underlying time zone.
#'
#' @param dir \code{Character} value, path to directory where the downloaded
#' binary files will be stored.
#'
#' @param aoi Either a \code{simple feature} vector polygon or point object,
#' a \code{numeric} vector of length two with point coordinates, or a
#' \code{numeric} vector of length four with the boundary coordinates of a
#' rectangular area of interest (in the order \code{x_min}, \code{x_max},
#' \code{y_min}, \code{y_max}). All coordinates must be provided in the native
#' RADOLAN coordinate system.
#'
#' @param aggregate \code{Character} value, optional keyword defining the
#' aggregation operation to perform with the extracted subset. If omitted,
#' the function will return a list of cropped terra rasters. Meaningful
#' keywords may be \code{"mean"}, \code{"median"}, \code{"sum"}, \code{"var"},
#' \code{"sd"}.
#'
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel
#' processing. If omitted, one CPU is used.
#'
#' @return A list with case specific elements. There will always be one list
#' element \code{time} that contains the time stamp of the respective subset.
#' If no aggregation function keyword (\code{fun}) is provided, the second
#' list element will contain the extracted values, either as a numeric matrix
#' (if aoi is a simple feature or boundary box), or as a numeric vector (if
#' aoi is a xy coordinate). If an aggregation keyword is provided, the second
#' list element will be a numeric vector containing the respective aggregated
#' values. Finally, the element \code{coordinates} is a data frame with the
#' extracted pixel IDs and their respective coordinates.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' \dontrun{
#'
#'   rdl <- dwd_subset(start = as.POSIXct("2022-01-29 12:00:00"),
#'                     stop = as.POSIXct("2022-01-29 18:00:00"),
#'                     dir = "~/Downloads/RADOLAN/daily/",
#'                     aoi = c(576000, 578000, 5712000, 5715000))
#'
#' }
#'
#' @export dwd_subset

dwd_subset <- function(

  start,
  stop,
  dir,
  aoi,
  aggregate,
  cpu

) {

  ## CHECK OR SET ARGUMENTS ---------------------------------------------------

  ## check start time format
  if(class(start)[1] != "POSIXct") {

    start <- try(as.POSIXct(start, tz = "UTC"))

    if(class(start)[1] == "try-error") {

      stop("Start time is no POSIXct or convertible format!")
    }
  }

  ## check stop time format
  if(class(stop)[1] != "POSIXct") {

    stop <- try(as.POSIXct(stop, tz = "UTC"))

    if(class(stop)[1] == "try-error") {

      stop("Stop time is no POSIXct or convertible format!")
    }
  }

  ## check that start time is smaller than stop time
  if(stop <= start) {

    stop("Start time must be smaller than stop time!")
  }

  ## check that data directory exists
  if(dir.exists(dir) == FALSE) {

    stop("Input directory does not exist!")
  }

  ## check aoi definition
  if(missing(aoi) == TRUE) {

    stop("No area of interest defined!")
  } else {

    if(class(aoi)[1] == "sf") {

      ## simple feature case
      type_aoi <- "sf"
    } else if(class(aoi)[1] == "numeric") {

      ## coordinate versus boundary box case
      if(length(aoi) == 2) {

        type_aoi <- "xy"

      } else if(length(aoi) == 4) {

        type_aoi <- "bb"

      } else {

        stop("AOI is neither a coordinate or boundary box!")
      }
    } else {

      stop("AOI is not in a supported format!")
    }
  }

  ## check aggregation keyword
  if(missing(aggregate) == FALSE) {

    if(aggregate %in% c("sum", "mean", "median", "var", "sd") == FALSE) {

      stop("Keyword for aggregation function not supported!")
    }
  } else {

    aggregate <- "nada"
  }

  ## PREPARE RASTER IMPORT ----------------------------------------------------

  ## make list of files to import
  files <- list.files(path = dir, full.names = TRUE)

  ## extract time stamps
  t_files <- do.call(c, lapply(X = files, FUN = function(x) {

    t <- substr(x = strsplit(x = x,
                             split = "raa01-rw_10000-",
                             fixed = TRUE)[[1]][2],
                start = 1,
                stop = 8)
    t <- as.POSIXct(x = t, format = "%y%m%d%H", tz = "UTC")

    return(t)
  }))

  ## remove time stamps out of start-stop window
  i_out <- which(t_files < start | t_files > stop)

  if(length(i_out) > 0) {
    t_files <- t_files[-i_out]
    files <- files[-i_out]
  }

  ## PROCESS RADOLAN RASTERS --------------------------------------------------

  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()

  if(is.na(cpu) == FALSE) {

    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {

    cores <- 1
  }

  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))

  ## process all files
  y <- parallel::parLapply(
    cl = cl, X = files, fun = function(f, tp, aoi, agg) {

      ## read file
      r <- try(dwdradar::readRadarFile(binfile = f), silent = TRUE)

      ## convert to raster object
      r <- try(terra::rast(r$dat), silent = TRUE)

      ## define CRS
      try(terra::crs(r) <- paste0("+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 ",
                              "+k=0.93301270189 +x_0=0 +y_0=0 +a=6370040 ",
                              "+b=6370040 +to_meter=1000 +no_defs"),
          silent = TRUE)
      try(terra::ext(r) <- c(-523.4622,376.5378,-4658.645,-3758.645),
          silent = TRUE)

      ## project to UTM
      r <- try(terra::project(x = r, y = "+proj=utm +zone=32 +datum=WGS84"),
               silent = TRUE)

      ## case aoi is simple feature
      if(tp == "sf") {

        r <- try(terra::crop(x = r, y = aoi), silent = TRUE)
        co <- try(cbind(terra:: cells(r), terra::crds(x = r)), silent = TRUE)

      }

      ## case aoi is xy coordinate
      if(tp == "bb") {

        r <- try(terra::crop(x = r, y = aoi), silent = TRUE)
        co <- try(cbind(terra:: cells(r), terra::crds(x = r)), silent = TRUE)

      }

      ## case aoi is xy coordinate
      if(tp == "xy") {

        r <- try(terra::crop(x = r, y = c(aoi[1] - 0.1, aoi[1] + 0.1,
                                      aoi[2] - 0.1, aoi[2] + 0.1)),
                 silent = TRUE)
        co <- try(cbind(terra:: cells(r), terra::crds(x = r)), silent = TRUE)

      }

      ## convert raster to matrix
      r <- try(matrix(data = terra::values(r),
                  nrow = dim(r)[1],
                  ncol = dim(r)[2],
                  byrow = TRUE), silent = TRUE)

      ## optionally, extract values and aggregate them
      if(agg != "nada" & class(r)[1] != "try-error") {

        r <- as.numeric(r)

        if(length(r) == 1) {

          r <- rep(r, 2)
        }

        if(agg == "mean") {

          r <- mean(r, na.rm = TRUE)
        }

        if(agg == "median") {

          r <- median(r, na.rm = TRUE)
        }

        if(agg == "sum") {

          r <- sum(r, na.rm = TRUE)
        }

        if(agg == "var") {

          r <- var(r, na.rm = TRUE)
        }

        if(agg == "sd") {

          r <- sd(r, na.rm = TRUE)
        }
      }

      if(class(r)[1] == "try-error") {

        r <- NA
      }

      return(list(r = r, co = co))

    }, tp = type_aoi, aoi = aoi, agg = aggregate)

  parallel::stopCluster(cl = cl)

  ## manage output
  co <- y[[1]]$co
  y <- lapply(X = y, FUN = function(y) {y$r})

  ## optionally simplify output
  if(aggregate != "nada") {

    y <- try(do.call(c, y), silent = TRUE)

  }

  ## optionally collect raster coordinates



  out = list(time = t_files,
             data = y,
             coordinates = co)

  return(out)
}
