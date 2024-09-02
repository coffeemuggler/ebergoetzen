#' Download binary RADOLAN files from DWD Climate Data Centre server - BUG: CHECK HOW RECENT-HIST FLIP DATE (MONTH) IS DEFINED BY DWD
#'
#' The function manages the download of RADOLAN gridded precipitation raster
#' data sets from the DWD Climate Data Centre based on user defined start and
#' stop times and saves them as unpacked binary files in a user defined output
#' directory. Currently, only hourly and 5-min RADOLAN files are supported.
#'
#' @param start \code{POSIXct} value, start time from which on the data shall
#' be downloaded. If no POSIXct format is provided, the function will try to
#' convert the text string to POSIXct, assuming UTC as underlying time zone.
#'
#' @param stop \code{POSIXct} value, stop time unti which on the data shall
#' be downloaded. If no POSIXct format is provided, the function will try to
#' convert the text string to POSIXct, assuming UTC as underlying time zone.
#'
#' @param dir \code{Character} value, path to directory where the downloaded
#' binary files will be stored.
#'
#' @param resolution \code{Character} values, keyword defining the temporal
#' resolution of the data to download. Possible choices are \code{"hourly"}
#' and \code{"5-min"}. Default value is \code{"hourly"}.
#'
#' @return Files written to a local directory.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' \dontrun{
#'
#'   dwd_get(start = as.POSIXct("2022-01-28"),
#'           stop = as.POSIXct("2022-02-02"),
#'           dir = "~/Downloads/RADOLAN/daily/")
#'
#' }
#'
#' @export dwd_get

dwd_get <- function(

  start,
  stop,
  dir,
  resolution = "hourly"

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

  ## check resolution keyword
  if(resolution %in% c("hourly") == FALSE) {

    stop("Keyword for resolution not supported!")
  }

  ## CASE hourly DATA ----------------------------------------------------------
  if(resolution == "hourly") {

    ## get time range vector
    t_range <- c(start, stop)

    ## define flip time between recent and historical data archive
    t_flip <- as.POSIXct(seq(Sys.Date(), length = 2, by = "-1 year")[2],
                         tz = "UTC")

    ## define historical data part
    if(start < t_flip) {

      start_his <- as.Date(start)

      ## set stop date for historical data
      if(stop > t_flip) {

        stop_his <- t_flip
        start_act <- t_flip
        stop_act <- stop
      } else {

        stop_his <- stop
        stop_act <- NA
        start_act <- NA
      }
    } else {

      start_his <- NA
      stop_his <- NA
      start_act <- start
      stop_act <- stop
    }

    ## CASE HISTORICAL DATA ---------------------------------------------------

    if(!is.na(start_his)) {

      ## define time series to download data for
      t_his <- as.POSIXct(seq(from = as.Date(start_his),
                              to = as.Date(stop_his),
                              by = "1 month"),
                          tz = "UTC")

      ## define base URL
      url_base <- paste0("https://opendata.dwd.de/climate_environment/CDC/",
                         "grids_germany/hourly/radolan/historical")

      ## append path to binary files
      url_base <- paste0(url_base, format(t_his, "/bin/%Y/"))

      ## define file names based on time stamps
      url_file <- format(t_his, "RW%Y%m.tar.gz")

      ## paste URL and online file names
      url_get <- paste0(url_base, url_file)

      ## combine strings
      url <- data.frame(get = url_get, file = url_file)

      ## reduce to unique files
      url <- url[!duplicated(x = url),]

      ## process all relevant data sets
      for(i in 1:length(url$get)) {

        ## download file
        try(utils::download.file(url = url$get[i],
                                 destfile = paste0(path.expand(dir), "/",
                                                   url$file[i]),
                                  method = "libcurl"), silent = TRUE)

        ## untar files
        try(utils::untar(tarfile = paste0(path.expand(dir), "/", url$file[i]),
                         exdir = path.expand(dir)), silent = TRUE)

        ## remove tar archive
        try(unlink(x = paste0(dir, "/", url$file[i]), recursive = TRUE),
            silent = TRUE)
      }
    }

    ## CASE RECENT DATA -------------------------------------------------------

    if(!is.na(start_act)) {

      ## define time series to download data for
      t_act <- seq(from = start_act, to = stop_act, by = 3600)

      ## define base URL
      url_base <- paste0("https://opendata.dwd.de/climate_environment/CDC/",
                         "grids_germany/hourly/radolan/recent/bin/")

      ## define file names based on time stamps
      url_file <- format(t_act, "raa01-rw_10000-%y%m%d%H50-dwd---bin.gz")

      ## paste URL and online file names
      url_get <- paste0(url_base, url_file)

      ## combine strings
      url <- data.frame(get = url_get, file = url_file)

      ## reduce to unique files
      url <- url[!duplicated(x = url),]

      ## process all relevant data sets
      for(i in 1:length(url$get)) {

        ## download file
        try(utils::download.file(url = url$get[i],
                                 destfile = paste0(dir, "/", url$file[i]),
                                 method = "libcurl"), silent = TRUE)

        ## unzip and remove archive
        try(R.utils::gunzip(paste0(dir, "/", url$file[i])), silent = TRUE)
      }
    }

    ## collect file time stamps
    f_collect <- list.files(path = dir, full.names = TRUE)
    t_collect <- list.files(path = dir)

    ## convert time stamps to POSIX format
    t_collect <- as.POSIXct(x = t_collect,
                            format = "raa01-rw_10000-%y%m%d%H50-dwd---bin",
                            tz = "UTC")

    ## identify files out start-stop time interval
    i_out <- which(t_collect < t_range[1] | t_collect > t_range[2])

    ## remove identified files
    unlink(x = f_collect[i_out], recursive = TRUE)
  }
}
