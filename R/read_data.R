#' Load event data from Ebergoetzen data set
#'
#' The function reads the full data set of an event, defined by start time
#' and duration. It assumes the data is organised in directories by year and
#' as daily files.
#'
#' @param start \code{Character} value, start time of the data to import. If
#' lazy users only submit a text string instead of a POSIXct object, the
#' function will try to convert that text string.
#'
#' @param stop \code{Character} value, stop time of the data to import. If
#' lazy users only submit a text string instead of a POSIXct object, the
#' function will try to convert that text string.
#'
#' @param duration \code{Numeric} value, alternative to definition of
#' \code{stop} argument, duration of the event for which the data will be
#' imported, in seconds.
#'
#' @param dir \code{Character} value, path to data directory.
#'
#' @param average \code{Logical} value, option to average redundant
#' sensor data. Default is \code{FALSE}. Only works when the output is
#' an \code{egd} (EberGoetzenData) object, i.e. \code{egd = TRUE}.
#'
#' @param aggregate \code{Numeric} value, factor by which to aggregate the
#' data set of initially 15 min sampling interval. If omitted, no aggregation
#' will be applied.
#'
#' @param smoothen \code{Numeric} value, factor by which to smoothen the
#' data by running mean filter. If omitted, no smoothing is performed.
#' Option requires package caTools to be installed.
#'
#' @param cum2rate \code{Logical} value, option to convert cumulative
#' precipitation values to rates (first derivative) Default is \code{TRUE}.
#'
#' @param egd \code{Logical} value, option to return the data organised as
#' \code{egd} (EberGoetzenData) object. Default is \code{TRUE}. If set to
#' \code{FALSE}, a data frame will be returned, instead.
#'
#' @param silent \code{Logical} value, option to suppress warnings, for
#' example of missing input data. Default is \code{FALSE}.
#'
#' @return Either an \code{egd} object or a \code{data.frame} with data
#' from the Ebergoetzen site for the defined event time interval.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' \dontrun{
#'
#' ## define example data path
#' dir_data <- paste0(system.file("extdata", package="ebergoetzen"), "/")
#'
#' ## read one day of EG data
#' x <- read_data(start = "2023-01-01",
#'                duration = 24 * 3600,
#'                dir = dir_data)
#'
#' plot(x$time, x$meteo$temperature$north$hydroclip, type = "l")
#' }
#'
#' @export read_data

read_data <- function(

  start,
  stop,
  duration,
  dir,
  average = FALSE,
  aggregate,
  smoothen,
  cum2rate = TRUE,
  egd = TRUE,
  silent = FALSE

) {

  ## define helper function
  convsumrate <- function(x) {
    diff_x <- diff(x)
    lead_x <- ifelse(is.na(diff_x[1]), NA, 0)
    y <- c(lead_x, diff_x)
    y[y < 0] <- 0
    return(y)
  }

  ## check start time format
  if(class(start)[1] != "POSIXct") {

    start <- try(as.POSIXct(start), silent = TRUE)

    if(class(start)[1] != "POSIXct") {

      stop("Start date is not a POSIXct format!")
    }
  }

  ## calculate end time
  if(missing(stop) == TRUE) {

    stop <- start + duration - 1

  } else {

    if(class(stop)[1] != "POSIXct") {

      stop <- try(as.POSIXct(stop), silent = TRUE)

      if(class(stop)[1] != "POSIXct") {

        stop("Stop date is not a POSIXct format!")
      }
    }
  }

  ## round time stamps to full days
  d_start <- as.POSIXct(format(x = start, format = "%Y-%m-%d"), tz = "UTC")
  d_stop <- as.POSIXct(format(x = stop, format = "%Y-%m-%d"), tz = "UTC")

  ## make sequence of days to import
  d_seq <- seq(from = d_start, to = d_stop, by = 24 * 3600)

  ## build file names from day sequence
  f_seq <- paste0(dir, "/",
                  format(d_seq, format = "%Y/%Y-%m-%d_ebergoetzen.txt"))

  ## import file(s)
  x <- try(lapply(X = f_seq, FUN = function(x) {

    ## try to read file
    y <- try(read.table(file = x,
                    header = TRUE,
                    sep = "\t",
                    stringsAsFactors = FALSE), silent = TRUE)

    ## try to convert time stamp to POSIXct format
    try(y$time <- as.POSIXct(y$time, tz = "UTC"))

    ## return successfully imported data
    if(class(y)[1] == "data.frame") {

      return(y)
    } else {

      return(NULL)
    }
  }), silent = silent)

  ## convert list to data frame
  x <- do.call(rbind, x)

  ## clip to input time interval
  x <- x[x$time >= start & x$time <= stop,]

  ## optionally convert cumulative sums to rates
  if(cum2rate == TRUE) {

    x$throughfall_north_a1_00 <- convsumrate(x$throughfall_north_a1_00)
    x$throughfall_north_a2_00 <- convsumrate(x$throughfall_north_a2_00)
    x$throughfall_north_a3_00 <- convsumrate(x$throughfall_north_a3_00)
    x$throughfall_north_a4_00 <- convsumrate(x$throughfall_north_a4_00)
    x$throughfall_north_a5_00 <- convsumrate(x$throughfall_north_a5_00)
    x$throughfall_north_f1_00 <- convsumrate(x$throughfall_north_f1_00)
    x$throughfall_north_f2_00 <- convsumrate(x$throughfall_north_f2_00)
    x$throughfall_north_f3_00 <- convsumrate(x$throughfall_north_f3_00)
    x$throughfall_north_f4_00 <- convsumrate(x$throughfall_north_f4_00)
    x$throughfall_north_f5_00 <- convsumrate(x$throughfall_north_f5_00)
    x$throughfall_north_j1_00 <- convsumrate(x$throughfall_north_j1_00)
    x$throughfall_north_j2_00 <- convsumrate(x$throughfall_north_j2_00)
    x$throughfall_north_j3_00 <- convsumrate(x$throughfall_north_j3_00)
    x$throughfall_north_j4_00 <- convsumrate(x$throughfall_north_j4_00)
    x$throughfall_north_j5_00 <- convsumrate(x$throughfall_north_j5_00)

    x$throughfall_south_a1_00 <- convsumrate(x$throughfall_south_a1_00)
    x$throughfall_south_a2_00 <- convsumrate(x$throughfall_south_a2_00)
    x$throughfall_south_a3_00 <- convsumrate(x$throughfall_south_a3_00)
    x$throughfall_south_a4_00 <- convsumrate(x$throughfall_south_a4_00)
    x$throughfall_south_a5_00 <- convsumrate(x$throughfall_south_a5_00)
    x$throughfall_south_f1_00 <- convsumrate(x$throughfall_south_f1_00)
    x$throughfall_south_f2_00 <- convsumrate(x$throughfall_south_f2_00)
    x$throughfall_south_f3_00 <- convsumrate(x$throughfall_south_f3_00)
    x$throughfall_south_f4_00 <- convsumrate(x$throughfall_south_f4_00)
    x$throughfall_south_f5_00 <- convsumrate(x$throughfall_south_f5_00)
    x$throughfall_south_j1_00 <- convsumrate(x$throughfall_south_j1_00)
    x$throughfall_south_j2_00 <- convsumrate(x$throughfall_south_j2_00)
    x$throughfall_south_j3_00 <- convsumrate(x$throughfall_south_j3_00)
    x$throughfall_south_j4_00 <- convsumrate(x$throughfall_south_j4_00)
    x$throughfall_south_j5_00 <- convsumrate(x$throughfall_south_j5_00)

    x$stemflow_north_g1_00 <- convsumrate(x$stemflow_north_g1_00)
    x$stemflow_north_g2_00 <- convsumrate(x$stemflow_north_g2_00)
    x$stemflow_north_m1_00 <- convsumrate(x$stemflow_north_m1_00)
    x$stemflow_north_m2_00 <- convsumrate(x$stemflow_north_m2_00)
    x$stemflow_north_k1_00 <- convsumrate(x$stemflow_north_k1_00)
    x$stemflow_north_k2_00 <- convsumrate(x$stemflow_north_k2_00)

    x$stemflow_south_g3_00 <- convsumrate(x$stemflow_south_g3_00)
    x$stemflow_south_g4_00 <- convsumrate(x$stemflow_south_g4_00)
    x$stemflow_south_m3_00 <- convsumrate(x$stemflow_south_m3_00)
    x$stemflow_south_m4_00 <- convsumrate(x$stemflow_south_m4_00)
    x$stemflow_south_k3_00 <- convsumrate(x$stemflow_south_k3_00)
    x$stemflow_south_k4_00 <- convsumrate(x$stemflow_south_k4_00)
  }

  ## optionally aggregate data set
  if(missing(aggregate) == FALSE) {

    x_names <- names(x)

    x <- lapply(X = x_names, FUN = function(name, x, n, cum2rate) {

      x_i <- x[,names(x) == name]

      if(grepl(x = name, pattern = "stemflow") |
         grepl(x = name, pattern = "throughfall")) {

        if(cum2rate == TRUE) {

          fun_method <- "sum"
        } else {

          fun_method <- "mean"
        }

      } else if(grepl(x = name, pattern = "precipitation")) {

        fun_method = "sum"

      } else if(grepl(x = name, pattern = "time")) {

        fun_method = "min"

      } else {

        fun_method <- "mean"
      }

      i_agg <- rep(1:(floor(length(x_i) / n) + 1),
                   each = n)[1:length(x_i)]

      x_agg <- try(stats::aggregate(x = x_i,
                                    by = list(i_agg),
                                    FUN = fun_method)[,2])

      if(class(x_agg)[1] == "try-error") {

        x_agg <- rep(NA, length(x_i))
      }

      return(x_agg)
    }, x = x, n = aggregate, cum2rate = cum2rate)

   names(x) <- x_names
  }

  ## optionally smoothen data set
  if(missing(smoothen) == FALSE) {

    x_names <- names(x)

    x_time <- x$time

    x <- lapply(X = x,
                FUN = caTools::runmean,
                k = smoothen)

    names(x) <- x_names

    x$time <- x_time
  }

  ## optionally convert data frame to EGD object
  if(egd == TRUE) {

    x <- list(
      time = x$time,
      meteo = list(
        temperature = list(
          weather = list(
            d00 = data.frame(
              cnr4 = x$temperature_weather_cnr4_00,
              pluvio = x$temperature_weather_pluvio_00,
              hydroclip = x$temperature_weather_hygroclipw_00)),
          north = list(
            d00 = data.frame(
              hydroclip = x$temperature_weather_hygroclipn_00)),
          south = list(
            d00 = data.frame(
              hydroclip = x$temperature_weather_hygroclips_00))),
        precipitation = list(
          weather = list(
            d00 = data.frame(
              pluvio = x$precipitationrt_weather_pluvio_00))),
        humidity = list(
          weather = list(
            d00 = data.frame(
              hydroclip = x$humidity_weather_hygroclip_00)),
          north = list(
            d00 = data.frame(
              hydroclip = x$humidity_weather_hygroclipn_00)),
          south = list(
            d00 = data.frame(
              hydroclip = x$humidity_weather_hygroclips_00))),
        snow = list(
          weather = list(
            d00 = data.frame(
              height = x$snowheight_weather_ush_00)),
          north = list(
            d00 = data.frame(
              height = x$snowheight_north_ush_00)),
          south = list(
            d00 = data.frame(
              height = x$snowheight_south_ush_00))),
        radiation = list(
          weather = list(
            d00 = list(
              long_upper = x$radiationlongupper_weather_cnr4_00,
              long_lower = x$radiationlonglower_weather_cnr4_00,
              short_upper = x$radiationshortupper_weather_cnr4_00,
              short_lower = x$radiationshortlower_weather_cnr4_00))),
        wind = list(
          weather = list(
            d00 = list(
              direction = x$winddirection_weather_wxt535_00,
              speed = x$windspeed_weather_wxt535_00)))),
      soil = list(
        temperature = list(
          north = list(
            d10 = data.frame(
              b3 = x$soiltemperature_north_b3_10,
              b5 = x$soiltemperature_north_b5_10,
              f5 = x$soiltemperature_north_f5_10,
              e8 = x$soiltemperature_north_e8_10,
              g3 = x$soiltemperature_north_g3_10),
            d20 = data.frame(
              b6 = x$soiltemperature_north_b6_20,
              c2 = x$soiltemperature_north_c2_20,
              f2 = x$soiltemperature_north_f2_20,
              f7 = x$soiltemperature_north_f7_20,
              g6 = x$soiltemperature_north_g6_20),
            d30 = data.frame(
              b4 = x$soiltemperature_north_b4_30,
              d2 = x$soiltemperature_north_d2_30,
              e2 = x$soiltemperature_north_e2_30,
              e7 = x$soiltemperature_north_e7_30,
              g4 = x$soiltemperature_north_g4_30),
            d50 = data.frame(
              c4 = x$soiltemperature_north_c4_50,
              c7 = x$soiltemperature_north_c7_50,
              d6 = x$soiltemperature_north_d6_50,
              e3 = x$soiltemperature_north_e3_50,
              g5 = x$soiltemperature_north_g5_50),
            d70 = data.frame(
              c5 = x$soiltemperature_north_c5_70,
              d3 = x$soiltemperature_north_d3_70,
              d7 = x$soiltemperature_north_d7_70,
              e6 = x$soiltemperature_north_e6_70,
              f4 = x$soiltemperature_north_f4_70)),
          south = list(
            d10 = data.frame(
              b3 = x$soiltemperature_south_b3_10,
              b5 = x$soiltemperature_south_b5_10,
              e8 = x$soiltemperature_south_e8_10,
              f5 = x$soiltemperature_south_f5_10,
              g3 = x$soiltemperature_south_g3_10),
            d20 = data.frame(
              b6 = x$soiltemperature_south_b6_20,
              c2 = x$soiltemperature_south_c2_20,
              f2 = x$soiltemperature_south_f2_20,
              f7 = x$soiltemperature_south_f7_20,
              g6 = x$soiltemperature_south_g6_20),
            d30 = data.frame(
              b4 = x$soiltemperature_south_b4_30,
              d2 = x$soiltemperature_south_d2_30,
              e2 = x$soiltemperature_south_e2_30,
              e7 = x$soiltemperature_south_e7_30,
              g4 = x$soiltemperature_south_g4_30),
            d50 = data.frame(
              c4 = x$soiltemperature_south_c4_50,
              c7 = x$soiltemperature_south_c7_50,
              d6 = x$soiltemperature_south_d6_50,
              e3 = x$soiltemperature_south_e3_50,
              g5 = x$soiltemperature_south_g5_50),
            d70 = data.frame(
              c5 = x$soiltemperature_south_c5_70,
              d3 = x$soiltemperature_south_d3_70,
              d7 = x$soiltemperature_south_d7_70,
              e6 = x$soiltemperature_south_e6_70,
              f4 = x$soiltemperature_south_f4_70))),
        moisture = list(
          north = list(
            d10 = data.frame(
              a4 = x$soilmoisture_north_a4_10,
              c1 = x$soilmoisture_north_c1_10,
              c8 = x$soilmoisture_north_c8_10,
              h2 = x$soilmoisture_north_h2_10,
              h6 = x$soilmoisture_north_h6_10),
            d20 = data.frame(
              a2 = x$soilmoisture_north_a2_20,
              a6 = x$soilmoisture_north_a6_20,
              f1 = x$soilmoisture_north_f1_20,
              f8 = x$soilmoisture_north_f8_20,
              h3 = x$soilmoisture_north_h3_20),
            d30 = data.frame(
              a1 = x$soilmoisture_north_a1_30,
              a5 = x$soilmoisture_north_a5_30,
              b8 = x$soilmoisture_north_b8_30,
              e1 = x$soilmoisture_north_e1_30,
              h7 = x$soilmoisture_north_h7_30),
            d50 = data.frame(
              a3 = x$soilmoisture_north_a3_50,
              d1 = x$soilmoisture_north_d1_50,
              d8 = x$soilmoisture_north_d8_50,
              h5 = x$soilmoisture_north_h5_50,
              h8 = x$soilmoisture_north_h8_50),
            d70 = data.frame(
              a7 = x$soilmoisture_north_a7_70,
              b1 = x$soilmoisture_north_b1_70,
              g1 = x$soilmoisture_north_g1_70,
              g8 = x$soilmoisture_north_g8_70,
              h4 = x$soilmoisture_north_h4_70)),
          south = list(
            d10 = data.frame(
              a4 = x$soilmoisture_south_a4_10,
              c1 = x$soilmoisture_south_c1_10,
              c8 = x$soilmoisture_south_c8_10,
              h2 = x$soilmoisture_south_h2_10,
              h6 = x$soilmoisture_south_h6_10),
            d20 = data.frame(
              a2 = x$soilmoisture_south_a2_20,
              a6 = x$soilmoisture_south_a6_20,
              f1 = x$soilmoisture_south_f1_20,
              f8 = x$soilmoisture_south_f8_20,
              h3 = x$soilmoisture_south_h3_20),
            d30 = data.frame(
              a1 = x$soilmoisture_south_a1_30,
              a5 = x$soilmoisture_south_a5_30,
              b8 = x$soilmoisture_south_b8_30,
              e1 = x$soilmoisture_south_e1_30,
              h7 = x$soilmoisture_south_h7_30),
            d50 = data.frame(
              a3 = x$soilmoisture_south_a3_50,
              d1 = x$soilmoisture_south_d1_50,
              d8 = x$soilmoisture_south_d8_50,
              h5 = x$soilmoisture_south_h5_50,
              h8 = x$soilmoisture_south_h8_50),
            d70 = data.frame(
              a7 = x$soilmoisture_south_a7_70,
              b1 = x$soilmoisture_south_b1_70,
              g1 = x$soilmoisture_south_g1_70,
              g8 = x$soilmoisture_south_g8_70,
              h4 = x$soilmoisture_south_h4_70))
        ),
        potential = list(
          north = list(
            d10 = data.frame(
              b3 = x$soilpotential_north_b3_10,
              b5 = x$soilpotential_north_b5_10,
              e8 = x$soilpotential_north_e8_10,
              f5 = x$soilpotential_north_f5_10,
              g3 = x$soilpotential_north_g3_10),
            d20 = data.frame(
              b6 = x$soilpotential_north_b6_20,
              c2 = x$soilpotential_north_c2_20,
              f2 = x$soilpotential_north_f2_20,
              f7 = x$soilpotential_north_f7_20,
              g6 = x$soilpotential_north_g6_20),
            d30 = data.frame(
              b4 = x$soilpotential_north_b4_30,
              d2 = x$soilpotential_north_d2_30,
              e2 = x$soilpotential_north_e2_30,
              e7 = x$soilpotential_north_e7_30,
              g4 = x$soilpotential_north_g4_30),
            d50 = data.frame(
              c4 = x$soilpotential_north_c4_50,
              c7 = x$soilpotential_north_c7_50,
              d6 = x$soilpotential_north_d6_50,
              e3 = x$soilpotential_north_e3_50,
              g5 = x$soilpotential_north_g5_50),
            d70 = data.frame(
              c5 = x$soilpotential_north_c5_70,
              d3 = x$soilpotential_north_d3_70,
              d7 = x$soilpotential_north_d7_70,
              e6 = x$soilpotential_north_e6_70,
              f4 = x$soilpotential_north_f4_70)),
          south = list(
            d10 = data.frame(
              b3 = x$soilpotential_south_b3_10,
              b5 = x$soilpotential_south_b5_10,
              e8 = x$soilpotential_south_e8_10,
              f5 = x$soilpotential_south_f5_10,
              g3 = x$soilpotential_south_g3_10),
            d20 = data.frame(
              b6 = x$soilpotential_south_b6_20,
              c2 = x$soilpotential_south_c2_20,
              f2 = x$soilpotential_south_f2_20,
              f7 = x$soilpotential_south_f7_20,
              g6 = x$soilpotential_south_g6_20),
            d30 = data.frame(
              b4 = x$soilpotential_south_b4_30,
              d2 = x$soilpotential_south_d2_30,
              e2 = x$soilpotential_south_e2_30,
              e7 = x$soilpotential_south_e7_30,
              g4 = x$soilpotential_south_g4_30),
            d50 = data.frame(
              c4 = x$soilpotential_south_c4_50,
              c7 = x$soilpotential_south_c7_50,
              d6 = x$soilpotential_south_d6_50,
              e3 = x$soilpotential_south_e3_50,
              g5 = x$soilpotential_south_g5_50),
            d70 = data.frame(
              c5 = x$soilpotential_south_c5_70,
              d3 = x$soilpotential_south_d3_70,
              d7 = x$soilpotential_south_d7_70,
              e6 = x$soilpotential_south_e6_70,
              f4 = x$soilpotential_south_f4_70))
        )),
      tree = list(
        throughfall = list(
          north = list(
            d00 = list(
              a1 = x$throughfall_north_a1_00,
              a2 = x$throughfall_north_a2_00,
              a3 = x$throughfall_north_a3_00,
              a4 = x$throughfall_north_a4_00,
              a5 = x$throughfall_north_a5_00,
              f1 = x$throughfall_north_f1_00,
              f2 = x$throughfall_north_f2_00,
              f3 = x$throughfall_north_f3_00,
              f4 = x$throughfall_north_f4_00,
              f5 = x$throughfall_north_f5_00,
              j1 = x$throughfall_north_j1_00,
              j2 = x$throughfall_north_j2_00,
              j3 = x$throughfall_north_j3_00,
              j4 = x$throughfall_north_j4_00,
              j5 = x$throughfall_north_j5_00)),
          south = list(
            d00 = list(
              a1 = x$throughfall_south_a1_00,
              a2 = x$throughfall_south_a2_00,
              a3 = x$throughfall_south_a3_00,
              a4 = x$throughfall_south_a4_00,
              a5 = x$throughfall_south_a5_00,
              f1 = x$throughfall_south_f1_00,
              f2 = x$throughfall_south_f2_00,
              f3 = x$throughfall_south_f3_00,
              f4 = x$throughfall_south_f4_00,
              f5 = x$throughfall_south_f5_00,
              j1 = x$throughfall_south_j1_00,
              j2 = x$throughfall_south_j2_00,
              j3 = x$throughfall_south_j3_00,
              j4 = x$throughfall_south_j4_00,
              j5 = x$throughfall_south_j5_00))),
        stemflow = list(
          north = list(
            d00 = list(
              g1 = x$stemflow_north_g1_00,
              g2 = x$stemflow_north_g2_00,
              m1 = x$stemflow_north_m1_00,
              m2 = x$stemflow_north_m2_00,
              k1 = x$stemflow_north_k1_00,
              k2 = x$stemflow_north_k2_00)),
          south = list(
            d00 = list(
              g3 = x$stemflow_south_g3_00,
              g4 = x$stemflow_south_g4_00,
              m3 = x$stemflow_south_m3_00,
              m4 = x$stemflow_south_m4_00,
              k3 = x$stemflow_south_k3_00,
              k4 = x$stemflow_south_k4_00)))))

    ## optionally average redundant sensor data
    if(average == TRUE) {

      x$soil$temperature$north$d10 <- apply(X = x$soil$temperature$north$d10,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$north$d20 <- apply(X = x$soil$temperature$north$d20,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$north$d30 <- apply(X = x$soil$temperature$north$d30,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$north$d50 <- apply(X = x$soil$temperature$north$d50,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$north$d70 <- apply(X = x$soil$temperature$north$d70,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)

      x$soil$temperature$south$d10 <- apply(X = x$soil$temperature$south$d10,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$south$d20 <- apply(X = x$soil$temperature$south$d20,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$south$d30 <- apply(X = x$soil$temperature$south$d30,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$south$d50 <- apply(X = x$soil$temperature$south$d50,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$temperature$south$d70 <- apply(X = x$soil$temperature$south$d70,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)

      x$soil$moisture$north$d10 <- apply(X = x$soil$moisture$north$d10,
                                            MARGIN = 1,
                                            FUN = median,
                                            na.rm = TRUE)
      x$soil$moisture$north$d20 <- apply(X = x$soil$moisture$north$d20,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$north$d30 <- apply(X = x$soil$moisture$north$d30,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$north$d50 <- apply(X = x$soil$moisture$north$d50,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$north$d70 <- apply(X = x$soil$moisture$north$d70,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)

      x$soil$moisture$south$d10 <- apply(X = x$soil$moisture$south$d10,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$south$d20 <- apply(X = x$soil$moisture$south$d20,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$south$d30 <- apply(X = x$soil$moisture$south$d30,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$south$d50 <- apply(X = x$soil$moisture$south$d50,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)
      x$soil$moisture$south$d70 <- apply(X = x$soil$moisture$south$d70,
                                         MARGIN = 1,
                                         FUN = median,
                                         na.rm = TRUE)

      x$soil$potential$north$d10 <- apply(X = x$soil$potential$north$d10,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$north$d20 <- apply(X = x$soil$potential$north$d20,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$north$d30 <- apply(X = x$soil$potential$north$d30,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$north$d50 <- apply(X = x$soil$potential$north$d50,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$north$d70 <- apply(X = x$soil$potential$north$d70,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)

      x$soil$potential$south$d10 <- apply(X = x$soil$potential$south$d10,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$south$d20 <- apply(X = x$soil$potential$south$d20,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$south$d30 <- apply(X = x$soil$potential$south$d30,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$south$d50 <- apply(X = x$soil$potential$south$d50,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
      x$soil$potential$south$d70 <- apply(X = x$soil$potential$south$d70,
                                          MARGIN = 1,
                                          FUN = median,
                                          na.rm = TRUE)
    }

    ## set object class
    class(x) <- "egd"
  }

  ## return output
  return(x)
}

