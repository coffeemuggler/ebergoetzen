#' Pick rain events from a time series
#'
#' The function screens a precipitation time series for rain events and
#' applies a series of rejection criteria to reduce the number of unwanted
#' events.
#'
#' @param data Either a two-column \code{data.frame} with the time stamps
#' as \code{POSIXct} objects in the first and the \code{numeric} rain values
#' in the second column, or an EberGoetzenData (\code{egd}) object, i.e. a
#' nested list with data from the Ebergoetzen measurement site. See function
#' \code{ebergoetzen::read_data} for details.
#'
#' @param intensity_min \code{Numeric} value, minimum rain intensity (amount
#' per time step) threshold to define a rain event.
#'
#' @param duration_min \code{Numeric} value, minimum duration of consecutive
#' rainy conditions, in seconds. If omitted, the criterion is not applied.
#'
#' @param amount_min \code{Numeric} value, minimum cumulative rain amount. If
#' omitted, the criterion is not applied.
#'
#' @param pause_min \code{Numeric} value, minimum time span between two
#' consecutive rain events. The respective second event will be removed if
#' it occurs within the specified minimum pause. If omitted, the criterion
#' will not be applied.
#'
#' @param lagtime \code{Numeric} value, lag time after the first occurrence
#' of zero rain after which subsequent rain is allowed to extend the duration
#' of the rain event. If omitted, the criterion will not be applied.
#'
#' @return A \code{data.frame} with identified rain events. Each event is
#' described by its start time, stop time, duration (in seconds), rain sum,
#' maximum and mean intensity.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' ## define example data path
#' dir_data <- paste0(system.file("extdata", package="ebergoetzen"), "/")
#'
#' ## read one day of EG data
#' x <- read_data(start = "2023-01-01",
#'                duration = 24 * 3600,
#'                dir = dir_data)
#'
#' e <- get_rainevents(data = x,
#'                     lagtime = 1 * 3600,
#'                     pause_min = 2 * 3600,
#'                     intensity_min = 0.05,
#'                     duration_min = 1800,
#'                     amount_min = 0.1)
#'
#' plot(x$time, x$meteo$precipitation$weather$d00$pluvio, type = "l")
#' abline(v = e$start, col = 3)
#' abline(v = e$stop, col = 2)
#' print(e)
#'
#' @export get_rainevents

get_rainevents <- function (

  data,
  intensity_min,
  duration_min = 0,
  amount_min = 0,
  pause_min = 0,
  lagtime = 0
) {

  if(class(data)[1] == "egd") {

    data <- data.frame(time = data$time,
                       rain = data$meteo$precipitation$weather$d00$pluvio)
  } else if(class(data)[1] != "data.frame") {

    stop("Data must be egd object or data frame!")
  } else if(class(data)[1] == "data.frame") {

    if(ncol(data) != 2) {

      stop("Data must be a two column data frame!")
    }

    if(class(data[,1])[1] != "POSIXct") {

      stop("Time stamps must be in POSIXct format!")
    }

    if(class(data[,2])[1] != "numeric") {

      stop("Rain values must be numeric!")
    }

    names(data) <- c("time", "rain")
  }

  ## check presence of minimum intensity value
  if(missing(intensity_min)) {

    stop("No minimum intensity provided!")
  }

  ## find times with rain above minimum intensity
  t_rain <- data$time[data$rain >= intensity_min]

  ## remove times below minimum pause duration
  t_rain <- t_rain[c(pause_min + 1, diff(as.numeric(t_rain))) >= pause_min]

  ## remove possible NA values
  t_rain <- t_rain[!is.na(t_rain)]

  dt <- median(diff(as.numeric(data$time)))

  ## find correct start and end times
  t_rain <- lapply(X = t_rain, FUN = function(t_pick, t, p, lagtime) {

    ## initiate counter for back-in-time search
    i <- 0
    t_0 <- which(t == t_pick)

    ## look for zero precipitation time step
    try(while(p[t_0 - i] > 0) {

      i <- i + 1
    }, silent = TRUE)

    if(class(i)[1] != "try-error") {

      ## save onset time
      t_0 <- t[t_0 - i]

      ## initiate counter for forward-in-time search
      i <- 0
      t_1 <- which(t == t_pick)
      i_lag <- ceiling(lagtime / dt)

      ## look for zero precipitation time step
      while(any(p[(t_1 + i):(t_1 + i + i_lag)] > 0, na.rm = TRUE)) {

        i <- i + 1
      }
      t_1 <- t[t_1 + i]

      ## calculate rain duration
      t_dur <- as.numeric(t_1) - as.numeric(t_0)

      ## calculate rain sum
      p_sum <- sum(p[t >= t_0 & t <= t_1], na.rm = TRUE)

      ## calculate maximum intensity
      p_max <- max(p[t >= t_0 & t <= t_1], na.rm = TRUE)

      ## calculate average intensity
      p_mean <- mean(p[t >= t_0 & t <= t_1], na.rm = TRUE)

      ## return corrected stat and end times
      return(data.frame(start = t_0,
                        stop = t_1,
                        duration = t_dur,
                        sum = p_sum,
                        max = p_max,
                        mean = p_mean))

    } else {

      ## return corrected start and end times
      return(data.frame(start = as.POSIXct(NA),
                        stop = as.POSIXct(NA),
                        duration = NA,
                        sum = NA,
                        max = NA,
                        mean = NA))
    }
  }, t = data$time, p = data$rain, lagtime = lagtime)

  ## convert list to data frame and remove duplicates
  t_rain <- do.call(rbind, t_rain)
  t_rain <- t_rain[!is.na(t_rain$start),]
  t_rain <- t_rain[!duplicated(x = t_rain$start),]
  t_rain <- t_rain[!duplicated(x = t_rain$stop),]

  ## remove rain events that are too short
  t_rain <- t_rain[t_rain$duration >= duration_min,]

  ## remove rain events that with too low intensity
  t_rain <- t_rain[t_rain$sum >= amount_min,]

  ## return output
  return(t_rain)
}
