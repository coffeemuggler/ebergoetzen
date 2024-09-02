#' Select items of a data set
#'
#' The function allows to isolate itmes of interest from a \code{egd} object.
#'
#' @param data \code{egd} object, see \code{ebergoetzen::read_data} for how to
#' create this object
#'
#' @param theme \code{Character} vector, one or more keywords that describe
#' the data theme to be selected. Available themes are \code{"meteo"},
#' \code{"soil"} and \code{"tree"}. If omitted, all three themes will be
#' selected and returned as a list object.
#'
#' @param metric \code{Character} vector, one or more keywords that describe
#' the metric (measured parameter) to be selected. Available themes are
#' \code{"temperature"}, \code{"precipitation"}, \code{"humidity"},
#' \code{"snow"}, \code{"radiation"}, \code{"wind"}, \code{"moisture"},
#' \code{"potential"}, \code{"throughfall"} and \code{"stemflow"}. Note that
#' some keywords apply for different \code{themes} and will be applied to
#' these themes if they are selected.
#'
#' @param site \code{Character} vector, one or more keywords that describe
#' the site to be selected. Available sites are \code{"weather"},
#' \code{"north"} and \code{"south"}. If omitted, all three sites will be
#' selected and returned as a list object, if metrics exist in them.
#'
#' @param depth \code{Character} vector, one or more keywords that describe
#' the depth to be selected. Available dpeths are \code{"d00"} (surface or
#' above ground sensor), \code{"d10"} (10 cm), \code{"d20"} (20 cm),
#' \code{"d30"} (30 cm), \code{"d50"} (50 cm) and \code{"d70"} (70 cm).
#' Note that for convenience, also just numeric values can be provided as
#' depth values (e.g., \code{c(0, 10, 20)}). If omitted, all depths will be
#' selected and returned as a list object, if metrics exist for them.
#'
#' @param simplify \code{Logical} value, option to simplify output object
#' structure, i.e., all list elements that contain only a single element
#' will be removed. Default is \code{TRUE}.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- select_data(data = x,
#'                  theme = c("soil", "tree"),
#'                  metric = c("temperature", "moisture, "throughfall"),
#'                  site = "north")
#'
#' }
#'
#' @export select_data

select_data <- function(

  data,
  theme,
  metric,
  site,
  depth,
  simplify = TRUE

) {

  ## check input data and parameters ------------------------------------------

  ## check input data for correct object class
  if(class(data)[1] != "egd") {

    stop("Data is not of class egd!")
  }

  ## check theme keyword
  kw_theme <- c("meteo", "soil", "tree")

  if(missing(theme) == FALSE) {

    if(any(theme %in% kw_theme == FALSE)) {

      stop("At least one theme keyword is not supported!")
    }
  } else {

    theme <- kw_theme
  }

  ## check metric keyword
  kw_metric <- c("temperature", "precipitation", "humidity", "snow",
                 "radiation", "wind", "moisture", "potential", "throughfall",
                 "stemflow")

  if(missing(metric) == FALSE) {

    if(any(metric %in% kw_metric == FALSE)) {

      stop("At least one metric keyword is not supported!")
    }
  } else {

    metric <- kw_metric
  }

  ## check site keyword
  kw_site <- c("weather", "north", "south")

  if(missing(site) == FALSE) {

    if(any(site %in% kw_site == FALSE)) {

      stop("At least one site keyword is not supported!")
    }
  } else {

    site <- kw_site
  }

  ## check depth keyword
  kw_depth <- c("d00", "d10", "d20", "d30", "d50", "d70")

  if(missing(depth) == FALSE) {

    ## optionally convert depth keywords
    depth[nchar(depth) == 1] <- paste0("0", depth)
    depth[nchar(depth) == 2] <- paste0("d0", depth)

    if(any(depth %in% kw_depth == FALSE)) {

      stop("At least one depth keyword is not supported!")
    }
  } else {

    depth <- kw_depth
  }

  ## select data by keywords --------------------------------------------------

  ## isolate and remove time element
  time <- data$time
  data$time <- NULL

  ## select by theme
  i_theme <- sapply(X = names(data), FUN = function(id, theme) {

    id %in% theme
  }, theme)
  data <- data[i_theme]

  ## select by metric
  data <- lapply(X = data, FUN = function(data, metric) {

    i_metric <- sapply(X = names(data), FUN = function(id, metric) {

      id %in% metric
    }, metric)

    data <- data[i_metric]

    return(data)

  }, metric)

  ## select by site
  data <- lapply(X = data, FUN = function(data, site) {

    data <- lapply(X = data, FUN = function(data, site) {

      i_site <- sapply(X = names(data), FUN = function(id, site) {

        id %in% site
      }, site)

      data <- data[i_site]

      return(data)

    }, site)

    return(data)

  }, site)

  ## select by depth
  data <- lapply(X = data, FUN = function(data, depth) {

    data <- lapply(X = data, FUN = function(data, depth) {

      data <- lapply(X = data, FUN = function(data, depth) {

        i_depth <- sapply(X = names(data), FUN = function(id, depth) {

          id %in% depth
        }, depth)

        data <- data[i_depth]

        return(data)

      }, depth)

      return(data)

    }, depth)

    return(data)

  }, depth)

  ## optionally simplify data structure
  if(simplify == TRUE) {

    data <- lapply(X = data, FUN = function(data) { # metric

      data <- lapply(X = data, FUN = function(data) { # site

        data <- lapply(X = data, FUN = function(data) { # depth

          if(length(data) == 1) {

            data <- data[[1]]
          }

          return(data)
        })

        if(length(data) == 1) {

          data <- data[[1]]
        }

        return(data)
      })

      if(length(data) == 1) {

        data <- data[[1]]
      }

      return(data)
    })
  }

  ## append time vector
  data$time <- time

  ## return output
  return(data)
}


