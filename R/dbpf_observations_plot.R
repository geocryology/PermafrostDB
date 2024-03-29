# =============================================================================
#'
#' @title Plot data for one location
#'
#' @description Opens interactive plot in browser in order to check and explore
#'              data series for one location.
#'
#' @details Allows to set aggregation parameters.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character string of the location to be plotted.
#'
#' @param unit_of_measurement Unit of measurments, defaults to "C"
#'
#' @param period Period over wich to aggregate time series [h], defaults
#'               to no aggregation (period = 'raw').
#'
#' @param time_b Begin time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param time_e End time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param verbose Provide terminal output of the query string? (defaults to FALSE)
#'
#' @return Data frame with locations in rows and columns loc_name, height,
#'         max, min, avg, cnt
#'dbpf_observations_plot
#' @export
#' @examples
#' \dontrun{
#' dbpf_observations_plot(con, "YK16-SO06_01")
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_observations_plot <- function(con, location_name, unit_of_measurement = "C",
                                  period = "raw",
                                  time_b = "2018-07-01 00:00:00+00",
                                  time_e = "2021-08-05 23:59:59+00",
                                  verbose = FALSE) {
    height <- NULL  # workaround to prevent 'no visible binding for global variable' 
    if (!requireNamespace("xts", quietly = TRUE)){
    utils::install.packages("xts")
    }

    if (!requireNamespace("dygraphs", quietly = TRUE)){
    utils::install.packages("dygraphs")
    }

    Sys.setenv(TZ='UTC')

    # get data
    if (period == "raw") {
    	# only take one location, not a vector
    	location_name <- location_name[1]
    	data <- dbpf_observations_raw(con, location_name, unit_of_measurement = "C",
                                      height_top = 100, height_bot = -500,
                                      time_b, time_e, verbose)
        #prepare plot object
    	time    <- data.frame(time=unique(data$time))
    	heights <- unique(data$height)
    	series <- NULL
    	snames <- NULL
    	for (d in 1:length(heights)) {
    		# match the values to times with merge to get regular series
    		y <- subset(data, height == heights[d], select=c("value", "time"))
    		m <- merge(x = time, y = y, by = "time", all.x = TRUE)
    		series <- cbind(series, m$value)
    		snames <- c(snames, paste(heights[d], "m"))
    	}
    } else {
    	#TODO: read multiple series, aggregate to common grid, and combine name+depth to label
    	#first series determines time frame
    	time <- data.frame(time=seq(as.POSIXct(time_b), as.POSIXct(time_e), by = 3600 * period))
    	series <- NULL
    	snames <- NULL
    	for (l in 1:length(location_name)) {
    		data <- dbpf_observations_agg(con, location_name[l], unit_of_measurement,
                                          period, time_b, time_e, verbose)
        	names(data) <- c("loc_name", "height", "value", "agg_cnt", "time")

    		#prepare plot object
    		heights <- unique(data$height)
    		for (d in 1:length(heights)) {
    			# match the values to times with merge to get regular series
    			y <- subset(data, height == heights[d], select=c("value", "time"))
    			m <- merge(x = time, y = y, by = "time", all.x = TRUE)
    			#only take columns with values
    			if (sum(is.na(m$value) == 0) > 0) {
    				series <- cbind(series, m$value)
    				snames <- c(snames, paste(location_name[l], heights[d], "m"))
    			}
    		}
    	}
    }

    #make time series
 	qxts <- xts::xts(series, order.by = time$time)

    #get the number of time series
    snamesLen <- length(snames)

 	#plot time series
    graph <- dygraphs::dygraph(qxts, main=paste(location_name, collapse=", "), ylab = "Temperature [\U00B0 C]")

  # iterate to create series labels
  	for (seriesNum in 1:snamesLen)
  	{
  	  graph <- graph %>% dygraphs::dySeries(paste0("V",seriesNum), label = snames[seriesNum])
  	}

  	graph <- graph %>%
  	dygraphs::dyLegend(width = 400) %>%
  	dygraphs::dyOptions(labelsUTC = TRUE) %>%
  	dygraphs::dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))

  	# display graph
  	graph
}

most_recent_time <- function(con, location_name){
  locIdQuery <- paste0("SELECT coordinates FROM locations WHERE name = '", location_name, "'")
  locID <- dbGetQuery(con, locIdQuery)
  obsQuery <- paste0("SELECT corrected_utc_time, location FROM observations ",
                     "WHERE observations.location = '", locID, "' ORDER BY corrected_utc_time ",
                     "DESC LIMIT 1")
  most_recent_obs <- dbGetQuery(con, obsQuery)
  return(most_recent_obs)
}