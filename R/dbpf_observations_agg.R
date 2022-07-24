# =============================================================================
#'
#' @title Return statistics about observations in DB
#'
#' @description Provides mean min max and count for a certain period and one
#'              or several locations.
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#'          Details of aggregation: The aggregation is based on a new column
#'          calculated based on the number of seconds elapsed since
#'          1970-01-01 00:00:00 UTC (referred to as 'epoch'). The rounding
#'          [FLOOR(epoch/period/3600) * period * 3600] thereby references all
#'          values of a period to the time when this period begins. For
#'          example, a daily average (period = 24) will include all values
#'          between 2016-11-01 00:00:00 and 2016-11-02 23:59:59 under the
#'          timestamp 2016-11-01 00:00:00.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character string or list of several with the location
#'                      name(s) to be queried for.
#'
#' @param unit_of_measurement Unit of measurments, defaults to "C". Used to
#' identify the observation time - see Details.
#'
#' @param period Period over wich to aggregate time series [h], defaults
#'               to one hour (period = 1).
#'
#' @param time_b Begin time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param time_e End time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param verbose Provide terminal output of the query string? (defaults to FALSE)
#'
#' @param fetch Whether to return results incrementally using dbSendQuery
#' & dbFetch(), otherwise downloads them all at once using dbGetQuery().
#' Defaults to FALSE.
#'
#' @param n if fetch == TRUE, then specifies the number of rows to download in
#' each batch. Ignored if fetch == FALSE.
#'
#' @return Data frame with locations in rows and columns loc_name, height,
#'         max, min, avg, cnt
#'
#' @details The unit_of_measurement variable is used to distinguish between
#' different measurement types.  The default (C) will return observations that
#' measure temperature in degrees celsius.  To obtain observations of relative
#' humidity, use "%RH".
#'
#' When performing large queries (millions of rows), it is recommended to set
#' fetch = TRUE. Otherwise R can run into trouble.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_observations_agg(con, "NGO-RC-163_ST01", period = 24)
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_observations_agg <- function(con, location_name, unit_of_measurement = "C",
                                  period = 1,
                                  time_b = "1950-01-01 00:00:00+00",
                                  time_e = "2050-01-01 00:00:00+00",
                                  verbose = FALSE,
                                  fetch = FALSE, n = 100000) {
    # make averaging period [s]
    period <- 3600 * period

    # make query
    q <- paste0("SELECT locations.name AS loc_name, ",
                "observations.height_min_metres AS height, ",
                "AVG(observations.numeric_value) as agg_avg, ",
                "COUNT(observations.numeric_value) as agg_cnt, ",
                "TO_TIMESTAMP(FLOOR(EXTRACT('epoch' FROM observations.corrected_utc_time) / ",
                period, ") * ", period,
                ") AT TIME ZONE 'UTC' AS time ",
                "FROM observations INNER JOIN ",
                "locations ON observations.location = locations.coordinates ",
                "WHERE observations.corrected_utc_time BETWEEN ",
                "'", time_b, "' AND '", time_e, "' AND ",
                "locations.name = ANY('{", paste(location_name, collapse=", ") ,"}'::text[]) ",
                "AND observations.unit_of_measure = '", unit_of_measurement, "' ",
                "GROUP BY observations.height_min_metres, locations.name, time ",
                "ORDER BY loc_name ASC, height DESC;")

    #query
    if (verbose) {
    	print("=== Query sent:")
    	print(q)
    }
    if (fetch){
      if (!requireNamespace("data.table")){
        stop("Package data.table is required")
      }
      res <- dbSendQuery(con, q)

      if (verbose){
        print('Fetching query...')
        completed <- character()
        nsite <- length(location_name)
      }

      obs_stat <- list(data.table::as.data.table(dbFetch(res, 1)))

      # Fetch records in chunks and append to
      while (!dbHasCompleted(res)){
        rcount <- dbGetRowCount(res)
        chunk <- data.table::as.data.table(dbFetch(res, n = n))
        obs_stat <- c(obs_stat, list(chunk))

        if (verbose){
          completed <- unique(c(completed, unique(chunk$loc_name)))
          pdone <- 100 * (length(completed) / nsite)
          print(sprintf('Copied rows %d to %d. (~%i%% of sites)',
                        rcount, rcount + nrow(chunk) - 1, round(pdone,0)))
          }
      }
      dbClearResult(res) # close result set

      if (verbose){
        print('Glueing everything together...')
        }
      obs_stat <- data.table::rbindlist(obs_stat, use.names = T)
    }else{
      obs_stat <- dbGetQuery(con, q)

      #handle time
      obs_stat$time <- as.POSIXct(obs_stat$time)
    }

    #return result
    return(obs_stat)
}