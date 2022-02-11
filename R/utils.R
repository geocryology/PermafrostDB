#===============================================================================
#' @title Interpolate sensor offsets
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location a character vector with one or more site names
#'
#' @param t_bnds a vector of POSIXct times for which estimates of vertical
#' sensor offset are desired.
#'
#' @param interpol one of ('constant', 'linear'). Passed to the interpolation
#' function. 'Constant' interpolation results in step-changes whereas 'linear'
#' yields linear interpolation (default).
#'
#' @param fill.array logical, whether or not locations that lack stick-up
#' height measurements should be added to the array with NA values for all time.
#' Defaults to true.
#'
#' 
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' loc <- c("NGO-DD-1005","NGO-DD-1006")
#' t_s <- as.POSIXct('2015-07-24 12:30:00', format='%Y-%m-%d %H:%M:%S')
#' t_e <- as.POSIXct('2017-09-24 12:30:00', format='%Y-%m-%d %H:%M:%S')
#' t_bn <- seq(t_s, t_e, by=3600*24)
#' offsets <- interpolate_sensor_offset(con, loc, t_bn)
#' }
#' @return a data.frame whose first column is equal to the time values specified
#' by t_bnds. All other colummns are the time-dependent sensor offsets for each
#' of the station locations specified by the location parameter
#===============================================================================
interpolate_sensor_offset <- function(con, location, t_bnds,
                          interpol='linear', fill.array=TRUE){

  stickup <- dbpf_manual_obs_by_location(con=con,
                                         location = location,
                                         sensor_label = 'stick-up_height')

  # Interpolate values
  interpol <- by(stickup, stickup$name,
                 interpolate_single, t_bnds=t_bnds, method=interpol)

  #if NA columns are desired for missing values
  if (fill.array){
    missing_locs <- location[!location %in% stickup$name]
    missing_stickup <- as.data.frame(matrix(NA,
                                            nrow = length(t_bnds),
                                            ncol=length(missing_locs)))

    names(missing_stickup) <- missing_locs
    interpol <- c(interpol, as.list(missing_stickup))
  }

  # reformat as a dataframe
  interpol <- as.data.frame(do.call(cbind, interpol))
  interpol <- cbind(t_bnds, interpol)
  names(interpol)[1] <- 'corrected_utc_time'

  return(interpol)
}

#===============================================================================
#' @title Interpolate sensor offsets for a single site
#'
#' @param x a dataframe containing, at a minimum, the columns
#' 'corrected_utc_time' and 'numeric_value'. These are the points
#'  used to perform the interpolation
#'
#' @param t_bnds a POSIXct vector containing values for every timestep for which
#' an estimate of the sensor offset is desired.
#'
#' @param change_only logical, whether or not to return the raw stick-up
#' height values, or to return the net change from the first reported
#' stick-up height
#'
#' @param method one of ('linear', 'constant') the interpolation method
#' to be passed to approx()
#'
#' @return a numeric vector of the same length as t_bnds which gives an
#' interpolated sensor offset for each timestep.
#'
#' @description interpolate between stick-up heights for a single sensor
#===============================================================================
interpolate_single <- function(x, t_bnds, change_only=T, method='linear'){
  times <- x$corrected_utc_time
  values <- x$numeric_value

  # Interpolate between stick-up height measurements
  if (sum(!is.na(times)) >= 2 & sum(!is.na(values)) >= 2){
    out <- approx(times, values, xout = t_bnds, method=method, rule=1:2)$y

  }else if (sum(!is.na(times)) == 1 & sum(!is.na(values)) == 1){
    out <- rep(NA, length(t_bnds))
    start_time <- times[!is.na(times)]
    start_value <- values[!is.na(values)]
    out[t_bnds > start_time] <- start_value

    }else{
    out <- rep(NA, length(t_bnds))
    change_only <- F # can't take difference if all NA
  }

 if (change_only){
   # subtract first non-NA value to get change in position
   out <- out - out[which.max(!is.na(out))]
 }

 return(out)
}


#' @title New import record
#' @description Create new import record and get id
#' @param con Database connection object
#' @return id of newly-created import record
#'
new_import_record <- function(con){
  #make new import record
  query <- paste0("INSERT INTO imports (import_time, import_parameters) ",
                  "VALUES ('",format(Sys.time(), "%Y-%m-%d %H:%M:%S%z"),
                  "','R-import: dbpf_manual_obs_add()') RETURNING id")
  iid <- dbGetQuery(con, query)

  return(iid)
}