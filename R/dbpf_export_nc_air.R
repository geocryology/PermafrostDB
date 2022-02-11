# =============================================================================
#'
#' @title Download air temperature and humidity time series to netCDF
#'
#' @description Downloads one or more time series from the carleton permafrost database
#' and export as a netCDF file. Designed to work with Geoprecision LOG-HC2-RC-US
#' loggers
#'
#' @details Requires that the PermafrostDB and ncdf4 packages be installed.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character, one or more location names to convert.
#'
#' @param file_name character, path to output netCDF file (*.nc)
#'
#' @param freq Character, one of ('daily', 'hourly'). Interval at which data is to
#' be aggregated from the database.
#'
#' @export
#' @author Nick Brown <nick.brown@@carleton.ca>
#' @importFrom ncdf4 ncvar_put nc_close ncatt_put nc_close
# ============================================================================
dbpf_export_nc_air <- function(con, location_name, file_name, freq='daily'){
  if (! freq %in% c("daily", "hourly")){
    stop("Invalid frequency specified.  Try 'daily' or 'hourly'.")
  }

  refdate <- as.POSIXct("1970-01-01 00:00:00", fmt="%Y-%m-%D %T", tz='UTC')
  period <- switch(freq, 'daily'=24, 'hourly'=1)

  ## Get data from DB
  db_T <- dbpf_observations_agg(con = con,
                                  location_name = location_name,
                                  period = period, unit_of_measurement = 'C')
  names(db_T)[names(db_T)=="agg_avg"] <- 'temperature'
  db_T <- db_T[-which(names(db_T)=="agg_cnt")]

  db_RH <- dbpf_observations_agg(con = con,
                                  location_name = location_name,
                                  period = period, unit_of_measurement = '%RH')
  names(db_RH)[names(db_RH)=="agg_avg"] <- 'humidity'
  db_RH <- db_RH[-which(names(db_RH)=="agg_cnt")]


  db_dat <- merge(db_T, db_RH, all=T, by = c("loc_name", "height", "time"))

  ## Get coordinate data
  loc <- dbpf_locations(con)
  coords <- loc[match(location_name, loc$name),
                c('name','lon', 'lat', 'elevation_in_metres')]

  ## Create temperature array
  db_dat$height <- abs(db_dat$height)
  db_dat <- db_dat[,c("loc_name", "temperature", "humidity", "height", "time")]

  m <- reshape2::acast(db_dat,
             formula = time ~ loc_name,
             value.var = 'temperature',
             fun.aggregate = function(x) x[1])

  m_humid <- reshape2::acast(db_dat,
             formula = time ~ loc_name,
             value.var = 'humidity',
             fun.aggregate = function(x) x[1])

  m_height <- reshape2::acast(db_dat,
                    formula = time ~ loc_name,
                    value.var = 'height',
                    fun.aggregate = function(x) x[1])

  vals_time <- as.POSIXct(dimnames(m)[[1]], fmt="%Y-%m-%D %T", tz='UTC')
  if (tolower(freq) == 'daily'){
    vals_time <- vals_time - refdate
    time_units <- "days since 1970-01-01 00:00:00"
  }else if(tolower(freq) == 'hourly'){
    vals_time <- as.numeric(vals_time) - as.numeric(refdate)
    time_units <- "seconds since 1970-01-01 00:00:00"
    print(class(utils::head(vals_time)))
  }

  vals_tmp <- m
  vals_humid <- m_humid
  vals_height <- m_height
  vals_name <- dimnames(m)[[2]]

  ## Create .nc file
  n_ts <- dim(m)[1] # assign dimensions based on timeseries array
  n_locations <- dim(m)[2]
  nc <- createAirLoggerNCF(file = file_name, n_timestep = n_ts,
                               n_stations = n_locations, close_file=F,
                               time_units=time_units)

  ## Populate data
  ncvar_put(nc, varid='time', vals = vals_time)
  ncvar_put(nc, varid="air_temperature", vals = vals_tmp)
  ncvar_put(nc, varid="platform_id", vals = vals_name)
  ncvar_put(nc, varid='latitude', vals = coords$lat)
  ncvar_put(nc, varid='longitude', vals = coords$lon)
  ncvar_put(nc, varid='elevation', vals = coords$elevation_in_metres)
  ncvar_put(nc, varid='height', vals = vals_height)
  ncvar_put(nc, varid='relative_humidity', vals = vals_humid)

  ## Close file
  nc_close(nc)
}


# =============================================================================
#'
#' @title Create empty netCDF file for surface temperatures
#'
#' @description Creates a netCDF file with specified dimensions. The file has no
#' data within it, but must be added in afterwards. The netCDF file structure can accommodate
#' one or more surface loggers with different beginning and end dates, but to economise on the
#' file type, the time series should share measurement times.
#'
#' @details The file structure is based on H.2.1. "Orthogonal multidimensional array
#' representation of time series"
#' (http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#_orthogonal_multidimensional_array_representation_of_time_series)
#'
#' The container has 2 dimensions (time and station) and 4 variables (air_temperature, latitude, longitude, platform_id).
#'
#' @param file character, path to output netCDF file (*.nc)
#'
#' @param n_timestep integer, the number of unique timesteps for which there is temperature data
#'
#' @param n_stations integer, how many sites are to be added to the file
#'
#' @param close_file logical, whether or not to close the connection to the file after creation.
#' Leaving the file open allows for the immediate addition of data. Defaults to FALSE.
#' 
#' @param time_units netcdf4-style string description of time units 
#' 
#' @export
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
createAirLoggerNCF <- function(file, n_timestep, n_stations, close_file=F,
                                   time_units="days since 1970-01-01 00:00:00"){

  ## Create Dimensions
  dummyDimTime <- c(1:n_timestep)
  dimTime <- ncdim_def("time",
                       units=time_units, unlim = F, vals = dummyDimTime,
                       create_dimvar = T)

  dummyDimStation <- c(1:n_stations)
  dimStation <- ncdim_def("station",
                          unlim = F, vals = dummyDimStation, units = '',
                          create_dimvar = F)

  dimNameStrlen <- ncdim_def("name_strlen",
                             unlim = F, vals = 1:25, units = '',
                             create_dimvar = F)

  ## Create Variables

  varTemp <- ncvar_def(name = 'air_temperature', prec='float',
                       units = "degrees_C",
                       dim = list(dimTime, dimStation), #XYZT converted to TZYX
                       longname = "Air temperature")

  varHumid <- ncvar_def(name = 'relative_humidity', prec='float',
                       units = "percent",
                       dim = list(dimTime, dimStation), #XYZT converted to TZYX
                       longname = "Air relative humidity")

  varLat <- ncvar_def(name = 'latitude', prec='float',
                      units = "degrees_N",
                      dim = list(dimStation),
                      longname = "Latitude")

  varElev <- ncvar_def(name= 'elevation', prec='float',
                       units = 'm',
                       dim=list(dimStation),
                       longname = "Station ground surface elevation")

  varHeight <- ncvar_def(name = 'height', prec='float',
                        units = "m",
                        dim = list(dimTime, dimStation),
                        longname = "Sensor height above ground surface")

  varLon <- ncvar_def(name = 'longitude', prec='float',
                      units = "degrees_E",
                      dim = list(dimStation),
                      longname = "Longitude")

  varStnName <- ncvar_def(name = 'platform_id', prec='char',
                          units = '',
                          dim = list(dimNameStrlen, dimStation), # strlen must be first
                          longname = "Station Name")

  ## Create File
  ncnew <- nc_create(file, vars = list(varTemp, varLat, varLon, varElev,
                                       varHeight, varStnName, varHumid))

  ## Create Attributes
  ncatt_put(ncnew, 'platform_id', "cf_role", "timeseries_id", prec='char')
  ncatt_put(ncnew, 'air_temperature', 'coordinates', 'latitude longitude time')
  ncatt_put(ncnew, 'relative_humidity', 'coordinates', 'latitude longitude time')
  ncatt_put(ncnew, 'time', 'calendar', 'standard')


  ## Create Global Attributes
  ncatt_put(ncnew, 0, "featureType", "timeSeries", prec='char')
  ncatt_put(ncnew, 0, "title", "timeSeries", prec='char')
  ncatt_put(ncnew, 0, "institution", "Carleton University", prec='char')
  ncatt_put(ncnew, 0, "source",
            "Carleton University Permafrost Database", prec='char')
  ncatt_put(ncnew, 0, "Conventions", "CF-1.6", prec='char')

  ## Close File or Return
  if (close_file){
    nc_close(ncnew)
    return(file)
  }else{
    return(ncnew)
  }
}
