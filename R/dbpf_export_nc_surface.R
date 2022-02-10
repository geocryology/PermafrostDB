# =============================================================================
#'
#' @title Download surface temperature time series to netCDF
#'
#' @description Downloads one or more time series from the carleton permafrost database
#' and export as a netCDF file.
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
#' @examples
#' \dontrun{
#' library(ncdf4)
#' library(reshape2)
#' con <- dbpf_con()
#' dbpf_export_nc_surface(con, "NGO-DD-2009_ST03", "./DD2009ST03_surf.nc")
#' dbpf_export_nc_surface(con,
#' c("NGO-DD-2009_ST03","NGO-DD-2011_ST02","NGO-DD-2009_ST01"), "./DD2009ST03_surf.nc")
#' dbDisconnect(con)
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_export_nc_surface <- function(con, location_name, file_name, freq='daily'){
  if (! freq %in% c("daily", "hourly")){
    stop("Invalid frequency specified.  Try 'daily' or 'hourly'.")
  }
  refdate <- as.POSIXct("1970-01-01 00:00:00", fmt="%Y-%m-%D %T", tz='UTC')
  period <- switch(freq, 'daily'=24, 'hourly'=1)

  ## Get data from DB
  db_dat <- dbpf_observations_agg(con = con,
                                  location_name = location_name,
                                  period = period)

  ## Get coordinate data
  loc <- dbpf_locations(con)
  coords <- loc[match(location_name, loc$name),
                c('name','lon', 'lat', 'elevation_in_metres')]

  ## Create temperature array
  db_dat$height = abs(db_dat$height) #careful!
  db_dat <- db_dat[,c("loc_name", "agg_avg", "height", "time")]

  m <- reshape2::acast(db_dat,
             formula = time ~ loc_name,
             value.var = 'agg_avg',
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
    print(class(head(vals_time)))
  }

  vals_tmp <- m
  vals_height <- m_height
  vals_name <- dimnames(m)[[2]]

  ## Create .nc file
  n_ts <- dim(m)[1] # assign dimensions based on timeseries array
  n_locations <- dim(m)[2]
  nc <- createSurfaceLoggerNCF(file = file_name, n_timestep = n_ts,
                               n_stations = n_locations, close_file=F,
                               time_units=time_units)

  ## Populate data
  ncvar_put(nc, varid='time', vals = vals_time)
  ncvar_put(nc, varid="soil_temperature", vals = vals_tmp)
  ncvar_put(nc, varid="platform_id", vals = vals_name)
  ncvar_put(nc, varid='latitude', vals = coords$lat)
  ncvar_put(nc, varid='longitude', vals = coords$lon)
  ncvar_put(nc, varid='elevation', vals = coords$elevation_in_metres)
  ncvar_put(nc, varid='depth', vals = vals_height)

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
#' The container has 2 dimensions (time and station) and 4 variables (soil_temperature, latitude, longitude, platform_id).
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
#' @export
#' @examples
#' \dontrun{
#' library(ncdf4)
#' # create temperature data
#' t1 <- 3*sin(seq(1:792)*2*pi/365)+2
#' t2 <- sin(seq(1:792)*2*pi/365)
#' m <- matrix(c(t1,t2), nrow=2, byrow=TRUE)
#'
#' #create ncdf file
#' ncnew <- createSurfaceLoggerNCF("./DD2009ST03_surf.nc", 792, 2,  FALSE)
#'
#' # put some data in the ncdf file
#' ncvar_put(ncnew, 'soil_temperature', m)
#' ncvar_put(ncnew, 'platform_id', c('station1', 'station2'))
#'
#' #close the file
#' nc_close(ncnew)
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
createSurfaceLoggerNCF <- function(file, n_timestep, n_stations, close_file=F,
                                   time_units="days since 1970-01-01 00:00:00"){

  missval = -999

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

  varTemp <- ncvar_def(name = 'soil_temperature', prec='float',
                       units = "degrees_C", missval = missval,
                       dim = list(dimTime, dimStation), #XYZT converted to TZYX
                       longname = "Near-surface ground temperature")

  varLat <- ncvar_def(name = 'latitude', prec='float',
                      units = "degrees_N",  missval = missval,
                      dim = list(dimStation),
                      longname = "Latitude")

  varElev <- ncvar_def(name= 'elevation', prec='float',
                       units = 'm',  missval = missval,
                       dim=list(dimStation),
                       longname = "Station surface elevation")

  varDepth <- ncvar_def(name = 'depth', prec='float',
                        units = "m",  missval = missval,
                        dim = list(dimTime, dimStation),
                        longname = "Sensor burial depth")

  varLon <- ncvar_def(name = 'longitude', prec='float',
                      units = "degrees_E",  missval = missval,
                      dim = list(dimStation),
                      longname = "Longitude")

  varStnName <- ncvar_def(name = 'platform_id', prec='char',
                          units = '',
                          dim = list(dimNameStrlen, dimStation), # strlen must be first
                          longname = "Station Name")

  ## Create File
  ncnew <- nc_create(file, vars = list(varTemp, varLat, varLon, varElev,
                                       varDepth, varStnName))

  ## Create Attributes from templates in '/PermafrostDB/extdata' or
  ##                                    '/PermafrostDB/inst/extdata' (unbuilt)
  f = 'extdata'
  p = 'PermafrostDB'
  nc_attributes_from_template(ncnew, system.file(f, 'depth.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'latitude.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'longitude.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'platform_id.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'soil_temperature.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'time.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, '_global_CU_database.csv', package=p))


  ## Close File or Return
  if (close_file){
    nc_close(ncnew)
    return(file)
  }else{
    return(ncnew)
  }
}
