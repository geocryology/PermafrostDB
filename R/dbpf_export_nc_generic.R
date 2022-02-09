# =============================================================================
#'
#' @title Export continuous data to netcdf
#'
#' @description Exports temperature and relative humidity data from the database
#' into a netcdf file compliant with CF conventions v1.6
#' 
#' @param location_name integer, the number of unique timesteps for which there is
#' temperature data
#' 
#' @param file_name character, path to output netCDF file (*.nc)
#' 
#' @param freq The frequency at which to aggregate data from the database
#' one of ('daily', 'hourly')
#' 
#' @param verbose whether or not to report on the progress of the download
#' and file creation.  Defaults to TRUE. 
#' 
#' @details Rows are returned from the database using a combination of 
#' dbSendQuery() and dbFetch().
#' 
#' @examples 
#' \dontrun{
#' x <- dbpf_export_nc_generic(con, c("NGO-DD-1005", "NGO-DD-1006", "NGO-DD-1007", 
#'                                   "NGO-DD-1005_ST01", "AIRT1TOP"),
#'                             "~/example_ncdf.nc")
#'  }
#' @export
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_export_nc_generic <- function(con, location_name, file_name, freq='daily', 
                                   verbose = T){ 
  
  ## Get data from DB
  period <- switch(tolower(freq), 'daily' = 24, 'hourly' = 1)
  
  # Temperature
  if (verbose){print('downloading temperature')}
  db_dat_T <- dbpf_observations_agg(con = con, 
                                  location_name = location_name,
                                  period = period, fetch = T, verbose = verbose)
  db_dat_T <- cbind(db_dat_T, data.frame(measurement = character(nrow(db_dat_T))))
  
  # Humidity
  if (verbose){print('downloading humidity')}
  db_dat_RH <- dbpf_observations_agg(con = con, 
                                  location_name = location_name,
                                  period = period, unit_of_measurement = '%RH',
                                  fetch = T, verbose = verbose)
  db_dat_RH <- cbind(db_dat_RH, data.frame(measurement = character(nrow(db_dat_RH))))
  
  # we have to keep dummy data so that arrays are the right shape (for indexing)
  if (nrow(db_dat_RH) != 0){db_dat_RH$measurement <- "RH"} 
  if (nrow(db_dat_T)  != 0){db_dat_T$measurement <- "temp"}
  db_dat <- rbind(db_dat_T, db_dat_RH)
  db_dat <- db_dat[order(db_dat$loc_name),]

  #reshape and get values
  if (verbose){print("reshaping data")}
  db_dat$height = -(db_dat$height) 
  db_dat <- db_dat[,c("loc_name", "height", "agg_avg", "time", "measurement")]
  
  # get depth indices
  depths <- by(db_dat$height, db_dat$loc_name, unique, simplify=T)
  indx <- sapply(depths, length) # how many z levels for each station
  depths <- as.data.frame(do.call(rbind,lapply(depths, `length<-`, max(indx))))
  vals_depths <- -as.matrix(t(depths))
  
  #convert depth value to level number
  Z <- by(db_dat$height, db_dat$loc_name, function(x) as.numeric(as.factor(x))) #
  db_dat$height <- as.numeric(unlist(Z))

  #reshape data into an n-d array (level, time, station, measurement_type)
  m <- reshape2::acast(db_dat,
             formula = height ~ time ~ loc_name ~ measurement,
             value.var = 'agg_avg',
             fun.aggregate = function(x) x[1])
  
  refdate <- as.POSIXct("1970-01-01 00:00:00", fmt="%Y-%m-%D %T", tz='UTC')
  
  vals_time_true <- as.POSIXct(dimnames(m)[[2]], fmt="%Y-%m-%D %T", tz='UTC')
  if (tolower(freq) == 'daily'){
    vals_time <- vals_time_true - refdate
    time_units <- "days since 1970-01-01 00:00:00"
  }else if(tolower(freq) == 'hourly'){
    vals_time <- as.numeric(vals_time_true) - as.numeric(refdate)
    time_units <- "seconds since 1970-01-01 00:00:00"
  }
  
  # get generic info about dataset
  vals_name <- dimnames(m)[[3]]
  n_depths <- dim(m)[1]
  n_ts <- dim(m)[2]
  n_stations <- dim(m)[3]
 
  # Split data into different variables (air temp, ground temp etc.)
  tmp_i <- which(dimnames(m)[[4]]=='temp')
  rh_i <- which(dimnames(m)[[4]]=='RH')
  vals_g_tmp <- m[,,,tmp_i]
  vals_a_tmp <- m[,,,tmp_i]
  vals_a_rh <- m[,,,rh_i]
  
  #split temperatures into air and ground files based on depths
  if (all(dim(depths)==1)){ 
  # a bit of a workaround here if there is only 1 sensor
    surface_cutoff <- array(-as.numeric(depths), dim=c(n_depths, n_ts, n_stations))
  }else{
    surface_cutoff <- replicate(n_ts, -as.matrix(depths), simplify='array') 
    surface_cutoff <- aperm(surface_cutoff, c(2, 3, 1))  # rotate array
  }
  vals_g_tmp[surface_cutoff > 0] <- NA
  vals_a_tmp[surface_cutoff <= 0] <- NA
 
  ## Get coordinate data
  loc <- dbpf_locations(con) 
  coords <- loc[match(vals_name, loc$name), 
                c('name','lon', 'lat', 'elevation_in_metres')]
  ## 
  if (length(vals_name) != length(location_name)){
    nodata = location_name[! location_name %in% vals_name]
    warning(sprintf("The following locations do not have data and are not
                    included in the output: %s", 
                    paste(nodata, collapse = ',')))
  }
  ## create NA arrays for any missing data
  if (0 %in% dim(vals_g_tmp)){vals_g_tmp <- array(NA, dim=c(n_depths, n_ts, n_stations))}
  if (0 %in% dim(vals_a_tmp)){vals_a_tmp <- array(NA, dim=c(n_depths, n_ts, n_stations))}
  if (0 %in% dim(vals_a_rh)){vals_a_rh <- array(NA, dim=c(n_depths, n_ts, n_stations))}
  
  ## Get offset data from stick-up heights
  vals_off <- interpolate_sensor_offset(location = vals_name,
                                        t_bnds   = vals_time_true, 
                                        dt       = freq,
                                        interpol = 'constant')

  vals_off <- vals_off[, c('corrected_utc_time', vals_name)] #reorder cols to match
  vals_off <- as.matrix(vals_off[, -1]) # get rid of date column and convert to matrx

  ## Create .nc file
  if (verbose){print("creating netcdf file")}
  nc <- createGenericNCDF(file = file_name,
                                 n_timestep = n_ts,
                                 n_stations = n_stations,
                                 n_levels   = n_depths, 
                                 close_file = F,
                                 time_units = time_units)

  ## Populate data
  ncdf4::ncvar_put(nc, varid='height',            vals = vals_depths)
  ncdf4::ncvar_put(nc, varid='time',              vals = vals_time)
  ncdf4::ncvar_put(nc, varid="soil_temperature",  vals = vals_g_tmp) 
  ncdf4::ncvar_put(nc, varid="air_temperature",   vals = vals_a_tmp)
  ncdf4::ncvar_put(nc, varid="relative_humidity", vals = vals_a_rh)
  ncdf4::ncvar_put(nc, varid="station_name",      vals = vals_name)
  ncdf4::ncvar_put(nc, varid='latitude',          vals = coords$lat)
  ncdf4::ncvar_put(nc, varid='longitude',         vals = coords$lon)
  ncdf4::ncvar_put(nc, varid='elevation',         vals = coords$elevation_in_metres)
  ncdf4::ncvar_put(nc, varid='height_offset',     vals = vals_off) 
  
  ## Close file
  ncdf4::nc_close(nc)
}

# =============================================================================
#'
#' @title Create empty netCDF file for one or more thermistor chains 
#'
#' @description Creates a netCDF file with specified dimensions. The file has no
#' data within it, but must be added in afterwards. Thermistors should may have
#' different measurement depths but should have the same number of measurement  
#' depths (levels)
#'
#' @details The file structure is based on H.5.1. "Multidimensional array of 
#' time series profiles representation of time series" 
#' (http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#_multidimensional_array_representations_of_time_series_profiles)
#' * Depth values are not time-dependent, therefore the depth levels of a 
#' station must not change between time periods
#' 
#' @param file character, path to output netCDF file (*.nc)
#' 
#' @param n_timestep integer, the number of unique timesteps for which there is
#'  temperature data
#' 
#' @param n_depths integer, the largest number of depth measurements in any 
#' profile
#' 
#' @param n_stations  integer, how many sites are to be added to the file
#' 
#' @param close_file logical, whether or not to close the connection to the file 
#' after creation. Leaving the file open allows for the immediate addition of 
#' data. Defaults to FALSE.
#' 
#' @export
#' @examples
#'  \dontrun{
#' library(ncdf4) 
#' # create temperature data
#' t1 <- 3*sin(seq(1:792)*2*pi/365)+2
#' t2 <- sin(seq(1:792)*2*pi/365)
#' m <- matrix(c(t1,t2), nrow=2, byrow=TRUE)
#' 
#' #create ncdf file 
#' ncnew <- createMultiThermistorNCF("./thermistor_multi.nc", 2, 792, 1,  FALSE)
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
createGenericNCDF <- function(file, n_stations, n_timestep, n_levels,
                                     close_file=F, 
                                     time_units="days since 1970-01-01 00:00:00"
){
  
  ## Create Dimensions
  dummyDimTime <- as.integer(c(1:n_timestep))
  dimTime <- ncdf4::ncdim_def("time", 
                       units=time_units, unlim = F, vals = dummyDimTime,
                       create_dimvar = T)
  
  dummyDimStation <- as.integer(c(1:n_stations))
  dimStation <- ncdf4::ncdim_def("station", 
                          unlim = F, vals = dummyDimStation, units = '',
                          create_dimvar = T)
  
  dummyDimZ <- as.integer(c(1:n_levels))
  dimZ <- ncdf4::ncdim_def('z', 
                    unlim = F, vals = dummyDimZ, units = '',
                    create_dimvar = F)
  
  dimNameStrlen <- ncdf4::ncdim_def("name_strlen",
                             unlim = F, vals = 1:25, units = '',
                             create_dimvar = F)
  
  ## Create Variables
  
  varLon <- ncdf4::ncvar_def(name = 'longitude', prec='float',
                      units = "degrees_E", 
                      dim = list(dimStation),
                      longname = "station longitude (WGS84)") 
  
  varLat <- ncdf4::ncvar_def(name = 'latitude', prec='float',
                      units = "degrees_N", 
                      dim = list(dimStation),
                      longname = "station latitude (WGS84)")  
  
  varElev <- ncdf4::ncvar_def(name= 'elevation', prec='float',
                       units = 'm', missval = -999,
                       dim=list(dimStation), 
                       longname = "station surface elevation")
  
  varStnName <- ncdf4::ncvar_def(name = 'station_name', prec='char', 
                          units = '', 
                          dim = list(dimNameStrlen, dimStation), # strlen must be first (opposite of NCDF convention)
                          longname = "station name")
  
  varHeight <- ncdf4::ncvar_def(name = 'height', prec='float',
                        units = "m", 
                        dim = list(dimZ, dimStation),  
                        longname = "idealized sensor height relative to ground surface")  
  
  varSoilTemp <- ncdf4::ncvar_def(name = 'soil_temperature', prec='float',
                       units = "degrees_C", missval = -999,
                       dim = list(dimZ, dimTime, dimStation),
                       longname = "ground temperature")
  
  varHumid <- ncdf4::ncvar_def(name = 'relative_humidity', prec='float',
                        units = "percent", missval = -999,
                        dim = list(dimZ, dimTime, dimStation), #XYZTS converted to STZYX on writing
                        longname = "relative humidity of the air")
  
  varAirTemp <- ncdf4::ncvar_def(name = 'air_temperature', prec='float',
                       units = "degrees_C", missval = -999,
                       dim = list(dimZ, dimTime, dimStation), #XYZTS converted to STZYX on writing
                       longname = "air temperature")
  
  varOffset <- ncdf4::ncvar_def(name = 'height_offset', prec='float', 
                         units = 'm', missval=-999,
                         dim = list(dimTime, dimStation),
                         longname = 'estimated vertical offset of sensors from idealized height')
  
  varcrs <- ncdf4::ncvar_def(name = 'crs', prec='integer',
                      units = '', longname = "coordinate system",
                      dim = list())
  ## Create File
  ncnew <- ncdf4::nc_create(file, vars = list(varLon, varLat, varElev, varStnName, 
                                       varHeight, varAirTemp, varSoilTemp, 
                                       varHumid, varcrs, varOffset))
  
  ## Create Attributes
  f = 'extdata'
  p = 'PermafrostDB'
  nc_attributes_from_template(ncnew, system.file(f, 'height.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'elevation.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'latitude.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'longitude.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'station_name.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'soil_temperature.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'air_temperature.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'relative_humidity.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'time.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'station.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, 'crs.csv', package=p))
  nc_attributes_from_template(ncnew, system.file(f, '_global_CU_database.csv', package=p))
  
  
  ## Close File or Return
  if (close_file){
    ncdf4::nc_close(ncnew)
    return(file)
  }else{
    return(ncnew)
  }
}
