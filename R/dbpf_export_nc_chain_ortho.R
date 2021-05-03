

# =============================================================================
#'
#' @title Download ground temperature time series to netCDF 
#'
#' @description Downloads one or more time series from the carleton permafrost 
#' database and export as a netCDF file. The netCDF file is designed to 
#' accommodate many stations with the same measurement depths.  It is possible 
#' to group stations with different measurement depths but this is at the cost
#' of wasted space because any non-shared measurement depths will be filled with 
#' null values.
#'
#' @details Requires that the PermafrostDB, reshape2 and ncdf4 packages be 
#' installed. The file structure is based on H.5.1. "Multidimensional array 
#' representations of time series profiles" with  Example H.16 used as a
#' template and a one-dimensional time coordinate variable.
#'
#' @param con Database connection object, as returned by dbpf_con()
#' 
#' @param location_name Character, one or more location names to convert. These should
#' correspond to locations with thermistor strings
#' 
#' @param file_name character, path to output netCDF file (*.nc)
#' 
#' @param freq Character, one of ('daily', 'hourly'). Interval at which data is to
#' be aggregated from the database.
#' 
#' @export
#' @examples
#' library(ncdf4)
#' library(permafrostDB)
#' con <- dbpf_con() 
#' dbpf_export_nc_chain_ortho(con = con, location_name = c('NGO-DD-1012'), 
#' file_name = "./test_therm.nc")
#' dbpf_export_nc_chain_ortho(con = con, 
#' location_name = c('NGO-RC-170', 'NGO-RC-163'), file_name = "./test_therm.nc")
#' dbpf_export_nc_chain_ortho(con = con, 
#' location_name = c('NGO-RC-169', 'NGO-RC-171', 'NGO-RC-172'),
#'  file_name = "./test_therm.nc")
#' dbDisconnect(con)
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_export_nc_chain_ortho <- function(con, location_name, file_name, freq='daily'){ 
  
  period <- switch(tolower(freq), 'daily'=24, 'hourly'=1)
  
  ## Get data from DB
  db_dat <- dbpf_observations_agg(con = con, 
                                  location_name = location_name,
                                  period = period)
    
  #reshape and get values
  db_dat$height = abs(db_dat$height) 
  db_dat <- db_dat[,c("loc_name", "height", "agg_avg", "time")]
  
  m <- acast(db_dat,
           formula = height ~ time ~ loc_name,
           value.var = 'agg_avg',
           fun.aggregate = function(x) x[1])
  
  refdate <- as.POSIXct("1970-01-01 00:00:00", fmt="%Y-%m-%D %T", tz='UTC')
  
  vals_time <- as.POSIXct(dimnames(m)[[2]], fmt="%Y-%m-%D %T", tz='UTC')
  if (tolower(freq) == 'daily'){
    vals_time <- vals_time - refdate
    time_units <- "days since 1970-01-01 00:00:00"
  }else if(tolower(freq) == 'hourly'){
    vals_time <- as.numeric(vals_time) - as.numeric(refdate)
    time_units <- "seconds since 1970-01-01 00:00:00"
    print(class(head(vals_time)))
  }
  
  vals_tmp <- m
  vals_name <- dimnames(m)[[3]]
  vals_depths <- -as.numeric(dimnames(m)[[1]])
  
  n_ts <- dim(m)[2]
  n_depths <- dim(m)[1]
  n_stations <- dim(m)[3]
  
  ## Get coordinate data
  loc <- dbpf_locations() 
  coords <- loc[match(location_name, loc$name), 
                c('name','lon', 'lat', 'elevation_in_metres')]
  
  ## Create .nc file
  nc <- createOrthogonalThermistorNCF(file = file_name,
                                      n_timestep = n_ts,
                                      n_stations = n_stations,
                                      n_depth = n_depths, 
                                      close_file=F)
  
  ## Populate data
  ncvar_put(nc, varid='depth', vals=vals_depths)
  ncvar_put(nc, varid='time', vals = vals_time)
  ncvar_put(nc, varid="soil_temperature", vals = vals_tmp) #unique(db_dat$agg_avg))
  ncvar_put(nc, varid="platform_id", vals = vals_name)
  ncvar_put(nc, varid='latitude', vals = coords$lat)
  ncvar_put(nc, varid='longitude', vals = coords$lon)
  ncvar_put(nc, varid='elevation', vals = coords$elevation_in_metres)
  
  ## Close file
  nc_close(nc)
}

# =============================================================================
#'
#' @title Create empty netCDF file thermistor stations with identical measurement 
#' depths
#'
#' @description Creates a netCDF file with specified dimensions. The file has no
#' data within it, but must be added in afterwards. 
#'
#' @details The file structure is based on H.5.1. "Multidimensional array of
#'  time series profiles representation of time series" 
#' (http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#_multidimensional_array_representations_of_time_series_profiles)
#' 
#' @param file Character, path to output netCDF file (*.nc)
#' 
#' @param n_timestep Integer, the number of unique timesteps for which there is 
#' temperature data
#' 
#' @param n_depths Integer, the largest number of depth measurements in any
#'  profile
#' 
#' @param n_stations  Integer, how many sites are to be added to the file
#' 
#' @param close_file Logical, whether or not to close the connection to the file
#'  after creation. Leaving the file open allows for the immediate addition of 
#'  data. Defaults to FALSE. 
#' 
#' @export
#' @examples
#' library(ncdf4) 
#' 
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
createOrthogonalThermistorNCF <- function(file, n_stations, n_timestep,
                                          n_depths, close_file=F, 
                                          time_units="days since 1970-01-01 00:00:00"){
  missval = -999
  ## Create Dimensions
  dummyDimTime <- c(1:n_timestep)
  dimTime <- ncdim_def("time", 
                       units=time_units, unlim = F, 
                       vals = dummyDimTime,
                       create_dimvar = T)
  
  dummyDimStation <- c(1:n_stations)
  dimStation <- ncdim_def("station", 
                          unlim = F, vals = dummyDimStation, units = '',
                          create_dimvar = F)
  
  dummyDimDepth <- c(1:n_depths)*0.1 # so that the coordinate var is float not int
  dimDepth <- ncdim_def('depth', 
                        unlim = F, vals = dummyDimDepth, units = '',
                        create_dimvar = T)
  
  dummyDimNameStrlen <- c(1:25)
  dimNameStrlen <- ncdim_def("name_strlen",
                             unlim = F, vals = dummyDimNameStrlen, units = '',
                             create_dimvar = F)
  
  ## Create Variables
  
  varLon <- ncvar_def(name = 'longitude', prec='float',
                      units = "degrees_E", 
                      dim = list(dimStation),
                      longname = "Longitude") 
  
  varLat <- ncvar_def(name = 'latitude', prec='float',
                      units = "degrees_N", 
                      dim = list(dimStation),
                      longname = "Latitude")  
  
  varElev <- ncvar_def(name= 'elevation', prec='float',
                       units = 'm', 
                       dim=list(dimStation), 
                       longname = "Station surface elevation")
  
  varStnName <- ncvar_def(name = 'platform_id', prec='char', 
                          units = '', 
                          dim = list(dimNameStrlen, dimStation), 
                          longname = "Station Name")
  
  varTemp <- ncvar_def(name = 'soil_temperature', prec='float',
                       units = "degrees_C",
                       dim = list(dimDepth, dimTime, dimStation),
                       longname = "Ground temperature")
  
  
  ## Create File
  ncnew <- nc_create(file, vars = list(varLon, varLat, varElev, varStnName,
                                       varTemp))
  
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



